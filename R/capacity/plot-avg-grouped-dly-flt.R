all_details <- enr_all_details %>%
  filter(!(entity_name == "ANA LUX") & !(entity_name == "SES Area (RP1)")) %>%
  mutate(entity_name = factor(entity_name, 
                              levels = sort(unique(enr_all$entity_name)))) %>%
  mutate(entity_group = case_when(
    entity_name %in% c("Oro Navigacija","PANSA")                                ~ "Baltic FAB",
    entity_name %in% c("DCAC Cyprus","HCAA","ENAV","MATS")                      ~ "Blue Med FAB",
    entity_name %in% c("BULATSA","ROMATSA")                                     ~ "Danube FAB",
    entity_name %in% c("NAVIAIR","LFV")                                         ~ "DK-SE FAB",
    entity_name %in% c("Austro Control","Croatia Control","ANS CR",
                       "HungaroControl (EC)","LPS","Slovenia Control")          ~ "FAB CE",
    entity_name %in% c("Belgocontrol","DSNA","DFS","LVNL","MUAC","Skyguide", 
                       "skeyes")                                                ~ "FABEC",
    entity_name %in% c("EANS","ANS Finland","LGS","Avinor")                     ~ "NEFAB",
    entity_name %in% c("NAV Portugal (Continental)","ENAIRE")                   ~ "SW FAB",
    entity_name %in% c("IAA","NATS (Continental)")                              ~ "UK-Ireland FAB",
    entity_name %in% c("Albcontrol","ARMATS","DHMI","M-NAV","MOLDATSA",
                       "Sakaeronavigatsia","SMATSA","UkSATSE")                  ~ "Other",
    entity_name %in% c("EUROCONTROL Area (MS)")                                 ~ "ECTL"
  )) %>%
  mutate(
    entity_group = factor(entity_group, levels = c("Baltic FAB", "Blue Med FAB", "Danube FAB", 
                                                   "DK-SE FAB", "FAB CE", "FABEC", "NEFAB",
                                                   "SW FAB", "UK-Ireland FAB", "Other", "ECTL")) 
  )

all_details_yyyy <- all_details %>%
  filter(yyyy == year_input) %>%
  group_by(entity_group, entity_name, delay_group) %>%
  summarise(delay = sum(delay)) 

all_summaries_yyyy <- enr_all_summaries %>%
  filter(yyyy == year_input) %>%
  filter(!(entity_name == "ANA LUX") & !(entity_name == "SES Area (RP1)")) %>%
  group_by(entity_name) %>%
  summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), flt_dly_gt15 = sum(flights_delayed_gt15))

all_yyyy <- inner_join(all_details_yyyy, all_summaries_yyyy, by = "entity_name")

# Create reference line ----------------------------------------------------  
ref_details <- all_details %>%
  filter(yyyy == ref_year) %>%
  group_by(entity_group, entity_name) %>%
  summarise(delay = sum(delay)) 

ref_summaries <- enr_all_summaries %>%
  filter(yyyy == ref_year) %>%
  filter(!(entity_name == "ANA LUX") & !(entity_name == "SES Area (RP1)")) %>%
  group_by(entity_name) %>%
  summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), flt_dly_gt15 = sum(flights_delayed_gt15))

ref <- inner_join(ref_details, ref_summaries, by = "entity_name")

ref <- ref %>% 
  mutate(new = delay/flt_tot) %>%
  select(entity_name, entity_group, new)

all_plot <- inner_join(all_yyyy, ref, by = c("entity_name","entity_group"))

# Create plot -------------------------------------------------------------------------  
g <- ggplot(all_plot, aes(x = entity_name, y = delay/flt_tot, fill = delay_group))

ert_dly_yyyy_plot <- g + coord_flip() +
  geom_bar(stat = "identity") + facet_grid(entity_group ~ ., scales = "free_y", space = "free") +
  theme_pru() +
  scale_color_pru() +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_fill_viridis(discrete = TRUE,
                     name = "",
                     breaks = c("W", "Da", "Sa", "Ca", "Other"),
                     labels = c(
                       "Weather [W, D]",
                       "ATC Disruptions [I, T]",
                       "ATC Staffing [S]",
                       "ATC Capacity [C]",
                       "Other [all other codes]")) +
  geom_errorbar(aes(y = new, ymin = new, ymax = new, col = ref_year), linetype = 1, size = 1) + 
  scale_colour_manual(name='', values="#ef3b2c", guide ="legend") +
  guides(col = guide_legend(override.aes = list(linetype=1), title = "")) + 
  labs(
    x = "",
    y = "En-route ATFM delay (min/flight)",
    title = paste("Average en-route ATFM delay per flight -", year_input, "(min)"),
    subtitle = "Split per delay group") +
  theme(legend.position="bottom")
add_logo(plot_name = ert_dly_yyyy_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/Avg_ENR_ATFM_delay_flight_group_", state_under_review, ".png"))
