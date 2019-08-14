# flight efficiency

# compute indicators
all1 <- all %>%
  mutate(additional_flown_km = (dist_flown_km - dist_achieved_km),
         flt_efficiency = ((dist_flown_km / dist_achieved_km) -1)) 

# countries
all_countries <- all1 %>%
  filter(entity_type == "State (FIR)")

all_countries <- all_countries %>%
  filter(type_model == model) %>% 
  filter(entity_name != "Morocco" & entity_name != "Spain (Canarias)" & entity_name != "Spain (Continental)") %>%
  #mutate(entity_name = ifelse(entity_name == "Spain (Canarias)", "Spain", entity_name),
  #       entity_name = ifelse(entity_name == "Spain (Continental)", "Spain", entity_name)) %>%
  filter(yyyy == year_input) %>% 
  group_by(entity_name, yyyy) %>%
  summarise(flt_efficiency = sum(flt_efficiency)) %>%
  mutate(flt_eff = 100 - flt_efficiency)

avg_ectl <- sum(all_countries$flt_eff)/length(all_countries$flt_eff)

g <- ggplot(all_countries, aes(x = reorder(entity_name, - flt_eff), y = flt_eff))

flight_efficiency <- g +
  geom_bar(stat = "identity", fill = "#4393c3", alpha = 0.6) + 
  geom_text(aes(label = paste0(round(flt_eff, 1),"%")),
            position = position_dodge(0.9),
            vjust = 0.4, hjust = -0.1, angle = 90, size = 2.5) +
  geom_hline(yintercept = avg_ectl,  col = "red", lty = 2) +
  geom_text(aes(x = 34, label="Eurocontrol average", y = avg_ectl), 
            col = "red", vjust = -1, size = 3) +
  coord_cartesian(ylim=c(83,100)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%", accuracy = 1)) +
  labs(
    x = "",
    y = "Flight efficiency",
    title = "",
    subtitle = "") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
rm(all1)