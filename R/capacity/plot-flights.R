
year_start <- ymd(year_start)
year_end <- ymd(year_end)

entity_details_mm12 <- enr_all_details %>% filter(entity_name == state_under_review) %>%
  filter(date >= year_start & date <= year_end) %>%
  group_by(yyyy, mm) %>%
  summarise(delay = sum(delay))

entity_summaries_mm12 <- enr_all_summaries %>% filter(entity_name == state_under_review) %>%
  filter(date >= year_start & date <= year_end) %>%
  group_by(yyyy, mm) %>%
  summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), flt_dly_gt15 = sum(flights_delayed_gt15))

entity_mm12 <- inner_join(entity_details_mm12, entity_summaries_mm12, by = c("yyyy", "mm")) %>%
  mutate(date = ymd(str_c(yyyy, mm, "01", sep = "-")))

ert_dly_12m_plot <- ggplot(entity_mm12, aes(x = date, y = flt_tot)) +
  geom_line(stat = "identity", size = 1, color = "#225ea8") +
  geom_point(stat = "identity", size = 3, shape = 18, color = "#225ea8") +
  theme_pru() +
  scale_color_pru() +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd(year_start),ymd(year_end), by = "4 months")) +
  labs(
    x = "",
    y = "Flights",
    title = paste("Flights per month -", state_under_review),
    subtitle = "") +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = ""),
                     limits = c((min(entity_mm12$flt_tot) - 2000), 
                                (max(entity_mm12$flt_tot) + 2000))
  ) + 
  theme(axis.text.x = element_text(vjust = 0.8))
add_logo(plot_name = ert_dly_12m_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/Nbr_flights_", state_under_review, ".png"))
