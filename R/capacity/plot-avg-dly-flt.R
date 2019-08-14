year_start <- ymd(year_start)
  year_end <- ymd(year_end)
  
  all_details_mm12 <- enr_all_details %>% filter(entity_name == state_under_review) %>%
    filter(date >= year_start & date <= year_end) %>%
    group_by(yyyy, mm) %>%
    summarise(delay = sum(delay))
  
  all_summaries_mm12 <- enr_all_summaries %>% filter(entity_name == state_under_review) %>%
    filter(date >= year_start & date <= year_end) %>%
    group_by(yyyy, mm) %>%
    summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), flt_dly_gt15 = sum(flights_delayed_gt15))
  
  
  all_mm12 <- inner_join(all_details_mm12, all_summaries_mm12, by = c("yyyy", "mm")) %>%
    mutate(date = ymd(str_c(yyyy, mm, "01", sep = "-")))
  
  
  g <- ggplot(all_mm12, aes(x = date, y = delay/flt_tot))
  
  avg_dly_12m_plot <- g +
    geom_bar(stat = "identity", fill = "#35978f", alpha = 0.8) +
    theme_pru() +
    scale_color_pru() +
    scale_x_date(
      date_labels = "%b-%Y",
      breaks = seq.Date(ymd(year_start),ymd(year_end), by = "4 months")) +
    labs(
      x = "",
      y = "Average en-route ATFM delay per flight",
      title = paste("Average en-route ATFM delay per flight (min.) -", state_under_review),
      subtitle = "") + 
    theme(axis.text.x = element_text(vjust = 0.8))
  add_logo(plot_name = avg_dly_12m_plot,
           source = "Source: PRU analysis",
           width_pixels = 640,
           height_pixels = 450,
           save_filepath = paste0(dir, "Figures/Avg_ENR_ATFM_delay_flight_", state_under_review, ".png"))
