apt_arrival_dly <- function(airport, since, to){
  
  all_details_apt <- apt_all_details %>%
    filter(apt_name == airport) %>%
    filter(yyyy >= since & yyyy <= to) %>%
    group_by(yyyy, delay_group) %>%
    summarise(delay = sum(delay)) 
  
  all_summaries_apt <- apt_all_summaries %>%
    filter(apt_name == airport) %>%
    filter(yyyy >= since & yyyy <= to) %>%
    group_by(yyyy) %>%
    summarise(arrival_delay = sum(arrival_delay), num_arrivals = sum(num_arrivals))
  
  all_apt <- inner_join(all_details_apt, all_summaries_apt, by = "yyyy")
  
  all_apt <- all_apt %>%
    mutate(avg = arrival_delay/num_arrivals,
           dly = delay/num_arrivals) %>%
    select(- delay)
  
  all_apt$yyyy <- as.character(all_apt$yyyy)
  
  g <- ggplot(all_apt, aes(x = yyyy, y = dly, fill = delay_group))
  
  apt_arr_dly_plot <- g + 
    geom_bar(stat = "identity") +
    theme_pru() +
    scale_color_pru() +
    scale_fill_viridis(discrete = TRUE,
                       name = "",
                       breaks = c("ATC attributed", "W", "G", "Other"),
                       labels = c(
                         "ATC attributed [C, S, I, T]",
                         "Weather [W, D]",
                         "Airport Capacity [G]",
                         "Other [all other codes]")) +
    labs(x = "",
         y = "Avg. airport ATFM arrival delay per arrival (min)",
         title = "",
         subtitle = "") +
    # ylim(0,1) +
    theme(legend.position = "bottom")
  add_logo(plot_name = apt_arr_dly_plot,
           source = "Source: PRU analysis",
           width_pixels = 640,
           height_pixels = 450)
  
  # apt_arr_dly_plot
}