avg_daily_arrivals <- function(airport, since, to){

data <- apt_all_details %>%
  group_by(yyyy) %>%
  filter(apt_name == airport) %>%
  filter(yyyy >= since & yyyy <= to) %>%
  summarise(avg_arr = sum(num_arrivals)/length(num_arrivals))

g <- ggplot(data, aes(x = yyyy, y = avg_arr))

avg_daily_arr_plot <- g + 
  geom_line(size = 2, alpha = 0.6, col = '#b2182b', group=0) +
  theme_pru() +
  scale_color_pru() +
  labs(x = "",
       y = "Avg. daily arrivals",
       title = "",
       subtitle = "") +
  ylim(c(0, max(data$avg_arr)))

add_logo(plot_name = avg_daily_arr_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450)

# avg_daily_arr_plot
}

