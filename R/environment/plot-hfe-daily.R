# daily 

hfe_daily <- function(year_start, year_end, state_under_review){

all <- all %>%
  filter(entity_name == state_under_review) %>% 
  mutate(var = ((dist_flown_km / dist_achieved_km) -1)*100) %>%
  select(date, type_model, var) %>%
  mutate(perc = 100 - var) %>%
  filter(date >= year_start & date <= year_end)

ggplot(all, aes(x = as.POSIXct(date), y = perc)) + 
  geom_line(aes(color = type_model), size = 1, alpha = 0.5) +
  geom_ma(ma_fun = SMA, n = 30, aes(color = type_model), # linetype
          show.legend = TRUE, size = 1) +  # simple moving avg
  scale_x_datetime(date_labels = "%b-%Y",
                   date_breaks = "4 month",
                   expand=c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%", accuracy = 1),
                     expand = c(0,0)) +
  #coord_cartesian(ylim=c(90,100)) +
  scale_color_manual("", labels = c("Actual trajectory", "Flight plan", "Shortest constrained route"), 
                     values = c("#b2182b", "#2166ac", "#450000")) +
  labs(y = "Efficiency", x = "") +
  theme(legend.position="bottom")
}