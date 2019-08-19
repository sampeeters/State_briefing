
# Flights hours controlled

Share_flight_hours_controlled=sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$CFMU_DURATION_HR)/
  sum(filter(PRU_FAC_data, UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & MONTH<=curr_month-1)$CFMU_DURATION_HR)
  
Share_flight_hours_controlled_pie=data.frame(Area=c(ANSP, "Rest"),
                                             Flt_hours=c(sum(filter(PRU_FAC_data, 
                                                                    UNIT_NAME==ANSP & YEAR == curr_year & 
                                                                      MONTH<=curr_month-1)$CFMU_DURATION_HR),
                                                         sum(filter(PRU_FAC_data, 
                                                                    UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & 
                                                                      MONTH<=curr_month-1)$CFMU_DURATION_HR)-
                                                           sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & 
                                                                        MONTH<=curr_month-1)$CFMU_DURATION_HR)),
                                             Share=c(Share_flight_hours_controlled, 1-Share_flight_hours_controlled)) %>% 
  mutate(end = 2 * pi * cumsum(Flt_hours)/sum(Flt_hours)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_flight_hours_controlled_pie_plot=ggplot(Share_flight_hours_controlled_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Area)) +
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_flight_hours_controlled*100, 1), nsmall=1), "%"))) +
  scale_fill_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "% flight hours controlled\nin the EUROCONTROL area")
add_logo(plot_name = Share_flight_hours_controlled_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_flt_hours_pie.png"))
