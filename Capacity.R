
# Heading key figures

Header=data.frame(Text="", Text2="")
names(Header)=c(paste0("Key figures (", curr_year, " - ", 
                          ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), "")

Header_table=tableGrob(Header, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                       bg_params=list(fill="#3399cc")),
                                                             core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table$widths = unit(c(0.95, 0.05), "npc")





# Share flights hours controlled

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
                                                                        MONTH<=curr_month-1)$CFMU_DURATION_HR))) %>% 
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





# Share en-route ATFM delay

Share_ENR_ATFM_delay=sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$TDM_ERT)/
  sum(filter(PRU_FAC_data, UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & MONTH<=curr_month-1)$TDM_ERT)

Share_ENR_ATFM_delay_pie=data.frame(Area=c(ANSP, "Rest"),
                                    ENR_delay=c(sum(filter(PRU_FAC_data, 
                                                           UNIT_NAME==ANSP & YEAR == curr_year & 
                                                             MONTH<=curr_month-1)$TDM_ERT),
                                                sum(filter(PRU_FAC_data, 
                                                           UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & 
                                                             MONTH<=curr_month-1)$TDM_ERT)-
                                                  sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & 
                                                               MONTH<=curr_month-1)$TDM_ERT))) %>% 
  mutate(end = 2 * pi * cumsum(ENR_delay)/sum(ENR_delay)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_ENR_ATFM_delay_pie_plot=ggplot(Share_ENR_ATFM_delay_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Area)) +
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_ENR_ATFM_delay*100, 1), nsmall=1), "%"))) +
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
  labs(title = "% of en-route ATFM delay\nin the EUROCONTROL area")
add_logo(plot_name = Share_ENR_ATFM_delay_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_ENR_ATFM_delay_pie.png"))





# Share of en-route ATFM delayed flights

Share_ENR_ATFM_delayed_flights=sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$TDF_ERT)/
  sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$TTF_FLT)

Share_ENR_ATFM_delayed_flights_pie=data.frame(Area=c("Delayed", "Rest"),
                                              ENR_delay=c(sum(filter(PRU_FAC_data, 
                                                                     UNIT_NAME==ANSP & YEAR == curr_year & 
                                                                       MONTH<=curr_month-1)$TDF_ERT),
                                                          sum(filter(PRU_FAC_data, 
                                                                     UNIT_NAME==ANSP & YEAR == curr_year & 
                                                                       MONTH<=curr_month-1)$TTF_FLT)-
                                                            sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & 
                                                                         MONTH<=curr_month-1)$TDF_ERT))) %>% 
  mutate(end = 2 * pi * cumsum(ENR_delay)/sum(ENR_delay)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_ENR_ATFM_delayed_flights_pie_plot=ggplot(Share_ENR_ATFM_delayed_flights_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Area)) +
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_ENR_ATFM_delayed_flights*100, 1), nsmall=1), "%"))) +
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
  labs(title = "% of en-route ATFM\ndelayed flights")
add_logo(plot_name = Share_ENR_ATFM_delayed_flights_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_ENR_ATFM_delayed_flights_pie.png"))





# En-route ATFM delay per flight

ENR_ATFM_delay_flight=sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$TDM_ERT)/
  sum(filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR == curr_year & MONTH<=curr_month-1)$TTF_FLT)

ENR_ATFM_delay_flight_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, label=paste0(ANSP, "\n", format(round(ENR_ATFM_delay_flight, 2), nsmall=1)), colour="grey", size=5) +
  scale_fill_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "En-route ATFM delay\nper flight (min)")
add_logo(plot_name = ENR_ATFM_delay_flight_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_ATFM_delay_flight.png"))





# Complexity table

Complexity_table=data.frame(Term="Complexity score (min. intercations per hr)",
                            Value=10.59,
                            Diff=-0.07,
                            Text="vs. same period in previous year") %>% 
  mutate(Colour=case_when(Diff<0 ~ "green4", Diff>=0 ~ "red"))
Complexity_table_colours=Complexity_table$Colour
Complexity_table=select(Complexity_table, -Colour)
names(Complexity_table)=c(paste0("Complexity (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), "", "", "")

Complexity_table1=tableGrob(Complexity_table[1], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Complexity_table2=tableGrob(Complexity_table[2], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10, fontface="bold")),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Complexity_table3=tableGrob(Complexity_table[3], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, 
                                                                                       fontsize=10, col=Tfc_ev_state_colours)),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Complexity_table4=tableGrob(Complexity_table[4], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Complexity_table=gtable_combine(Complexity_table1, Complexity_table2,Complexity_table3, Complexity_table4)
Complexity_table$widths = unit(c(0.4, 0.08, 0.06, 0.46), "npc")





# Complexity score bars




# OVERVIEW

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6))
g=arrangeGrob(Header_table, 
              Share_flight_hours_controlled_pie_plot, Share_ENR_ATFM_delay_pie_plot,
              Share_ENR_ATFM_delayed_flights_pie_plot, ENR_ATFM_delay_flight_plot,
              Complexity_table,
              as.table=TRUE,
              heights=c(0.1, 0.5, 0.2),
              widths = c(2, 2, 2, 2),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/ENR_capacity_overview.png"), plot=g, width = 20, height = 21, units = "cm", dpi=200)
