
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

Complexity_results_ECTL=filter(CPLX_data, BADA_VERSION==BADA_curr & 
                                 MONTH_NUM<=curr_month-1 & YEAR %in% c(curr_year-1, curr_year)) %>% 
  group_by(YEAR) %>% 
  summarise(CPLX_FLIGHT_HRS=sum(CPLX_FLIGHT_HRS),
            CPLX_INTER_HRS=sum(CPLX_INTER_HRS),
            VERTICAL_INTER_HRS=sum(VERTICAL_INTER_HRS),
            HORIZ_INTER_HRS=sum(HORIZ_INTER_HRS),
            SPEED_INTER_HRS=sum(SPEED_INTER_HRS)) %>% 
  mutate(CPLX_Score=(VERTICAL_INTER_HRS+HORIZ_INTER_HRS+SPEED_INTER_HRS)/CPLX_FLIGHT_HRS*60,
         `Vertical interactions`=VERTICAL_INTER_HRS/CPLX_FLIGHT_HRS*60,
         `Horizontal interactions`=HORIZ_INTER_HRS/CPLX_FLIGHT_HRS*60,
         `Speed interactions`=SPEED_INTER_HRS/CPLX_FLIGHT_HRS*60)

Complexity_results_ANSP=filter(CPLX_data, BADA_VERSION==BADA_curr & ENTITY_NAME==ANSP & 
                            MONTH_NUM<=curr_month-1 & YEAR %in% c(curr_year-1, curr_year)) %>% 
  group_by(YEAR) %>% 
  summarise(CPLX_FLIGHT_HRS=sum(CPLX_FLIGHT_HRS),
            CPLX_INTER_HRS=sum(CPLX_INTER_HRS),
            VERTICAL_INTER_HRS=sum(VERTICAL_INTER_HRS),
            HORIZ_INTER_HRS=sum(HORIZ_INTER_HRS),
            SPEED_INTER_HRS=sum(SPEED_INTER_HRS)) %>% 
  mutate(CPLX_Score=(VERTICAL_INTER_HRS+HORIZ_INTER_HRS+SPEED_INTER_HRS)/CPLX_FLIGHT_HRS*60,
         `Vertical interactions`=VERTICAL_INTER_HRS/CPLX_FLIGHT_HRS*60,
         `Horizontal interactions`=HORIZ_INTER_HRS/CPLX_FLIGHT_HRS*60,
         `Speed interactions`=SPEED_INTER_HRS/CPLX_FLIGHT_HRS*60)

Complexity_table=data.frame(Term="Complexity score (min. interactions per hr)",
                            Value=filter(Complexity_results_ANSP, YEAR==curr_year)$CPLX_Score,
                            Diff=filter(Complexity_results_ANSP, YEAR==curr_year)$CPLX_Score-
                                                filter(Complexity_results_ANSP, YEAR==curr_year-1)$CPLX_Score,
                            Text1="vs. same period in previous year") %>% 
  mutate(Value=format(round(Value, 2), nsmall=2),
         Diff1=Diff,
         Diff=paste0(ifelse(Diff>0, "+", ifelse(round(Diff, 1)==0 & Diff<0, "-", "")), format(round(Diff, 2), nsmall=2)),
         Colour=case_when(Diff1<0 ~ "green4", Diff1>=0 ~ "red"))
Complexity_table_colours=Complexity_table$Colour
Complexity_table=select(Complexity_table, -Diff1, -Colour)
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

CPLX_results=rbind(cbind(select(filter(Complexity_results_ANSP, YEAR==curr_year), `Vertical interactions`,
                                `Horizontal interactions`, `Speed interactions`), Area=ANSP),
                   cbind(select(filter(Complexity_results_ECTL, YEAR==curr_year), `Vertical interactions`,
                                `Horizontal interactions`, `Speed interactions`), Area="EUROCONTROL")) %>% 
  melt(id.vars = "Area") %>% 
  mutate(variable=factor(variable, levels=c("Horizontal interactions", "Speed interactions", "Vertical interactions")))
CPLX_scores=rbind(cbind(select(filter(Complexity_results_ANSP, YEAR==curr_year), CPLX_Score), Area=ANSP),
                  cbind(select(filter(Complexity_results_ECTL, YEAR==curr_year), CPLX_Score), Area="EUROCONTROL")) %>% 
  mutate(CPLX_Score=as.numeric(format(round(CPLX_Score, 2), nsmall=2)))

CPLX_bar_plot=ggplot(CPLX_results, aes(x=Area, y=value)) + 
  geom_bar(data=filter(CPLX_results, variable!="CPLX_Score"), aes(fill=variable), stat = "identity")  +
  geom_text(data = CPLX_scores, aes(Area, CPLX_Score+0.2, label=CPLX_Score, fill=NULL)) +
  theme_pru() +
  scale_fill_pru() +
  coord_flip() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major.y = element_blank()) +
  labs(x="", title = "Complexity score\n(Minutes of interactions per flight hour)")
add_logo(plot_name = CPLX_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/CPLX_bar.png"))




# Complexity score time line

Complexity_results_ANSP_month=filter(CPLX_data, BADA_VERSION==BADA_curr & ENTITY_NAME==ANSP & 
                                 YEAR %in% c(curr_year-2, curr_year-1, curr_year)) %>% 
  group_by(MONTH_NUM, YEAR) %>% 
  summarise(CPLX_FLIGHT_HRS=sum(CPLX_FLIGHT_HRS),
            VERTICAL_INTER_HRS=sum(VERTICAL_INTER_HRS),
            HORIZ_INTER_HRS=sum(HORIZ_INTER_HRS),
            SPEED_INTER_HRS=sum(SPEED_INTER_HRS)) %>% 
  mutate(CPLX_Score=(VERTICAL_INTER_HRS+HORIZ_INTER_HRS+SPEED_INTER_HRS)/CPLX_FLIGHT_HRS*60,
         Date=as.POSIXct(paste0("01-", MONTH_NUM, "-", YEAR), format="%d-%m-%Y"))

CPLX_timeline_plot=ggplot(Complexity_results_ANSP_month, aes(x=Date, y=CPLX_Score)) + 
  geom_line(size=2, color="blue")  +
  theme_pru() +
  scale_fill_pru() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x="", y="Interactions per flight hr\n(min)", title = paste0(ANSP, " - Complexity score")) +
  lims(y=c(0, 15))
add_logo(plot_name = CPLX_timeline_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/CPLX_timeline.png"))





# OVERVIEW

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 8, 8))
g=arrangeGrob(Header_table, 
              Share_flight_hours_controlled_pie_plot, Share_ENR_ATFM_delay_pie_plot,
              Share_ENR_ATFM_delayed_flights_pie_plot, ENR_ATFM_delay_flight_plot,
              Complexity_table,
              CPLX_bar_plot, CPLX_timeline_plot,
              as.table=TRUE,
              heights=c(0.1, 0.5, 0.2, 0.3),
              widths = c(2, 2, 2, 2),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/ENR_capacity_overview.png"), plot=g, width = 20, height = 21, units = "cm", dpi=200)
