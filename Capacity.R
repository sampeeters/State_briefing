
## En-route ATFM delays

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
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_flight_hours_controlled*100, 1), nsmall=1), "%")), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
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
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_ENR_ATFM_delay*100, 1), nsmall=1), "%")), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
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
  annotate("text", x=0, y=0, label=paste0(ANSP, "\n", paste0(format(round(Share_ENR_ATFM_delayed_flights*100, 1), nsmall=1), "%")), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
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
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
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
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10)) +
  labs(x="", title = "Complexity score\n(Minutes of interactions per flight hour)") + 
  guides(fill = guide_legend(nrow = 2, byrow = T))
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
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(x="", y="Interactions per flight hr\n(min)", title = paste0(ANSP, " - Complexity score")) +
  lims(y=c(0, 15))
add_logo(plot_name = CPLX_timeline_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/CPLX_timeline.png"))




# Capacity table
Capacity_data=filter(PRU_FAC_data, UNIT_NAME==ANSP & YEAR %in% seq(curr_year-6, curr_year) & MONTH<=curr_month-1) %>% 
  group_by(YEAR) %>% 
  summarise(Nbr_days=n_distinct(ENTRY_DATE),
            Con_fl=sum(TTF_FLT, na.rm=TRUE),
            Tot_ATFM_delay_flight=sum(TDM, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            Apt_ATFM_delay_flight=sum(TDM_ARP, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            ENR_ATFM_delay_flight=sum(TDM_ERT, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            ENR_ATFM_delayed_flights=sum(TDF_ERT, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            Days_ENR_delay_1min=sum(DAY_ERT_1MIN, na.rm=TRUE),
            Tot_ENR_ATFM_delay=sum(TDM_ERT, na.rm=TRUE),
            ENR_ATFM_delay_C=sum(TDM_ERT_C, na.rm=TRUE),
            ENR_ATFM_delay_S=sum(TDM_ERT_S, na.rm=TRUE),
            ENR_ATFM_delay_I_T=sum(TDM_ERT_I, na.rm=TRUE)+sum(TDM_ERT_T, na.rm=TRUE),
            ENR_ATFM_delay_W_D=sum(TDM_ERT_W, na.rm=TRUE)+sum(TDM_ERT_D, na.rm=TRUE),
            ENR_ATFM_delay_other=sum(TDM_ERT_NA, na.rm=TRUE)+sum(TDM_ERT_A, na.rm=TRUE)+sum(TDM_ERT_E, na.rm=TRUE)+sum(TDM_ERT_G, na.rm=TRUE)+
              sum(TDM_ERT_M, na.rm=TRUE)+sum(TDM_ERT_N, na.rm=TRUE)+sum(TDM_ERT_O, na.rm=TRUE)+sum(TDM_ERT_P, na.rm=TRUE)+
              sum(TDM_ERT_R, na.rm=TRUE)+sum(TDM_ERT_V, na.rm=TRUE)
            ) %>% 
  arrange(YEAR)
Capacity_data[nrow(Capacity_data)+1,]=Capacity_data[nrow(Capacity_data),]-Capacity_data[nrow(Capacity_data)-1,]

Capacity_data_table=data.frame(Term=c("Controlled flights",
                                      "Total ATFM delay per flight (min/flt)",
                                      "Airport ATFM delay per flight (min/flt)",
                                      "En-route ATFM delay per flight (min/flt)",
                                      "En-route ATFM delayed flights (%)",
                                      "Days with avg. en-route delay >1 min",
                                      "Total En-route ATFM delay (min)",
                                      "ATC Capacity [code C]",
                                      "ATC Staffing [code S]",
                                      "ATC Disruptions [code I,T]",
                                      "Adverse weather [code W,D]",
                                      "Other [all other codes]"),
                               Curr_year=as.numeric(select(filter(Capacity_data, YEAR==curr_year), -YEAR, -Nbr_days)),
                               Diff_prev_year=c(filter(Capacity_data, YEAR==1)$Con_fl/
                                                  filter(Capacity_data, YEAR==curr_year-1)$Con_fl*100,
                                                filter(Capacity_data, YEAR==1)$Tot_ATFM_delay_flight,
                                                filter(Capacity_data, YEAR==1)$Apt_ATFM_delay_flight,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_flight,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delayed_flights*100,
                                                filter(Capacity_data, YEAR==1)$Days_ENR_delay_1min,
                                                filter(Capacity_data, YEAR==1)$Tot_ENR_ATFM_delay/
                                                  filter(Capacity_data, YEAR==curr_year-1)$Tot_ENR_ATFM_delay*100,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_C/
                                                  filter(Capacity_data, YEAR==curr_year-1)$ENR_ATFM_delay_C*100,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_S/
                                                  filter(Capacity_data, YEAR==curr_year-1)$ENR_ATFM_delay_S*100,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_I_T/
                                                  filter(Capacity_data, YEAR==curr_year-1)$ENR_ATFM_delay_I_T*100,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_W_D/
                                                  filter(Capacity_data, YEAR==curr_year-1)$ENR_ATFM_delay_W_D*100,
                                                filter(Capacity_data, YEAR==1)$ENR_ATFM_delay_other/
                                                  filter(Capacity_data, YEAR==curr_year-1)$ENR_ATFM_delay_other*100
                                                ),
                               Perc=c("%", "", "", "", "%", "", "%", "%", "%", "%", "%", "%"),
                               Digits=c(1, 2, 2, 2, 1, 0, 1, 1, 1, 1, 1, 1)) %>% 
  mutate(Text=paste0(ifelse(Diff_prev_year>0, "+", ifelse(round(Diff_prev_year, Digits)==0 & Diff_prev_year<0, "-", "")), 
                     format(round(Diff_prev_year, Digits), nsmall=Digits), Perc),
         Colour=ifelse(Term=="Controlled flights",
                       case_when(Diff_prev_year<0 ~ "red", Diff_prev_year>=0 ~ "green4"),
                       case_when(Diff_prev_year<0 ~ "green4", Diff_prev_year>=0 ~ "red")))
Capacity_data_table_colours=Capacity_data_table$Colour
Capacity_data_table=select(Capacity_data_table, -Diff_prev_year, -Colour, -Perc, -Digits)
names(Capacity_data_table)=c(paste0("Capacity (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), 
                             curr_year, paste0("vs ", curr_year-1))

Capacity_data_table1=tableGrob(Capacity_data_table[1], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10),
                                                           bg_params=list(fill=c("white", "white", "white", "white", "white", "white", 
                                                                                 "#d9d9d9", pru_pal()(5)))),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Capacity_data_table2=tableGrob(Capacity_data_table[2], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Capacity_data_table3=tableGrob(Capacity_data_table[3], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, 
                                                                          fontsize=10,col=Capacity_data_table_colours)),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Capacity_data_table=gtable_combine(Capacity_data_table1, Capacity_data_table2, Capacity_data_table3)
Capacity_data_table$widths = unit(c(0.64, 0.18, 0.18), "npc")





# Evolution of traffic and average ATFM delay per flight

Capacity_data2=filter(Capacity_data,YEAR!=1) %>% 
  mutate(Avg_ENR_ATFM_delay=Tot_ENR_ATFM_delay/Con_fl,
         `ATC Capacity [C]`=ENR_ATFM_delay_C/Con_fl,
         `ATC Staffing [S]`=ENR_ATFM_delay_S/Con_fl,
         `ATC Disruptions [I,T]`=ENR_ATFM_delay_I_T/Con_fl,
         `Weather [W,D]`=ENR_ATFM_delay_W_D/Con_fl,
         `Other [all other codes]`=ENR_ATFM_delay_other/Con_fl,
         Avg_nbr_flights=Con_fl/Nbr_days,
         Avg_nbr_flights_diff=(Avg_nbr_flights/lag(Avg_nbr_flights)-1)*100) %>% 
  filter(YEAR %in% seq(curr_year-5, curr_year))
  
Capacity_data3=select(Capacity_data2,-Con_fl, -Tot_ATFM_delay_flight, -Apt_ATFM_delay_flight, -ENR_ATFM_delay_flight, 
                      -ENR_ATFM_delayed_flights, -Days_ENR_delay_1min, -Tot_ENR_ATFM_delay, -ENR_ATFM_delay_C, -ENR_ATFM_delay_S, 
                      -ENR_ATFM_delay_I_T, -ENR_ATFM_delay_W_D, -ENR_ATFM_delay_other, -Avg_nbr_flights) 
  
Capacity_data_melt=melt(Capacity_data3, id.vars = "YEAR") %>% 
  mutate(variable=factor(variable, levels = c("ATC Capacity [C]", "ATC Staffing [S]", "Weather [W,D]", "ATC Disruptions [I,T]",
                                              "Other [all other codes]")))

Capacity_data_bar_plot=ggplot(Capacity_data_melt, aes(x=YEAR, y=value)) + 
  geom_bar(data=filter(Capacity_data_melt, variable!="Avg_ENR_ATFM_delay"), aes(fill=variable), stat = "identity")  +
  geom_text(data = filter(Capacity_data2, YEAR %in% seq(curr_year-5, curr_year)), 
            aes(YEAR, Avg_ENR_ATFM_delay+0.1, label=format(round(Avg_ENR_ATFM_delay, 1), nsmall=1), fill=NULL))+
  geom_line(data = Capacity_data2, aes(x=YEAR, y=Avg_nbr_flights*max(Capacity_data2$Avg_ENR_ATFM_delay)/max(Capacity_data2$Avg_nbr_flights), 
                                       colour="blue"), 
            stat="identity", size=2) +
  scale_colour_manual(name = "", values="blue", label="Avg. daily flights") +
  geom_text(data = Capacity_data2, aes(YEAR, Avg_nbr_flights*max(Capacity_data2$Avg_ENR_ATFM_delay)/max(Capacity_data2$Avg_nbr_flights)+0.1, 
                                       label=paste0(ifelse(Avg_nbr_flights_diff>0, "+", ""), format(round(Avg_nbr_flights_diff, 1), nsmall=1),
                                                    "%"), fill=NULL), colour="blue")+
  theme_pru() +
  scale_y_continuous(sec.axis = sec_axis(~./max(Capacity_data2$Avg_ENR_ATFM_delay)*max(Capacity_data2$Avg_nbr_flights))) +
  scale_fill_pru() +
  scale_x_continuous(breaks=seq(curr_year-5, curr_year)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical") +
  labs(x="", y="", title = "Evolution of traffic and avg. en-route ATFM delay (min)") + 
  guides(fill = guide_legend(nrow = 2, byrow = T))
add_logo(plot_name = Capacity_data_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Capacity_data_bar.png"))





# En-route ATFM delay groups

Data_temp=filter(Capacity_data2, YEAR==curr_year) %>% 
  select(`ATC Capacity`=ENR_ATFM_delay_C,
         `ATC Staffing`=ENR_ATFM_delay_S,
         `ATC Disruptions`=ENR_ATFM_delay_I_T,
         `Adverse weather`=ENR_ATFM_delay_W_D,
         `Other`=ENR_ATFM_delay_other)
ENR_ATFM_delay_groups_pie=data.frame(Group=colnames(Data_temp),
                                     Value=as.numeric(Data_temp[1,])) %>% 
  mutate(end = 2 * pi * cumsum(Value)/sum(Value)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1),
         Share=Value/filter(Capacity_data2, YEAR==curr_year)$Tot_ENR_ATFM_delay*100,
         Group=factor(Group, levels = c("ATC Capacity", "ATC Staffing", "ATC Disruptions", "Adverse weather",
                                              "Other")))
ENR_ATFM_delay_groups_pie_plot=ggplot(ENR_ATFM_delay_groups_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Group)) +
  geom_text(aes(x = 1.1 * sin(middle), y = 1.1 * cos(middle), label = paste0(format(round(Share, 1), nsmall=1), "%"),
                hjust = hjust, vjust = vjust), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_blank()) +
  labs(title = "")
add_logo(plot_name = ENR_ATFM_delay_groups_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_ATFM_delay_groups_pie.png"))



# OVERVIEW

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 8, 8),
             c(9, 9, 10, 10),
             c(9, 9, 11, 11))
g=arrangeGrob(Header_table, 
              Share_flight_hours_controlled_pie_plot, Share_ENR_ATFM_delay_pie_plot,
              Share_ENR_ATFM_delayed_flights_pie_plot, ENR_ATFM_delay_flight_plot,
              Complexity_table,
              CPLX_bar_plot, CPLX_timeline_plot,
              Capacity_data_table,
              Capacity_data_bar_plot,
              ENR_ATFM_delay_groups_pie_plot,
              as.table=TRUE,
              nrow=6,
              ncol=4,
              heights=c(0.05, 0.2, 0.1, 0.2, 0.25, 0.2),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/ENR_capacity_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)




# Average en route ATFM delay per flight (all ANSPs)

Data_ANSPs=filter(PRU_FAC_data, YEAR %in% c(curr_year-1, curr_year) & MONTH<=curr_month-1) %>% 
  group_by(UNIT_NAME, YEAR) %>% 
  summarise(Con_fl=sum(TTF_FLT, na.rm=TRUE),
            `ATC Capacity [C]`=sum(TDM_ERT_C, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            `ATC Staffing [S]`=sum(TDM_ERT_S, na.rm=TRUE)/sum(TTF_FLT, na.rm=TRUE),
            `ATC Disruptions [I,T]`=(sum(TDM_ERT_I, na.rm=TRUE)+sum(TDM_ERT_T, na.rm=TRUE))/sum(TTF_FLT, na.rm=TRUE),
            `Weather [W,D]`=(sum(TDM_ERT_W, na.rm=TRUE)+sum(TDM_ERT_D, na.rm=TRUE))/sum(TTF_FLT, na.rm=TRUE),
            `Other reasons`=(sum(TDM_ERT_NA, na.rm=TRUE)+sum(TDM_ERT_A, na.rm=TRUE)+sum(TDM_ERT_E, na.rm=TRUE)+sum(TDM_ERT_G, na.rm=TRUE)+
              sum(TDM_ERT_M, na.rm=TRUE)+sum(TDM_ERT_N, na.rm=TRUE)+sum(TDM_ERT_O, na.rm=TRUE)+sum(TDM_ERT_P, na.rm=TRUE)+
              sum(TDM_ERT_R, na.rm=TRUE)+sum(TDM_ERT_V, na.rm=TRUE))/sum(TTF_FLT, na.rm=TRUE),
            Tot_ENR_ATFM_delay=`ATC Capacity [C]`+`ATC Staffing [S]`+`ATC Disruptions [I,T]`+`Weather [W,D]`+`Other reasons`
  ) %>% 
  filter(UNIT_NAME %in% FAB_ANSPs$ANSP) %>% 
  left_join(FAB_ANSPs, by=c("UNIT_NAME"="ANSP")) %>% 
  arrange(YEAR, FAB, UNIT_NAME)

Data_ANSPs2=filter(Data_ANSPs, YEAR==curr_year) %>%
  select(-YEAR, -Tot_ENR_ATFM_delay, -Con_fl) %>% 
  melt(id.vars = c("UNIT_NAME", "FAB"))

Data_ANSPs_prev_year=filter(Data_ANSPs, YEAR==curr_year-1)
bars=rep(Data_ANSPs_prev_year$Tot_ENR_ATFM_delay, 5)

ENR_delay_flight_bar_plot=ggplot(Data_ANSPs2, aes(x=UNIT_NAME, y=value)) + 
  geom_bar(aes(fill=variable), stat = "identity")  +
  geom_text(data = filter(Data_ANSPs, YEAR==curr_year), 
            aes(UNIT_NAME, Tot_ENR_ATFM_delay+0.05, label=format(round(Tot_ENR_ATFM_delay, 2), nsmall=2), fill=NULL, angle=90))+
  geom_errorbar(aes(ymin = bars, ymax = bars), 
                size = 2, colour="black") + 
  scale_colour_continuous(name="", label=paste0(curr_year-1, " (Jan-", month.abb[curr_month-1], ")")) +
  facet_grid(.~FAB, scales = "free_x", switch="x") +
  theme_pru() +
  scale_fill_pru() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical",
        axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
  labs(x="", y="Average delay per flight (min.)", 
       title = paste0("Average en-route ATFM delay per flight - Jan-", month.abb[curr_month-1], " ", curr_year, " (min)"))
add_logo(plot_name = ENR_delay_flight_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_delay_flight_bar.png"))

Changes_vs_prev_year=arrange(Data_ANSPs, UNIT_NAME, YEAR) %>% 
  mutate(C_diff=`ATC Capacity [C]`-lag(`ATC Capacity [C]`),
         S_diff=`ATC Staffing [S]`-lag(`ATC Staffing [S]`),
         IT_diff=`ATC Disruptions [I,T]`-lag(`ATC Disruptions [I,T]`),
         WD_diff=`Weather [W,D]`-lag(`Weather [W,D]`),
         Other_diff=`Other reasons`-lag(`Other reasons`),
         Tot=C_diff+S_diff+IT_diff+WD_diff+Other_diff,
         Tfc_diff=Con_fl/lag(Con_fl)-1)
  
Changes_vs_prev_year2=filter(Changes_vs_prev_year, YEAR==curr_year) %>%
  select(-YEAR, -Tot_ENR_ATFM_delay, -Con_fl, -`ATC Capacity [C]`, -`ATC Staffing [S]`, -`ATC Disruptions [I,T]`, -`Weather [W,D]`, 
         -`Other reasons`, -Tfc_diff, -Tot) %>% 
  rename(`ATC Capacity [C]`=C_diff, `ATC Staffing [S]`=S_diff, `ATC Disruptions [I,T]`=IT_diff, `Weather [W,D]`=WD_diff, 
         `Other reasons`=Other_diff) %>% 
  melt(id.vars = c("UNIT_NAME", "FAB"))

ENR_delay_flight_change_bar_plot=ggplot(Changes_vs_prev_year2, aes(x=UNIT_NAME, y=value)) + 
  geom_bar(aes(fill=variable), stat = "identity")  +
  geom_point(data=Changes_vs_prev_year, 
             aes(x=UNIT_NAME, y=Tfc_diff*max(Changes_vs_prev_year$Tot, na.rm = TRUE)/max(Changes_vs_prev_year$Tfc_diff, na.rm = TRUE)),
             size=3, shape=18, colour="blue") +
  geom_text(data = filter(Changes_vs_prev_year, YEAR==curr_year), 
            aes(UNIT_NAME, Tfc_diff*max(Changes_vs_prev_year$Tot, na.rm = TRUE)/max(Changes_vs_prev_year$Tfc_diff, na.rm = TRUE)+0.08, 
                label=paste0(format(round(Tfc_diff*100, 1), nsmall=1), "%"), fill=NULL, angle=90), colour="blue", vjust=0.3)+
  facet_grid(.~FAB, scales = "free_x", switch="x") +
  theme_pru() +
  scale_y_continuous(sec.axis = sec_axis(~./max(Changes_vs_prev_year$Tot, na.rm = TRUE)*
                                           max(Changes_vs_prev_year$Tfc_diff, na.rm = TRUE),
                     labels = scales::percent, name="Traffic change (%)")) +
  scale_fill_pru() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical",
        axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
  labs(x="", y="Change (minutes per flight)", 
       title = paste0("Change in traffic and average en route ATFM delay per flight vs same period in the previous year (Jan-", 
                      month.abb[curr_month-1], " ", curr_year, ")"))
add_logo(plot_name = ENR_delay_flight_change_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_delay_flight_change_bar.png"))






## Airport arrival ATFM delays

# Heading key figures

Header=data.frame(Text="", Text2="")
names(Header)=c(paste0("Key figures (", curr_year, " - ", 
                       ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), "")

Header_table=tableGrob(Header, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                            bg_params=list(fill="#3399cc")),
                                                               core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table$widths = unit(c(0.95, 0.05), "npc")


# Share of airport arrival ATFM delay

Share_APT_ATFM_delay=sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$DLY_APT_ARR_1,
                         na.rm = TRUE)/
  sum(filter(PRU_FAC_data, UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & MONTH<=curr_month-1)$TDM_ARP)

Share_APT_ATFM_delay_pie=data.frame(Area=c("Delay", "Rest"),
                                    APT_delay=c(sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                                             MONTH_NUM<=curr_month-1)$DLY_APT_ARR_1,
                                                    na.rm = TRUE),
                                                sum(filter(PRU_FAC_data, UNIT_NAME=="EUROCONTROL" & YEAR == curr_year & 
                                                             MONTH<=curr_month-1)$TDM_ARP)-
                                                  sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                                               MONTH_NUM<=curr_month-1)$DLY_APT_ARR_1,
                                                      na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(APT_delay)/sum(APT_delay)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_APT_ATFM_delay_pie_plot=ggplot(Share_APT_ATFM_delay_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Area)) +
  annotate("text", x=0, y=0, label=paste0(format(round(Share_APT_ATFM_delay*100, 1), nsmall=1), "%"), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "% of airport arrival ATFM delay\nin the EUROCONTROL area")
add_logo(plot_name = Share_APT_ATFM_delay_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_APT_ATFM_delay_pie.png"))

# Share of airport ATFM delayed arrivals

Share_APT_ATFM_delayed_arr=sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$FLT_ARR_1_DLY,
                               na.rm = TRUE)/
  sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$FLT_ARR_1, na.rm = TRUE)

Share_APT_ATFM_delayed_arr_pie=data.frame(Area=c("Delay", "Rest"),
                                    APT_delay=c(sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                                             MONTH_NUM<=curr_month-1)$FLT_ARR_1_DLY, na.rm = TRUE),
                                                sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                                             MONTH_NUM<=curr_month-1)$FLT_ARR_1, na.rm = TRUE)-
                                                  sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                                               MONTH_NUM<=curr_month-1)$FLT_ARR_1_DLY, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(APT_delay)/sum(APT_delay)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_APT_ATFM_delayed_arr_pie_plot=ggplot(Share_APT_ATFM_delayed_arr_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = Area)) +
  annotate("text", x=0, y=0, label=paste0(format(round(Share_APT_ATFM_delayed_arr*100, 1), nsmall=1), "%"), size=3) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "% of airport ATFM\ndelayed arrivals")
add_logo(plot_name = Share_APT_ATFM_delayed_arr_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_APT_ATFM_delayed_arr_pie.png"))




# Average airport arrival ATFM delay per flight

Avg_APT_ATFM_delay_flight=sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                               MONTH_NUM<=curr_month-1)$DLY_APT_ARR_1, na.rm = TRUE)/
  sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$FLT_ARR_1, na.rm = TRUE)

Avg_APT_ATFM_delay_flight_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, label=paste0(ANSP, "\n", format(round(Avg_APT_ATFM_delay_flight, 1), nsmall=1)), colour="grey", 
           size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Avg. airport arrival ATFM\ndelay per arrival (min)")
add_logo(plot_name = Avg_APT_ATFM_delay_flight_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_APT_ATFM_delay_flight.png"))

# Average airport arrival ATFM delay per delayed flight

Avg_APT_ATFM_delay_delayed_flight=sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & 
                                       MONTH_NUM<=curr_month-1)$DLY_APT_ARR_1, na.rm = TRUE)/
  sum(filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$FLT_ARR_1_DLY, na.rm = TRUE)

Avg_APT_ATFM_delay_delayed_flight_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, label=paste0(ANSP, "\n", format(round(Avg_APT_ATFM_delay_delayed_flight, 1), nsmall=1)), colour="grey", 
           size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Avg. airport arrival ATFM delay\nper delayed arrival (min)")
add_logo(plot_name = Avg_APT_ATFM_delay_delayed_flight_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_APT_ATFM_delay_delayed_flight.png"))



# Capacity table

Capacity_data_APT=filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR %in% seq(curr_year-1, curr_year) & MONTH_NUM<=curr_month-1) %>% 
  group_by(YEAR) %>% 
  summarise(Con_fl=sum(FLT_ARR_1, na.rm = TRUE),
            Del_fl=sum(FLT_ARR_1_DLY, na.rm = TRUE),
            Tot_APT_ATFM_delay=sum(DLY_APT_ARR_1, na.rm=TRUE),
            APT_ATFM_delay_C_G_S=sum(DLY_APT_ARR_C_1, na.rm=TRUE)+sum(DLY_APT_ARR_G_1, na.rm=TRUE)+sum(DLY_APT_ARR_S_1, na.rm=TRUE),
            APT_ATFM_delay_I_T=sum(DLY_APT_ARR_I_1, na.rm=TRUE)+sum(DLY_APT_ARR_T_1, na.rm=TRUE),
            APT_ATFM_delay_W_D=sum(DLY_APT_ARR_W_1, na.rm=TRUE)+sum(DLY_APT_ARR_D_1, na.rm=TRUE),
            APT_ATFM_delay_other=Tot_APT_ATFM_delay-APT_ATFM_delay_C_G_S-APT_ATFM_delay_I_T-APT_ATFM_delay_W_D
  ) %>% 
  arrange(YEAR) %>% 
  mutate(YEAR=as.numeric(YEAR))
Capacity_data_APT[nrow(Capacity_data_APT)+1,]=Capacity_data_APT[nrow(Capacity_data_APT),]-Capacity_data_APT[nrow(Capacity_data_APT)-1,]

Capacity_data_APT_table=data.frame(Term=c("Arrivals",
                                          "Apt. arrival ATFM delay per arrival (min/arr)",
                                          "Apt. arr. ATFM delay per delayed arrival",
                                          "Airport arr. ATFM delayed flights (%)",
                                          "",
                                          "Total airport arr. ATFM delay (min)",
                                          "Capacity & Staffing [code C, G, S]",
                                          "ATC Disruptions [code I,T]",
                                          "Adverse weather [code W,D]",
                                          "Other [all other codes]"),
                                   Curr_year=c(filter(Capacity_data_APT, YEAR==curr_year)$Con_fl,
                                               format(round(Avg_APT_ATFM_delay_flight, 1), nsmall=1),
                                               format(round(Avg_APT_ATFM_delay_delayed_flight, 1), nsmall=1),
                                               paste0(format(round(Share_APT_ATFM_delayed_arr*100, 1), nsmall=1), "%"),
                                               "",
                                               filter(Capacity_data_APT, YEAR==curr_year)$Tot_APT_ATFM_delay,
                                               filter(Capacity_data_APT, YEAR==curr_year)$APT_ATFM_delay_C_G_S,
                                               filter(Capacity_data_APT, YEAR==curr_year)$APT_ATFM_delay_I_T,
                                               filter(Capacity_data_APT, YEAR==curr_year)$APT_ATFM_delay_W_D,
                                               filter(Capacity_data_APT, YEAR==curr_year)$APT_ATFM_delay_other
                                   ),
                                   Diff_prev_year=c(filter(Capacity_data_APT, YEAR==1)$Con_fl/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Con_fl*100,
                                                    filter(Capacity_data_APT, YEAR==curr_year)$Tot_APT_ATFM_delay/
                                                      filter(Capacity_data_APT, YEAR==curr_year)$Con_fl-
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Tot_APT_ATFM_delay/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Con_fl,
                                                    filter(Capacity_data_APT, YEAR==curr_year)$Tot_APT_ATFM_delay/
                                                      filter(Capacity_data_APT, YEAR==curr_year)$Del_fl-
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Tot_APT_ATFM_delay/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Del_fl,
                                                    filter(Capacity_data_APT, YEAR==curr_year)$Del_fl/
                                                      filter(Capacity_data_APT, YEAR==curr_year)$Con_fl-
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Del_fl/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Con_fl,
                                                    "",
                                                    filter(Capacity_data_APT, YEAR==1)$Tot_APT_ATFM_delay/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$Tot_APT_ATFM_delay*100,
                                                    filter(Capacity_data_APT, YEAR==1)$APT_ATFM_delay_C_G_S/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$APT_ATFM_delay_C_G_S*100,
                                                    filter(Capacity_data_APT, YEAR==1)$APT_ATFM_delay_I_T/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$APT_ATFM_delay_I_T*100,
                                                    filter(Capacity_data_APT, YEAR==1)$APT_ATFM_delay_W_D/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$APT_ATFM_delay_W_D*100,
                                                    filter(Capacity_data_APT, YEAR==1)$APT_ATFM_delay_other/
                                                      filter(Capacity_data_APT, YEAR==curr_year-1)$APT_ATFM_delay_other*100
                                   ),
                                   Perc=c("%", "", "", "%", "", "%", "%", "%", "%", "%"),
                                   Digits=c(1, 2, 2, 1, 0, 1, 1, 1, 1, 1)) %>% 
  mutate(Diff_prev_year=as.numeric(as.character(Diff_prev_year)),
         Text=paste0(ifelse(Diff_prev_year>0, "+", ifelse(round(Diff_prev_year, Digits)==0 & Diff_prev_year<0, "-", "")), 
                     format(round(Diff_prev_year, Digits), nsmall=Digits), Perc),
         Colour=ifelse(Term=="Controlled flights",
                       case_when(Diff_prev_year<0 ~ "red", Diff_prev_year>=0 ~ "green4"),
                       case_when(Diff_prev_year<0 ~ "green4", Diff_prev_year>=0 ~ "red")))
Capacity_data_APT_table_colours=Capacity_data_APT_table$Colour
Capacity_data_APT_table=dplyr::select(Capacity_data_APT_table, -Diff_prev_year, -Colour, -Perc, -Digits)
names(Capacity_data_APT_table)=c(paste0("Capacity (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), 
                             curr_year, paste0("vs ", curr_year-1))

Capacity_data_APT_table1=tableGrob(Capacity_data_APT_table[1], rows=NULL, 
                               theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10),
                                                              bg_params=list(fill=c("white", "white", "white", "white", "white", "white", 
                                                                                    "#d9d9d9", pru_pal()(5)))),
                                                    rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                    colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                 bg_params=list(fill="#3399cc"))))
Capacity_data_APT_table2=tableGrob(Capacity_data_APT_table[2], rows=NULL, 
                               theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                    rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                    colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                 bg_params=list(fill="#3399cc"))))
Capacity_data_APT_table3=tableGrob(Capacity_data_APT_table[3], rows=NULL, 
                               theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, 
                                                                             fontsize=10,col=Capacity_data_table_colours)),
                                                    rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                    colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                 bg_params=list(fill="#3399cc"))))
Capacity_data_APT_table=gtable_combine(Capacity_data_APT_table1, Capacity_data_APT_table2, Capacity_data_APT_table3)
Capacity_data_APT_table$widths = unit(c(0.64, 0.18, 0.18), "npc")



# Heading

Header2=data.frame(Text="", Text2="")
names(Header2)=c(paste0("Traffic and arrival ATFM delay by airport (", State_curr, " (", 
                       ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], "))"))), "")

Header_table2=tableGrob(Header2, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                            bg_params=list(fill="#3399cc")),
                                                               core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table2$widths = unit(c(0.95, 0.05), "npc")



# Share of arrivals per airport

Flights_data_APT=filter(PRU_APT_ATFM_delay, STATE_NAME==State_curr & YEAR ==curr_year & MONTH_NUM<=curr_month-1) %>% 
  group_by(APT_NAME) %>% 
  summarise(Con_fl=sum(FLT_ARR_1, na.rm = TRUE),
            Tot_APT_ATFM_delay=sum(DLY_APT_ARR_1, na.rm=TRUE)
            
  )
Top_flights_APT=arrange(Flights_data_APT, -Con_fl) %>%
  select(-Tot_APT_ATFM_delay) %>% 
  mutate(Share=as.numeric(Con_fl/sum(Con_fl))) %>% 
  head(8)

Top_flights_APT[nrow(Top_flights_APT)+1,]=c("Other", sum(Flights_data_APT$Con_fl)-sum(Top_flights_APT$Con_fl), 
                                             1-sum(Top_flights_APT$Con_fl)/sum(Flights_data_APT$Con_fl))
Top_flights_APT=mutate(Top_flights_APT,
                       APT_NAME=factor(APT_NAME, levels = rev(Top_flights_APT$APT_NAME)),
                       Con_fl=as.numeric(Con_fl),
                       Share=as.numeric(Share))

Top_flights_APT_bar_plot=ggplot(Top_flights_APT, aes(x=APT_NAME, y=Share*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(APT_NAME, Share*100+2, label=paste0(format(round(Share*100, 1), nsmall=1), "%"), fill=NULL))+
  theme_pru() +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical") +
  labs(x="", y="", title = paste0("% of arrivals ", State_curr, " (", 
                                  ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1])), " ", curr_year, ")")) +
  coord_flip()
add_logo(plot_name = Top_flights_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Top_flights_APT_bar.png"))


# Share of ATFM delay per airport

Top_delay_APT=arrange(Flights_data_APT, -Tot_APT_ATFM_delay) %>%
  select(-Con_fl) %>% 
  mutate(Share=as.numeric(Tot_APT_ATFM_delay/sum(Tot_APT_ATFM_delay))) %>% 
  head(8)

Top_delay_APT[nrow(Top_delay_APT)+1,]=c("Other", sum(Flights_data_APT$Tot_APT_ATFM_delay)-sum(Top_delay_APT$Tot_APT_ATFM_delay), 
                                            1-sum(Top_delay_APT$Tot_APT_ATFM_delay)/sum(Flights_data_APT$Tot_APT_ATFM_delay))
Top_delay_APT=mutate(Top_delay_APT,
                       APT_NAME=factor(APT_NAME, levels = rev(Top_delay_APT$APT_NAME)),
                       Tot_APT_ATFM_delay=as.numeric(Tot_APT_ATFM_delay),
                       Share=as.numeric(Share))

Top_delay_APT_bar_plot=ggplot(Top_delay_APT, aes(x=APT_NAME, y=Share*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(APT_NAME, Share*100+2, label=paste0(format(round(Share*100, 1), nsmall=1), "%"), fill=NULL))+
  theme_pru() +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical") +
  labs(x="", y="", title = paste0("% of arrival ATFM delays ", State_curr, " (", 
                                  ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1])), " ", curr_year, ")")) +
  coord_flip()
add_logo(plot_name = Top_delay_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Top_delay_APT_bar.png"))


# OVERVIEW APT

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 7, 7),
             c(6, 6, 8, 8),
             c(9, 9, 9, 9),
             c(10, 10, 11, 11),
             c(12, 12, 12, 12))
g=arrangeGrob(Header_table, 
              Share_APT_ATFM_delay_pie_plot, Share_APT_ATFM_delayed_arr_pie_plot,
              Avg_APT_ATFM_delay_flight_plot, Avg_APT_ATFM_delay_delayed_flight_plot,
              Capacity_data_APT_table,
              Header_table2,
              Top_flights_APT_bar_plot,
              Top_delay_APT_bar_plot,
              as.table=TRUE,
              nrow=6,
              ncol=4,
              heights=c(0.05, 0.2, 0.1, 0.2, 0.05, 0.2, 0.2),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/ENR_capacity_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)
