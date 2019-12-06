
# Vertical flight efficiency during climb and descent

# Average time flown level per flight

Avg_time_lvl_flight_CDO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60
Avg_time_lvl_flight_CCO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60


Avg_time_lvl_flight_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=-0.4, ymax=0), fill="green") +
  annotate("text", x=0, y=0.2, label=paste0("Descent: ", format(round(Avg_time_lvl_flight_CDO, 1), nsmall=1)), colour="grey", size=5) +
  annotate("text", x=0, y=-0.2, label=paste0("Climb: ", format(round(Avg_time_lvl_flight_CCO, 1), nsmall=1)), colour="grey50", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Avg. time flown level\nper flight (min)")
add_logo(plot_name = Avg_time_lvl_flight_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_time_lvl_flight.png"))



# Share of level time

source("Multiple_scales_functions.R")

Share_lvl_time_CDO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, YEAR == curr_year & MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)
Share_lvl_time_CCO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, YEAR == curr_year & MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)

Share_lvl_time_CDO_pie=data.frame(Area=c(State_curr, "EUROCONTROL"),
                                  Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE),
                                        sum(filter(CDO_CCO_data, YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)-
                                          sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                       MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_lvl_time_CCO_pie=data.frame(Area=c(State_curr, "EUROCONTROL"),
                                  Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE),
                                        sum(filter(CDO_CCO_data, YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)-
                                          sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                       MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_lvl_time_pie_plot=ggplot() + 
  geom_arc_bar(data=Share_lvl_time_CDO_pie, aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                                                start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "blue")) +
  new_scale_fill() +
  geom_arc_bar(data=Share_lvl_time_CCO_pie, aes(x0 = 0, y0 = 0, r0 = 1, r = 1.4,
                                                start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "green")) +
  annotate("text", x=0, y=-2.2, label=paste0("CDO: ", format(round(Share_lvl_time_CDO*100, 1), nsmall=1), "%\n",
                                             "CCO: ", format(round(Share_lvl_time_CCO*100, 1), nsmall=1), "%"), size=3) +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2.8, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Share of level time with\nrespect to the\nEUROCONTROL total")
add_logo(plot_name = Share_lvl_time_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_lvl_time_pie.png"))



# Median CDO/CCO altitude

CDO_results_per_flight_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CDO_results_per_flight_', curr_year))
CCO_results_per_flight_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CCO_results_per_flight_', curr_year))
Flight_data_CDO_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CDO_', curr_year))
Flight_data_CCO_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CCO_', curr_year))
CDO_data=left_join(select(CDO_results_per_flight_curr_year, SAM_ID, CDO_ALT), select(Flight_data_CDO_curr_year, SAM_ID, ADES_FILED)) %>% 
  filter(substr(ADES_FILED, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CDO_ALT)
Med_CDO_alt=median(CDO_data$CDO_ALT)
CCO_data=left_join(select(CCO_results_per_flight_curr_year, SAM_ID, CCO_ALT), select(Flight_data_CCO_curr_year, SAM_ID, ADEP)) %>% 
  filter(substr(ADEP, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CCO_ALT)
Med_CCO_alt=median(CCO_data$CCO_ALT)


Med_CDO_CCO_alt_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=-0.4, ymax=0), fill="green") +
  annotate("text", x=0, y=0.2, label=paste0("Descent: ", format(round(Med_CDO_alt*100, 0), nsmall=0)), colour="grey", size=5) +
  annotate("text", x=0, y=-0.2, label=paste0("Climb: ", format(round(Med_CCO_alt*100, 0), nsmall=0)), colour="black", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Median CDO/CCO\naltitude (feet)")
add_logo(plot_name = Med_CDO_CCO_alt_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Med_CDO_CCO_alt.png"))



# Share of unimpeded flights

Share_CDO_flights=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                               MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)
Share_CCO_flights=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                               MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)

Share_CDO_flights_pie=data.frame(Area=c("Unimpeded", "Impeded"),
                                 Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE),
                                       sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)-
                                         sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                      MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_CCO_flights_pie=data.frame(Area=c("Unimpeded", "Impeded"),
                                 Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE),
                                       sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)-
                                         sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                      MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_unimpeded_flights_pie_plot=ggplot() + 
  geom_arc_bar(data=Share_CDO_flights_pie, aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                                               start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "blue")) +
  new_scale_fill() +
  geom_arc_bar(data=Share_CCO_flights_pie, aes(x0 = 0, y0 = 0, r0 = 1, r = 1.4,
                                               start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "green")) +
  annotate("text", x=0, y=-2.2, label=paste0("CDO: ", format(round(Share_CDO_flights*100, 1), nsmall=1), "%\n",
                                             "CCO: ", format(round(Share_CCO_flights*100, 1), nsmall=1), "%"), size=3) +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2.8, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "% of unimpeded flights")
add_logo(plot_name = Share_unimpeded_flights_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_unimpeded_flights_pie.png"))



# Heading table

Header2=data.frame(Text="", Text2="", Text3="")
names(Header2)=c("Vertical flight efficiency", "Climb", "Descent")

Header_table2=tableGrob(Header2, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table2$widths = unit(c(0.5, 0.25, 0.25), "npc")

# VFE table

VFE_data=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-1, curr_year) & 
                  MONTH_NUM<=curr_month-1) %>% 
  group_by(YEAR) %>% 
  summarise(Avg_lvl_time_CDO=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60,
            Share_CDO_flights=sum(NBR_CDO_FLIGHTS, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE),
            Avg_lvl_time_CCO=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60,
            Share_CCO_flights=sum(NBR_CCO_FLIGHTS, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE))
VFE_data[nrow(VFE_data)+1,]=VFE_data[2,]-VFE_data[1,]
CDO_results_per_flight_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CDO_results_per_flight_', curr_year-1))
CCO_results_per_flight_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CCO_results_per_flight_', curr_year-1))
Flight_data_CDO_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CDO_', curr_year-1))
Flight_data_CCO_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CCO_', curr_year-1))
CDO_data_prev_year=left_join(select(CDO_results_per_flight_prev_year, SAM_ID, CDO_ALT), select(Flight_data_CDO_prev_year, SAM_ID, ADES_FILED)) %>% 
  filter(substr(ADES_FILED, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CDO_ALT)
Med_CDO_alt_prev_year=median(CDO_data_prev_year$CDO_ALT)
CCO_data_prev_year=left_join(select(CCO_results_per_flight_prev_year, SAM_ID, CCO_ALT), select(Flight_data_CCO_prev_year, SAM_ID, ADEP)) %>% 
  filter(substr(ADEP, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CCO_ALT)
Med_CCO_alt_prev_year=median(CCO_data_prev_year$CCO_ALT)


VFE_data_table=data.frame(Term=c("Avg. time flown level per flight (min.)",
                                 "Median CDO/CCO altitude (feet)",
                                 "Share of unimpeded flights"),
                          Curr_year_climb=c(Avg_time_lvl_flight_CCO, Med_CCO_alt*100, Share_CCO_flights*100),
                          Diff_prev_year_climb=c(filter(VFE_data, YEAR==1)$Avg_lvl_time_CCO, 
                                                 Med_CCO_alt*100-Med_CCO_alt_prev_year*100,
                                                 filter(VFE_data, YEAR==1)$Share_CCO_flights*100),
                          Curr_year_descent=c(Avg_time_lvl_flight_CDO, Med_CDO_alt*100, Share_CDO_flights*100),
                          Diff_prev_year_descent=c(filter(VFE_data, YEAR==1)$Avg_lvl_time_CDO, 
                                                   Med_CDO_alt*100-Med_CDO_alt_prev_year*100,
                                                   filter(VFE_data, YEAR==1)$Share_CDO_flights*100),
                          Perc=c("", "", "%"),
                          Digits=c(1, 0, 1)) %>% 
  mutate(Row=seq(1, 3),
         Curr_year_climb_text=paste0(format(round(Curr_year_climb, Digits), nsmall=Digits), Perc),
         Diff_prev_year_climb_text=paste0(ifelse(Diff_prev_year_climb>0, "+", ifelse(round(Diff_prev_year_climb, Digits)==0 & 
                                                                                       Diff_prev_year_climb<0, "-", "")), 
                                          format(round(Diff_prev_year_climb, Digits), nsmall=Digits), Perc),
         Curr_year_descent_text=paste0(format(round(Curr_year_descent, Digits), nsmall=Digits), Perc),
         Diff_prev_year_descent_text=paste0(ifelse(Diff_prev_year_descent>0, "+", ifelse(round(Diff_prev_year_descent, Digits)==0 & 
                                                                                           Diff_prev_year_descent<0, "-", "")), 
                                            format(round(Diff_prev_year_descent, Digits), nsmall=Digits), Perc),
         Colour_climb=ifelse(Row==1,
                             case_when(Diff_prev_year_climb>0 ~ "red", Diff_prev_year_climb<=0 ~ "green4"),
                             case_when(Diff_prev_year_climb>=0 ~ "green4", Diff_prev_year_climb<0 ~ "red")),
         Colour_descent=ifelse(Row==1,
                               case_when(Diff_prev_year_descent>0 ~ "red", Diff_prev_year_descent<=0 ~ "green4"),
                               case_when(Diff_prev_year_descent>=0 ~ "green4", Diff_prev_year_descent<0 ~ "red")))
VFE_data_table_colours_climb=VFE_data_table$Colour_climb
VFE_data_table_colours_descent=VFE_data_table$Colour_descent
VFE_data_table=dplyr::select(VFE_data_table, Term, Curr_year_climb_text, Diff_prev_year_climb_text, Curr_year_descent_text, 
                             Diff_prev_year_descent_text)
names(VFE_data_table)=c(paste0("Period (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))),
                        curr_year, paste0("vs ", curr_year-1), curr_year, paste0("vs ", curr_year-1))

VFE_data_table1=tableGrob(VFE_data_table[1], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table2=tableGrob(VFE_data_table[2], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table3=tableGrob(VFE_data_table[3], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_data_table_colours_climb)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table4=tableGrob(VFE_data_table[4], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table5=tableGrob(VFE_data_table[5], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_data_table_colours_descent)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))

VFE_data_table=gtable_combine(VFE_data_table1, VFE_data_table2, VFE_data_table3, VFE_data_table4, VFE_data_table5)
VFE_data_table$widths = unit(c(0.5, 0.125, 0.125, 0.125, 0.125), "npc")



# Heading

Header3=data.frame(Text="", Text2="")
names(Header3)=c(paste0("Traffic and time flown level by airport (", State_curr, " (", 
                        ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], "))"))), "")

Header_table3=tableGrob(Header3, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table3$widths = unit(c(0.95, 0.05), "npc")



# Share of arrivals per airport

Top_N=6
VFE_Flights_data_APT=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-4, curr_year) & MONTH_NUM<=curr_month-1) %>% 
  group_by(Airport.Name, YEAR) %>% 
  summarise(Con_fl_CDO=sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE),
            Con_fl_CCO=sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE),
            Con_fl=max(Con_fl_CDO, Con_fl_CCO),
            Tot_time_lvl_CDO=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm=TRUE),
            Tot_time_lvl_CCO=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm=TRUE))
VFE_Top_flights_APT=filter(VFE_Flights_data_APT, YEAR==curr_year) %>% 
  arrange(-Con_fl) %>%
  ungroup() %>% 
  select(-Con_fl_CDO, -Con_fl_CCO, -Tot_time_lvl_CDO, -Tot_time_lvl_CCO, -YEAR) %>% 
  mutate(Share=as.numeric(Con_fl/sum(Con_fl))) %>% 
  head(Top_N) %>% 
  as.data.frame()

VFE_Top_flights_APT[nrow(VFE_Top_flights_APT)+1,]=c("Other", sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Con_fl)-
                                                      sum(VFE_Top_flights_APT$Con_fl), 
                                                    1-sum(VFE_Top_flights_APT$Con_fl)/sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Con_fl))
VFE_Top_flights_APT=mutate(VFE_Top_flights_APT,
                           Airport.Name=factor(Airport.Name, levels = rev(VFE_Top_flights_APT$Airport.Name)),
                           Con_fl=as.numeric(Con_fl),
                           Share=as.numeric(Share))

VFE_Top_flights_APT_bar_plot=ggplot(VFE_Top_flights_APT, aes(x=Airport.Name, y=Share*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(Airport.Name, Share*100+2, label=paste0(format(round(Share*100, 1), nsmall=1), "%"), fill=NULL))+
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
add_logo(plot_name = VFE_Top_flights_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/VFE_Top_flights_APT_bar.png"))


# Share of level flight per airport

VFE_Top_lvl_flight_APT=filter(VFE_Flights_data_APT, YEAR==curr_year) %>% 
  arrange(-Tot_time_lvl_CDO) %>%
  ungroup() %>% 
  select(-Con_fl_CDO, -Con_fl_CCO, -YEAR, -Con_fl) %>% 
  mutate(Share_CDO=as.numeric(Tot_time_lvl_CDO/sum(Tot_time_lvl_CDO)),
         Share_CCO=as.numeric(Tot_time_lvl_CCO/sum(Tot_time_lvl_CCO))) %>% 
  head(Top_N)

VFE_Top_lvl_flight_APT[nrow(VFE_Top_lvl_flight_APT)+1,]=c("Other", 
                                                          sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CDO)-
                                                            sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CDO), 
                                                          sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CCO)-
                                                            sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CCO), 
                                                          1-sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CDO)/
                                                            sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CDO), 
                                                          1-sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CCO)/
                                                            sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CCO))
VFE_Top_lvl_flight_APT=mutate(VFE_Top_lvl_flight_APT,
                              Airport.Name=factor(Airport.Name, levels = rev(VFE_Top_lvl_flight_APT$Airport.Name)),
                              Descent=as.numeric(Share_CDO),
                              Climb=as.numeric(Share_CCO)) %>% 
  select(-Tot_time_lvl_CCO, -Tot_time_lvl_CDO, -Share_CDO, -Share_CCO) %>% 
  melt(id.vars=c("Airport.Name")) %>% 
  mutate(variable=factor(variable, levels = c("Climb", "Descent")))

VFE_Top_lvl_flight_APT_bar_plot=ggplot(VFE_Top_lvl_flight_APT, aes(x=Airport.Name, y=value*100, fill=variable)) + 
  geom_bar(stat = "identity", position="dodge")  +
  geom_text(aes(y=value*100+2, label=paste0(format(round(value*100, 1), nsmall=1), "%")), 
            position = position_dodge(width=0.9)) +
  theme_pru() +
  scale_fill_manual(values=c("green", "blue")) +
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
  labs(x="", y="", title = paste0("% of level flight ", State_curr, " (", 
                                  ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1])), " ", curr_year, ")")) +
  coord_flip()
add_logo(plot_name = VFE_Top_lvl_flight_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/VFE_Top_lvl_flight_APT_bar.png"))


# Average time flown level per flight for top N airports

VFE_data_timeline=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-4, curr_year) & 
                           MONTH_NUM<=curr_month-1 & Airport.Name %in% VFE_Top_flights_APT$Airport.Name) %>% 
  group_by(Airport.Name, YEAR) %>% 
  summarise(Descent=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60,
            Climb=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60) %>% 
  melt(id.vars=c("Airport.Name", "YEAR")) %>% 
  mutate(variable=factor(variable, levels = c("Climb", "Descent")))

Avg_time_lvl_airport_bar_plot=ggplot(VFE_data_timeline, aes(x=YEAR, y=value, fill=variable)) + 
  geom_bar(stat = "identity", position="dodge")  +
  geom_text(aes(y=value+0.14, label=format(round(value, 1), nsmall=1)), angle=90, colour="black", 
            position = position_dodge(width=0.9), vjust=0.5) +
  facet_grid(.~Airport.Name, scales = "free_x", switch="x") +
  theme_pru() +
  scale_fill_manual(values=c("green", "blue")) +
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
  labs(x="", y="Avg. time flown level per flight (min)\n", 
       title = paste0("Average time flown level per flight by airport (", State_curr, " - Jan-", 
                      month.abb[curr_month-1], " ", curr_year, ")"))
add_logo(plot_name = Avg_time_lvl_airport_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_time_lvl_airport_bar.png"))




# Overview

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 7, 7),
             c(8, 8, 8, 8),
             c(9, 9, 10, 10),
             c(11, 11, 11, 11))
g=arrangeGrob(Header_table,
              Avg_time_lvl_flight_plot,
              Share_lvl_time_pie_plot,
              Med_CDO_CCO_alt_plot,
              Share_unimpeded_flights_pie_plot,
              Header_table2,
              VFE_data_table,
              Header_table3,
              VFE_Top_flights_APT_bar_plot,
              VFE_Top_lvl_flight_APT_bar_plot,
              Avg_time_lvl_airport_bar_plot,
              as.table=TRUE,
              nrow=7,
              ncol=4,
              heights=c(0.05, 0.15, 0.05, 0.1, 0.05, 0.2, 0.4),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/CDO_CCO_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)

# Climb and descent


# Average time flown level per flight

Avg_time_lvl_flight_CDO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60
Avg_time_lvl_flight_CCO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60


Avg_time_lvl_flight_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=-0.4, ymax=0), fill="green") +
  annotate("text", x=0, y=0.2, label=paste0("Descent: ", format(round(Avg_time_lvl_flight_CDO, 1), nsmall=1)), colour="grey", size=5) +
  annotate("text", x=0, y=-0.2, label=paste0("Climb: ", format(round(Avg_time_lvl_flight_CCO, 1), nsmall=1)), colour="grey50", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Avg. time flown level\nper flight (min)")
add_logo(plot_name = Avg_time_lvl_flight_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_time_lvl_flight.png"))



# Share of level time

source("Multiple_scales_functions.R")

Share_lvl_time_CDO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, YEAR == curr_year & MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)
Share_lvl_time_CCO=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, YEAR == curr_year & MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)

Share_lvl_time_CDO_pie=data.frame(Area=c(State_curr, "EUROCONTROL"),
                                  Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE),
                                        sum(filter(CDO_CCO_data, YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)-
                                          sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                       MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_lvl_time_CCO_pie=data.frame(Area=c(State_curr, "EUROCONTROL"),
                                  Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE),
                                        sum(filter(CDO_CCO_data, YEAR == curr_year & 
                                                     MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)-
                                          sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                       MONTH_NUM<=curr_month-1)$TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_lvl_time_pie_plot=ggplot() + 
  geom_arc_bar(data=Share_lvl_time_CDO_pie, aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                                                start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "blue")) +
  new_scale_fill() +
  geom_arc_bar(data=Share_lvl_time_CCO_pie, aes(x0 = 0, y0 = 0, r0 = 1, r = 1.4,
                                                start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "green")) +
  annotate("text", x=0, y=-2.2, label=paste0("CDO: ", format(round(Share_lvl_time_CDO*100, 1), nsmall=1), "%\n",
                                             "CCO: ", format(round(Share_lvl_time_CCO*100, 1), nsmall=1), "%"), size=3) +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2.8, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Share of level time with\nrespect to the\nEUROCONTROL total")
add_logo(plot_name = Share_lvl_time_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_lvl_time_pie.png"))



# Median CDO/CCO altitude

CDO_results_per_flight_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CDO_results_per_flight_', curr_year))
CCO_results_per_flight_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CCO_results_per_flight_', curr_year))
Flight_data_CDO_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CDO_', curr_year))
Flight_data_CCO_curr_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CCO_', curr_year))
CDO_data=left_join(select(CDO_results_per_flight_curr_year, SAM_ID, CDO_ALT), select(Flight_data_CDO_curr_year, SAM_ID, ADES_FILED)) %>% 
  filter(substr(ADES_FILED, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CDO_ALT)
Med_CDO_alt=median(CDO_data$CDO_ALT)
CCO_data=left_join(select(CCO_results_per_flight_curr_year, SAM_ID, CCO_ALT), select(Flight_data_CCO_curr_year, SAM_ID, ADEP)) %>% 
  filter(substr(ADEP, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CCO_ALT)
Med_CCO_alt=median(CCO_data$CCO_ALT)


Med_CDO_CCO_alt_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=-0.4, ymax=0), fill="green") +
  annotate("text", x=0, y=0.2, label=paste0("Descent: ", format(round(Med_CDO_alt*100, 0), nsmall=0)), colour="grey", size=5) +
  annotate("text", x=0, y=-0.2, label=paste0("Climb: ", format(round(Med_CCO_alt*100, 0), nsmall=0)), colour="black", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Median CDO/CCO\naltitude (feet)")
add_logo(plot_name = Med_CDO_CCO_alt_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Med_CDO_CCO_alt.png"))



# Share of unimpeded flights

Share_CDO_flights=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                               MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)
Share_CCO_flights=sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                               MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE)/
  sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)

Share_CDO_flights_pie=data.frame(Area=c("Unimpeded", "Impeded"),
                                 Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE),
                                       sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_DESCENT, na.rm = TRUE)-
                                         sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                      MONTH_NUM<=curr_month-1)$NBR_CDO_FLIGHTS, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_CCO_flights_pie=data.frame(Area=c("Unimpeded", "Impeded"),
                                 Imp=c(sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE),
                                       sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                    MONTH_NUM<=curr_month-1)$NBR_FLIGHTS_CLIMB, na.rm = TRUE)-
                                         sum(filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR == curr_year & 
                                                      MONTH_NUM<=curr_month-1)$NBR_CCO_FLIGHTS, na.rm = TRUE))) %>% 
  mutate(end = 2 * pi * cumsum(Imp)/sum(Imp)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
Share_unimpeded_flights_pie_plot=ggplot() + 
  geom_arc_bar(data=Share_CDO_flights_pie, aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                                               start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "blue")) +
  new_scale_fill() +
  geom_arc_bar(data=Share_CCO_flights_pie, aes(x0 = 0, y0 = 0, r0 = 1, r = 1.4,
                                               start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "green")) +
  annotate("text", x=0, y=-2.2, label=paste0("CDO: ", format(round(Share_CDO_flights*100, 1), nsmall=1), "%\n",
                                             "CCO: ", format(round(Share_CCO_flights*100, 1), nsmall=1), "%"), size=3) +
  theme_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2.8, 2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "% of unimpeded flights")
add_logo(plot_name = Share_unimpeded_flights_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_unimpeded_flights_pie.png"))



# Heading table

Header2=data.frame(Text="", Text2="", Text3="")
names(Header2)=c("", "Climb", "Descent")

Header_table2=tableGrob(Header2, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table2$widths = unit(c(0.5, 0.25, 0.25), "npc")

# VFE table

VFE_data=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-1, curr_year) & 
                  MONTH_NUM<=curr_month-1) %>% 
  group_by(YEAR) %>% 
  summarise(Avg_lvl_time_CDO=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60,
            Share_CDO_flights=sum(NBR_CDO_FLIGHTS, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE),
            Avg_lvl_time_CCO=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60,
            Share_CCO_flights=sum(NBR_CCO_FLIGHTS, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE))
VFE_data[nrow(VFE_data)+1,]=VFE_data[2,]-VFE_data[1,]
CDO_results_per_flight_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CDO_results_per_flight_', curr_year-1))
CCO_results_per_flight_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/CCO_results_per_flight_', curr_year-1))
Flight_data_CDO_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CDO_', curr_year-1))
Flight_data_CCO_prev_year=
  readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/CDO_CCO/Results/Final/Flight_data_all_CCO_', curr_year-1))
CDO_data_prev_year=left_join(select(CDO_results_per_flight_prev_year, SAM_ID, CDO_ALT), select(Flight_data_CDO_prev_year, SAM_ID, ADES_FILED)) %>% 
  filter(substr(ADES_FILED, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CDO_ALT)
Med_CDO_alt_prev_year=median(CDO_data_prev_year$CDO_ALT)
CCO_data_prev_year=left_join(select(CCO_results_per_flight_prev_year, SAM_ID, CCO_ALT), select(Flight_data_CCO_prev_year, SAM_ID, ADEP)) %>% 
  filter(substr(ADEP, 1, 2)==filter(FAB_ANSPs, Country==State_curr)$Code) %>% 
  select(CCO_ALT)
Med_CCO_alt_prev_year=median(CCO_data_prev_year$CCO_ALT)


VFE_data_table=data.frame(Term=c("Avg. time flown level per flight (min.)",
                                 "Median CDO/CCO altitude (feet)",
                                 "Share of unimpeded flights"),
                          Curr_year_climb=c(Avg_time_lvl_flight_CCO, Med_CCO_alt*100, Share_CCO_flights*100),
                          Diff_prev_year_climb=c(filter(VFE_data, YEAR==1)$Avg_lvl_time_CCO, 
                                                 Med_CCO_alt*100-Med_CCO_alt_prev_year*100,
                                                 filter(VFE_data, YEAR==1)$Share_CCO_flights*100),
                          Curr_year_descent=c(Avg_time_lvl_flight_CDO, Med_CDO_alt*100, Share_CDO_flights*100),
                          Diff_prev_year_descent=c(filter(VFE_data, YEAR==1)$Avg_lvl_time_CDO, 
                                                   Med_CDO_alt*100-Med_CDO_alt_prev_year*100,
                                                   filter(VFE_data, YEAR==1)$Share_CDO_flights*100),
                          Perc=c("", "", "%"),
                          Digits=c(1, 0, 1)) %>% 
  mutate(Row=seq(1, 3),
         Curr_year_climb_text=paste0(format(round(Curr_year_climb, Digits), nsmall=Digits), Perc),
         Diff_prev_year_climb_text=paste0(ifelse(Diff_prev_year_climb>0, "+", ifelse(round(Diff_prev_year_climb, Digits)==0 & 
                                                                                       Diff_prev_year_climb<0, "-", "")), 
                                          format(round(Diff_prev_year_climb, Digits), nsmall=Digits), Perc),
         Curr_year_descent_text=paste0(format(round(Curr_year_descent, Digits), nsmall=Digits), Perc),
         Diff_prev_year_descent_text=paste0(ifelse(Diff_prev_year_descent>0, "+", ifelse(round(Diff_prev_year_descent, Digits)==0 & 
                                                                                           Diff_prev_year_descent<0, "-", "")), 
                                            format(round(Diff_prev_year_descent, Digits), nsmall=Digits), Perc),
         Colour_climb=ifelse(Row==1,
                             case_when(Diff_prev_year_climb>0 ~ "red", Diff_prev_year_climb<=0 ~ "green4"),
                             case_when(Diff_prev_year_climb>=0 ~ "green4", Diff_prev_year_climb<0 ~ "red")),
         Colour_descent=ifelse(Row==1,
                               case_when(Diff_prev_year_descent>0 ~ "red", Diff_prev_year_descent<=0 ~ "green4"),
                               case_when(Diff_prev_year_descent>=0 ~ "green4", Diff_prev_year_descent<0 ~ "red")))
VFE_data_table_colours_climb=VFE_data_table$Colour_climb
VFE_data_table_colours_descent=VFE_data_table$Colour_descent
VFE_data_table=dplyr::select(VFE_data_table, Term, Curr_year_climb_text, Diff_prev_year_climb_text, Curr_year_descent_text, 
                             Diff_prev_year_descent_text)
names(VFE_data_table)=c(paste0("Period (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))),
                        curr_year, paste0("vs ", curr_year-1), curr_year, paste0("vs ", curr_year-1))

VFE_data_table1=tableGrob(VFE_data_table[1], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table2=tableGrob(VFE_data_table[2], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table3=tableGrob(VFE_data_table[3], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_data_table_colours_climb)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table4=tableGrob(VFE_data_table[4], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_data_table5=tableGrob(VFE_data_table[5], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_data_table_colours_descent)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))

VFE_data_table=gtable_combine(VFE_data_table1, VFE_data_table2, VFE_data_table3, VFE_data_table4, VFE_data_table5)
VFE_data_table$widths = unit(c(0.5, 0.125, 0.125, 0.125, 0.125), "npc")



# Heading

Header3=data.frame(Text="", Text2="")
names(Header3)=c(paste0("Traffic and time flown level by airport (", State_curr, " (", 
                        ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], "))"))), "")

Header_table3=tableGrob(Header3, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table3$widths = unit(c(0.95, 0.05), "npc")



# Share of arrivals per airport

Top_N=6
VFE_Flights_data_APT=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-4, curr_year) & MONTH_NUM<=curr_month-1) %>% 
  group_by(Airport.Name, YEAR) %>% 
  summarise(Con_fl_CDO=sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE),
            Con_fl_CCO=sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE),
            Con_fl=max(Con_fl_CDO, Con_fl_CCO),
            Tot_time_lvl_CDO=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm=TRUE),
            Tot_time_lvl_CCO=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm=TRUE))
VFE_Top_flights_APT=filter(VFE_Flights_data_APT, YEAR==curr_year) %>% 
  arrange(-Con_fl) %>%
  ungroup() %>% 
  select(-Con_fl_CDO, -Con_fl_CCO, -Tot_time_lvl_CDO, -Tot_time_lvl_CCO, -YEAR) %>% 
  mutate(Share=as.numeric(Con_fl/sum(Con_fl))) %>% 
  head(Top_N) %>% 
  as.data.frame()

VFE_Top_flights_APT[nrow(VFE_Top_flights_APT)+1,]=c("Other", sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Con_fl)-
                                                      sum(VFE_Top_flights_APT$Con_fl), 
                                                    1-sum(VFE_Top_flights_APT$Con_fl)/sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Con_fl))
VFE_Top_flights_APT=mutate(VFE_Top_flights_APT,
                           Airport.Name=factor(Airport.Name, levels = rev(VFE_Top_flights_APT$Airport.Name)),
                           Con_fl=as.numeric(Con_fl),
                           Share=as.numeric(Share))

VFE_Top_flights_APT_bar_plot=ggplot(VFE_Top_flights_APT, aes(x=Airport.Name, y=Share*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(Airport.Name, Share*100+2, label=paste0(format(round(Share*100, 1), nsmall=1), "%"), fill=NULL))+
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
add_logo(plot_name = VFE_Top_flights_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/VFE_Top_flights_APT_bar.png"))


# Share of level flight per airport

VFE_Top_lvl_flight_APT=filter(VFE_Flights_data_APT, YEAR==curr_year & Airport.Name %in% VFE_Top_flights_APT$Airport.Name) %>% 
  arrange(-Tot_time_lvl_CDO) %>%
  ungroup() %>% 
  select(-Con_fl_CDO, -Con_fl_CCO, -YEAR, -Con_fl) %>% 
  mutate(Share_CDO=as.numeric(Tot_time_lvl_CDO/sum(Tot_time_lvl_CDO)),
         Share_CCO=as.numeric(Tot_time_lvl_CCO/sum(Tot_time_lvl_CCO)))

VFE_Top_lvl_flight_APT[nrow(VFE_Top_lvl_flight_APT)+1,]=c("Other", 
                                                          sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CDO)-
                                                            sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CDO), 
                                                          sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CCO)-
                                                            sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CCO), 
                                                          1-sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CDO)/
                                                            sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CDO), 
                                                          1-sum(VFE_Top_lvl_flight_APT$Tot_time_lvl_CCO)/
                                                            sum(filter(VFE_Flights_data_APT, YEAR==curr_year)$Tot_time_lvl_CCO))
VFE_Top_lvl_flight_APT=mutate(VFE_Top_lvl_flight_APT,
                              Airport.Name=factor(Airport.Name, levels = rev(VFE_Top_lvl_flight_APT$Airport.Name)),
                              Descent=as.numeric(Share_CDO),
                              Climb=as.numeric(Share_CCO)) %>% 
  select(-Tot_time_lvl_CCO, -Tot_time_lvl_CDO, -Share_CDO, -Share_CCO) %>% 
  melt(id.vars=c("Airport.Name")) %>% 
  mutate(variable=factor(variable, levels = c("Climb", "Descent")),
         Airport.Name=factor(Airport.Name, levels = rev(VFE_Top_flights_APT$Airport.Name)))

VFE_Top_lvl_flight_APT_bar_plot=ggplot(VFE_Top_lvl_flight_APT, aes(x=Airport.Name, y=value*100, fill=variable)) + 
  geom_bar(stat = "identity", position="dodge")  +
  geom_text(aes(y=value*100+2, label=paste0(format(round(value*100, 1), nsmall=1), "%")), 
            position = position_dodge(width=0.9)) +
  theme_pru() +
  scale_fill_manual(values=c("green", "blue")) +
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
  labs(x="", y="", title = paste0("% of level flight ", State_curr, " (", 
                                  ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1])), " ", curr_year, ")")) +
  coord_flip()
add_logo(plot_name = VFE_Top_lvl_flight_APT_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/VFE_Top_lvl_flight_APT_bar.png"))


# Average time flown level per flight for top N airports

VFE_data_timeline=filter(CDO_CCO_data, STATE_NAME==State_curr & YEAR %in% seq(curr_year-4, curr_year) & 
                           MONTH_NUM<=curr_month-1 & Airport.Name %in% VFE_Top_flights_APT$Airport.Name) %>% 
  group_by(Airport.Name, YEAR) %>% 
  summarise(Descent=sum(TOT_TIME_LEVEL_SECONDS_DESCENT, na.rm = TRUE)/sum(NBR_FLIGHTS_DESCENT, na.rm = TRUE)/60,
            Climb=sum(TOT_TIME_LEVEL_SECONDS_CLIMB, na.rm = TRUE)/sum(NBR_FLIGHTS_CLIMB, na.rm = TRUE)/60) %>% 
  melt(id.vars=c("Airport.Name", "YEAR")) %>% 
  mutate(variable=factor(variable, levels = c("Climb", "Descent")),
         Airport.Name=factor(Airport.Name, levels = VFE_Top_flights_APT$Airport.Name))

Avg_time_lvl_airport_line_plot=ggplot(VFE_data_timeline, aes(x=YEAR, y=value, colour=variable)) + 
  geom_line(size=2)  +
  geom_text(aes(y=value+0.14, label=format(round(value, 1), nsmall=1)), angle=90, colour="black", 
            position = position_dodge(width=0.9), vjust=0.5) +
  facet_grid(.~Airport.Name, scales = "free_x", switch="x") +
  theme_pru() +
  scale_colour_manual(values=c("green", "blue")) +
  scale_y_continuous(limits=c(0, max(VFE_data_timeline$value)+0.2)) +
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
  labs(x="", y="Avg. time flown level per flight (min)\n", 
       title = paste0("Average time flown level per flight by airport (", State_curr, " - Jan-", month.abb[curr_month-1], " ", curr_year, ")"))
add_logo(plot_name = Avg_time_lvl_airport_line_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_time_lvl_airport_line.png"))




# Overview

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 7, 7),
             c(8, 8, 8, 8),
             c(9, 9, 10, 10),
             c(11, 11, 11, 11))
g=arrangeGrob(Header_table,
              Avg_time_lvl_flight_plot,
              Share_lvl_time_pie_plot,
              Med_CDO_CCO_alt_plot,
              Share_unimpeded_flights_pie_plot,
              Header_table2,
              VFE_data_table,
              Header_table3,
              VFE_Top_flights_APT_bar_plot,
              VFE_Top_lvl_flight_APT_bar_plot,
              Avg_time_lvl_airport_bar_plot,
              as.table=TRUE,
              nrow=7,
              ncol=4,
              heights=c(0.05, 0.15, 0.05, 0.1, 0.05, 0.2, 0.4),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/CDO_CCO_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)


