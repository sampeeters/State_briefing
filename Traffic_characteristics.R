
# TRAFFIC DATA

Tfc_data=filter(STATFOR_data, YEAR %in% c(curr_year-1, curr_year-2) & TZ_NAME==State_curr) %>% 
  group_by(YEAR) %>% 
  summarise(Nbr_flights=sum(FLIGHT)) %>% 
  mutate(YEAR=as.numeric(YEAR),
         Nbr_days_year=365 + leap.year(YEAR),
         Avg_daily_flights=Nbr_flights/Nbr_days_year) %>% 
  arrange(YEAR)

Tfc_gr_Prev_year=(Tfc_data$Avg_daily_flights[2]/Tfc_data$Avg_daily_flights[1]-1)*100
Forecast_gr_curr_year_base=0
Forecast_gr_curr_year_high=0
Forecast_gr_curr_year_low=0
Avg_gr_rate_10y=0
Avg_gr_rate_5y=0
Forecast_avg_gr_rate=0

Traffic_evolution_state=data.frame(Term=c(paste0("Traffic growth ", curr_year-1, " (full year)"),
                                          paste0("Forecast growth ", curr_year, " (STATFOR ", STATFOR_version, ")"),
                                          "",
                                          "",
                                          paste0("Average annual growth rate (", STATFOR_start_year, "-", curr_year, ")"),
                                          paste0("Average annual growth rate (", curr_year-5, "-", curr_year, ")"),
                                          paste0("Forecast annual average growth rate (", curr_year, "-", curr_year+6, ")")),
                                   Forecast_type=c("", "Baseline", "High", "Low", "", "", ""),
                                   Value=c(Tfc_gr_Prev_year,Forecast_gr_curr_year_base,Forecast_gr_curr_year_high,Forecast_gr_curr_year_low,
                                           Avg_gr_rate_10y,Avg_gr_rate_5y,Forecast_avg_gr_rate)) %>% 
  mutate(Text=paste0(ifelse(Value>0, "+", ifelse(round(Value, 1)==0 & Value<0, "-", "")), format(round(Value, 1), nsmall=1), "%"),
         Colour=case_when(Value<0 ~ "red", Value>=0 ~ "green4"))
Tfc_ev_state_colours=Traffic_evolution_state$Colour
Traffic_evolution_state=select(Traffic_evolution_state, -Value, -Colour)
names(Traffic_evolution_state)=c(paste0("Traffic evolution (", State_curr, ")"), "", "")

Traffic_evolution_state_table1=tableGrob(Traffic_evolution_state[1:2], rows=NULL, 
                                        theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                             rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                             colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                          bg_params=list(fill="#3399cc"))))
Traffic_evolution_state_table2=tableGrob(Traffic_evolution_state[3], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, 
                                                                                       fontsize=10, col=Tfc_ev_state_colours)),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Traffic_evolution_state_table=gtable_combine(Traffic_evolution_state_table1, Traffic_evolution_state_table2)
Traffic_evolution_state_table$widths = unit(c(0.73, 0.15, 0.12), "npc")



# IFR flights index

Nbr_days=group_by(PRU_FAC_data, YEAR) %>% 
  summarise(Days=n_distinct(ENTRY_DATE)) %>% 
  filter(YEAR!=curr_year) %>% 
  mutate(YEAR=as.character(YEAR))
Index_data=filter(Flight_data, TZ_NAME %in% c("ECAC", State_curr) & YEAR !=curr_year) %>% 
  group_by(YEAR, TZ_NAME) %>% 
  summarise(Tot_flights=sum(FLIGHT)) %>% 
  left_join(Nbr_days) %>% 
  mutate(Avg_flt_day=Tot_flights/Days)
Index_data=mutate(Index_data, Tfc_index=Avg_flt_day/filter(Index_data, YEAR==2008)$Avg_flt_day*100) %>% 
  ungroup() %>%
  group_by(TZ_NAME) %>% 
  arrange(TZ_NAME, YEAR) %>% 
  mutate(Annual_growth=Avg_flt_day/lag(Avg_flt_day)-1) %>% 
  ungroup() %>% 
  mutate(TZ_NAME=paste0(TZ_NAME, " Index (2008)"))

Index_data_plot=ggplot(Index_data) + 
  geom_line(aes(x=YEAR, y=Tfc_index, colour=TZ_NAME, group=TZ_NAME), size=2) +
  theme_pru() +
  scale_color_pru() +
  ylim(c(80, max(Index_data$Tfc_index))) +
  labs(y="IFR flights index (base 2008)", title = "") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_blank())
add_logo(plot_name = Index_data_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Index_data.png"))


# Change of average daily flights

Change_data_plot=ggplot(filter(Index_data, TZ_NAME==paste0(State_curr, " Index (2008)"))) + 
  geom_bar(aes(x=YEAR, y=Annual_growth), fill="blue", stat = "identity")  +
  geom_text(aes(x=YEAR, y=Annual_growth+sign(Annual_growth)/200, 
                label=paste0(format(round(Annual_growth*100, 1), nsmall=1), "%"), angle=90), colour="blue") +
  theme_pru() +
  scale_color_pru() +
  # ylim(c(80, max(Index_data$Tfc_index))) +
  labs(y="% change vs previous year\n", title = "") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_blank(),
        axis.text.x = element_text(angle=90))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
add_logo(plot_name = Change_data_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Change_data.png"))


# SEASONALITY


Seasonality_days=filter(PRU_FAC_data, UNIT_NAME=="EUROCONTROL", YEAR %in% c(2008, curr_year-2, curr_year-1, curr_year)) %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(Nbr_days=n())

Tfc_data_seasonality=filter(STATFOR_data, YEAR %in% c(2008, curr_year-2, curr_year-1, curr_year) & TZ_NAME==State_curr) %>% 
  mutate(YEAR=as.integer(YEAR)) %>% 
  group_by(YEAR, MONTH_NUM, MONTH_MON) %>% 
  summarise(Nbr_flights=sum(FLIGHT)) %>% 
  left_join(Seasonality_days, by=c("YEAR", "MONTH_NUM"="MONTH")) %>% 
  mutate(Avg_daily_flights=round(Nbr_flights/Nbr_days),
         MONTH_MON=factor(MONTH_MON, levels = toupper(month.abb)))

Tfc_data_seasonality_plot=ggplot(Tfc_data_seasonality) + 
  geom_line(aes(x=MONTH_MON, y=Avg_daily_flights, colour=factor(YEAR), group=YEAR), size=2) +
  theme_pru() +
  scale_color_pru() +
  # ylim(c(4000, max(Tfc_data_seasonality$Avg_daily_flights))) +
  labs(y="Average daily flights", title = "Seasonality") +
  theme(axis.text.x = element_text(angle=90),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
add_logo(plot_name = Tfc_data_seasonality_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Tfc_data_seasonality.png"))




# TRAFFIC GROUPS

Groups=c("Overflights", "To/from", "Internal")

Tfc_groups=filter(STATFOR_data, YEAR %in% c(curr_year-1, curr_year) & TZ_NAME==State_curr & MONTH_NUM<=curr_month-1) %>% 
  mutate(DAIO2=case_when(
    DAIO== "O" ~ "Overflights",
    DAIO== "A" ~ "To/from",
    DAIO== "D" ~ "To/from",
    DAIO== "I" ~ "Internal"),
    DAIO2=factor(DAIO2, levels = Groups)) %>% 
  group_by(YEAR, TZ_NAME, DAIO2) %>% 
  summarise(Nbr_flights=sum(FLIGHT)) %>% 
  ungroup() %>% 
  group_by(TZ_NAME, DAIO2) %>% 
  arrange(TZ_NAME, DAIO2, YEAR) %>% 
  mutate(Diff_perc=(Nbr_flights-lag(Nbr_flights))/lag(Nbr_flights)*100) %>% 
  ungroup() %>% 
  group_by(TZ_NAME, YEAR) %>% 
  mutate(Share=Nbr_flights/sum(Nbr_flights)*100,
         Text=case_when(
           DAIO2=="Overflights" ~ "overflights",
           DAIO2== "To/from" ~ paste0("traffic from and to ", ifelse(State_curr=="UK", "the ", ""), State_curr),
           DAIO2== "Internal" ~ "domestic flights"),
         DAIO2=factor(DAIO2, levels = Groups))

Tot_flights_diff=(sum(filter(Tfc_groups, YEAR==curr_year)$Nbr_flights)-sum(filter(Tfc_groups, YEAR==curr_year-1)$Nbr_flights))/
  sum(filter(Tfc_groups, YEAR==curr_year-1)$Nbr_flights)*100
Overflights_diff=filter(Tfc_groups, YEAR==curr_year, DAIO2=="Overflights")$Diff_perc
To_from_diff=filter(Tfc_groups, YEAR==curr_year, DAIO2=="To/from")$Diff_perc
Internal_diff=filter(Tfc_groups, YEAR==curr_year, DAIO2=="Internal")$Diff_perc

Tfc_groups_table=data.frame(Name=c("Total flights", "Overflights", "To/from", "Internal"),
                            Curr_year=c(sum(filter(Tfc_groups, YEAR==curr_year)$Nbr_flights),
                                        filter(Tfc_groups, YEAR==curr_year, DAIO2=="Overflights")$Nbr_flights,
                                        filter(Tfc_groups, YEAR==curr_year, DAIO2=="To/from")$Nbr_flights,
                                        filter(Tfc_groups, YEAR==curr_year, DAIO2=="Internal")$Nbr_flights),
                            Vs_prev_year=c(Tot_flights_diff, Overflights_diff, To_from_diff, Internal_diff)) %>% 
  mutate(Text=paste0(ifelse(Vs_prev_year>0, "+", ifelse(round(Vs_prev_year, 1)==0 & Vs_prev_year<0, "-", "")), 
                     format(round(Vs_prev_year, 1), nsmall=1), "%"),
         Colour=case_when(Vs_prev_year<0 ~ "red", Vs_prev_year>=0 ~ "green4"))
Tfc_groups_table_colours=Tfc_groups_table$Colour
Tfc_groups_table=select(Tfc_groups_table, -Vs_prev_year, -Colour)
names(Tfc_groups_table)=c(paste0(State_curr, ifelse(curr_month==1, " (Jan)", paste0(" (Jan-", month.abb[curr_month-1], ")"))), 
                          curr_year, 
                          paste0("vs ", curr_year-1))

Tfc_groups_table1=tableGrob(Tfc_groups_table[1], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10),
                                                                        bg_params=list(fill=c("#d9d9d9", pru_pal()(3)))),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Tfc_groups_table2=tableGrob(Tfc_groups_table[2], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Tfc_groups_table3=tableGrob(Tfc_groups_table[3], rows=NULL, 
                                         theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, 
                                                                                       fontsize=10,col=Tfc_groups_table_colours)),
                                                              rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                              colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                           bg_params=list(fill="#3399cc"))))
Tfc_groups_table=gtable_combine(Tfc_groups_table1, Tfc_groups_table2, Tfc_groups_table3)
Tfc_groups_table$widths = unit(c(0.6, 0.2, 0.2), "npc")



Tfc_groups_pie=filter(Tfc_groups, YEAR==curr_year) %>%
  arrange(desc(DAIO2)) %>% 
  mutate(end = 2 * pi * cumsum(Nbr_flights)/sum(Nbr_flights),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

Tfc_groups_pie_plot=ggplot(Tfc_groups_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = DAIO2)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = paste0(format(round(Share, 1), nsmall=1), "%"),
                hjust = hjust, vjust = vjust)) +
  scale_fill_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = State_curr)
add_logo(plot_name = Tfc_groups_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Tfc_groups_pie.png"))





# TRAFFIC SEGMENTS

Segments_order=c("Scheduled", "Lowcost", "Business", "Cargo", "Non-Scheduled", "Military", "Other")
Tfc_segments=filter(STATFOR_data, YEAR %in% c(curr_year-1, curr_year) & TZ_NAME==State_curr & MONTH_NUM<=curr_month-1) %>% 
  group_by(YEAR, TZ_NAME, RULE_NAME) %>% 
  summarise(Nbr_flights=sum(FLIGHT)) %>% 
  ungroup() %>% 
  group_by(TZ_NAME, RULE_NAME) %>% 
  arrange(TZ_NAME, RULE_NAME, YEAR) %>% 
  mutate(Diff_perc=(Nbr_flights-lag(Nbr_flights))/lag(Nbr_flights)*100) %>% 
  ungroup() %>% 
  group_by(TZ_NAME, YEAR) %>% 
  mutate(Share=Nbr_flights/sum(Nbr_flights)*100,
         RULE_NAME=factor(RULE_NAME, levels=Segments_order))

Scheduled_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Scheduled")$Diff_perc
Lowcost_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Lowcost")$Diff_perc
Business_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Business")$Diff_perc
Cargo_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Cargo")$Diff_perc
Non_Scheduled_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Non-Scheduled")$Diff_perc
Military_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Military")$Diff_perc
Other_diff=filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Other")$Diff_perc

Tfc_segments_table=data.frame(Name=Segments_order,
                              Curr_year=c(filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Scheduled")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Lowcost")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Business")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Cargo")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Non-Scheduled")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Military")$Nbr_flights,
                                          filter(Tfc_segments, YEAR==curr_year, RULE_NAME=="Other")$Nbr_flights),
                              Vs_prev_year=c(Scheduled_diff, Lowcost_diff, Business_diff, Cargo_diff, Non_Scheduled_diff, 
                                             Military_diff, Other_diff)) %>% 
  mutate(Text=paste0(ifelse(Vs_prev_year>0, "+", ifelse(round(Vs_prev_year, 1)==0 & Vs_prev_year<0, "-", "")), 
                     format(round(Vs_prev_year, 1), nsmall=1), "%"),
         Colour=case_when(Vs_prev_year<0 ~ "red", Vs_prev_year>=0 ~ "green4"))
Tfc_segments_table_colours=Tfc_segments_table$Colour
Tfc_segments_table=select(Tfc_segments_table, -Vs_prev_year, -Colour)
names(Tfc_segments_table)=c(paste0(State_curr, ifelse(curr_month==1, " (Jan)", paste0(" (Jan-", month.abb[curr_month-1], ")"))), 
                            curr_year, 
                            paste0("vs ", curr_year-1))

Tfc_segments_table1=tableGrob(Tfc_segments_table[1], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10),
                                                           bg_params=list(fill=c(pru_pal()(7)))),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Tfc_segments_table2=tableGrob(Tfc_segments_table[2], rows=NULL, 
                              theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                                   rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                   colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                bg_params=list(fill="#3399cc"))))
Tfc_segments_table3=tableGrob(Tfc_segments_table[3], rows=NULL, 
                            theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10, col=Tfc_segments_table_colours)),
                                                 rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                                 colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                              bg_params=list(fill="#3399cc"))))
Tfc_segments_table=gtable_combine(Tfc_segments_table1, Tfc_segments_table2, Tfc_segments_table3)
Tfc_segments_table$widths = unit(c(0.6, 0.2, 0.2), "npc")



Tfc_segments_pie=filter(Tfc_segments, YEAR==curr_year) %>%
  arrange(desc(RULE_NAME)) %>% 
  mutate(end = 2 * pi * cumsum(Nbr_flights)/sum(Nbr_flights),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

Tfc_segments_pie_plot=ggplot(Tfc_segments_pie) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                   start = start, end = end, fill = RULE_NAME)) +
  geom_text(aes(x = 1.1 * sin(middle), y = 1.1 * cos(middle), label = paste0(format(round(Share, 1), nsmall=1), "%"),
                hjust = hjust, vjust = vjust), size=3) +
  # geom_text_repel(aes(x = 1.1 * sin(middle), y = 1.1 * cos(middle), label = paste0(format(round(Share, 1), nsmall=1), "%"),
  #                     hjust = hjust, vjust = vjust), size=3) +
  scale_fill_pru() +
  theme_void()+
  coord_fixed() +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = State_curr)
add_logo(plot_name = Tfc_segments_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Tfc_segments_pie.png"))





# OVERVIEW

g=arrangeGrob(Traffic_evolution_state_table, Index_data_plot, 
             Tfc_data_seasonality_plot, Change_data_plot,
             Tfc_groups_table, Tfc_groups_pie_plot,
             Tfc_segments_table, Tfc_segments_pie_plot,
             nrow=4, ncol=2,
             as.table=TRUE,
             heights=c(1, 1, 1, 1),
             widths = c(2, 1.5))
ggsave(paste0(dir, "Figures/", State_curr, "/Tfc_char_overview.png"), plot=g, width = 20, height = 21, units = "cm", dpi=200)

