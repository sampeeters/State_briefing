
# Horizontal flight efficiency

# Heading key figures

Header=data.frame(Text="", Text2="")
names(Header)=c(paste0("Key figures (", curr_year, " - ", 
                       ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), "")

Header_table=tableGrob(Header, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                            bg_params=list(fill="#3399cc")),
                                                               core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table$widths = unit(c(0.95, 0.05), "npc")


# Share of total additional distance flown in the EUROCONTROL area

HFE_results=group_by(HFE_data, ENTITY_NAME, TYPE_MODEL, YEAR) %>% 
  filter(ENTITY_NAME %in% c("EUROCONTROL Area (PRR)", State_curr) & MONTH_NUM<=curr_month-1) %>% 
  summarise(Flights=sum(FLIGHTS),
            Dist_achieved_km=sum(DIST_ACHIEVED_KM),
            Dist_flown_km=sum(DIST_FLOWN_KM),
            Dist_direct_km=sum(DIST_DIRECT_KM)) %>% 
  arrange(ENTITY_NAME, TYPE_MODEL) %>% 
  mutate(HFE=Dist_achieved_km/Dist_flown_km,
         Avg_add_dist=(Dist_flown_km-Dist_achieved_km)/Flights,
         Tot_add_dist=Dist_flown_km-Dist_achieved_km)

Share_add_dist=filter(HFE_results, ENTITY_NAME==State_curr & YEAR == curr_year & TYPE_MODEL=="CPF")$Tot_add_dist/
  filter(HFE_results, ENTITY_NAME=="EUROCONTROL Area (PRR)" & YEAR == curr_year & TYPE_MODEL=="CPF")$Tot_add_dist

Share_add_dist_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="white") +
  annotate("text", x=0, y=0.2, label=paste0(format(round(Share_add_dist*100, 1), nsmall=1), "%"), colour="red", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "% of total add. distance flown\nin the EUROCONTROL area\nactual trajectory\n")
add_logo(plot_name = Share_add_dist_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_add_dist.png"))


# Horizontal en-route flight efficiency - actual trajectory

HFE_actual_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="red") +
  annotate("text", x=0, y=0.2, 
           label=paste0(format(round(filter(HFE_results, ENTITY_NAME==State_curr & YEAR == curr_year & TYPE_MODEL=="CPF")$HFE*100, 1), 
                               nsmall=1), "%"), colour="grey", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Horizontal en-route flight\nefficiency (%)\nactual trajectory")
add_logo(plot_name = HFE_actual_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/HFE_actual.png"))

# Horizontal en-route flight efficiency - flight plan

HFE_flight_plan_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, 
           label=paste0(format(round(filter(HFE_results, ENTITY_NAME==State_curr & YEAR == curr_year & TYPE_MODEL=="FTFM")$HFE*100, 1), 
                               nsmall=1), "%"), colour="grey", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Horizontal en-route flight\nefficiency (%)\nflight plan")
add_logo(plot_name = HFE_flight_plan_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/HFE_flight_plan.png"))

# Horizontal en-route flight efficiency - scr

HFE_scr_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="darkgreen") +
  annotate("text", x=0, y=0.2, 
           label=paste0(format(round(filter(HFE_results, ENTITY_NAME==State_curr & YEAR == curr_year & TYPE_MODEL=="SFR")$HFE*100, 1), 
                               nsmall=1), "%"), colour="grey", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Horizontal en-route flight\nefficiency (%)\nshortest constrained route")
add_logo(plot_name = HFE_scr_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/HFE_shortest_constrained_route.png"))

# Heading table

Header2=data.frame(Text="", Text2="", Text3="")
names(Header2)=c("Horizontal en-route flight efficiency", State_curr, "EUROCONTROL Area (PRR)")

Header_table2=tableGrob(Header2, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table2$widths = unit(c(0.5, 0.2, 0.3), "npc")

# HFE table

Multipl_factors=rep(c(rep(100,3), rep(1, 3), rep(1/1000000,3)), 2)
Rounding=rep(c(rep(1,6), rep(2,3)), 2)
HFE_results2=filter(HFE_results, YEAR==curr_year-1) %>% 
  select(-Flights, -Dist_achieved_km, -Dist_flown_km, -Dist_direct_km, -YEAR) %>% 
  melt(id.vars=c("ENTITY_NAME", "TYPE_MODEL")) %>% 
  cbind(YEAR=curr_year-1) %>% 
  left_join(filter(HFE_results, YEAR==curr_year) %>% 
              select(-Flights, -Dist_achieved_km, -Dist_flown_km, -Dist_direct_km) %>% 
              melt(id.vars=c("ENTITY_NAME", "TYPE_MODEL")) %>% 
              cbind(YEAR=curr_year), by=c("ENTITY_NAME", "TYPE_MODEL", "variable")) %>% 
  mutate(TYPE_MODEL=factor(TYPE_MODEL, levels=c("FTFM", "CPF", "SFR")),
         variable=factor(variable, levels=c("HFE", "Avg_add_dist", "Tot_add_dist"))) %>% 
  arrange(ENTITY_NAME, variable, TYPE_MODEL) %>% 
  mutate(MF=Multipl_factors,
         value.x=value.x*MF,
         value.y=value.y*MF,
         Diff=value.y-value.x)

HFE_data_table=data.frame(Term=c("Horizontal en-route flight efficiency",
                                 "",
                                 "",
                                 "Average additional distance (km)",
                                 "",
                                 "",
                                 "Total additional distance (million km)",
                                 "",
                                 ""),
                          Type=rep(c("Flight plan",
                                     "Actual",
                                     "Shortest"),3),
                          Curr_year_state=filter(HFE_results2, ENTITY_NAME==State_curr)$value.y,
                          Diff_prev_year_state=filter(HFE_results2, ENTITY_NAME==State_curr)$Diff,
                          Curr_year_ECTL=filter(HFE_results2, ENTITY_NAME=="EUROCONTROL Area (PRR)")$value.y,
                          Diff_prev_year_ECTL=filter(HFE_results2, ENTITY_NAME=="EUROCONTROL Area (PRR)")$Diff,
                          Perc=c("%", "%", "%", "", "", "", "M", "M", "M"),
                          Digits=c(1, 1, 1, 1, 1, 1, 2, 2, 2)) %>% 
  mutate(Row=seq(1, 9),
         Curr_year_state_text=paste0(format(round(Curr_year_state, Digits), nsmall=Digits), Perc),
         Diff_prev_year_state_text=paste0(ifelse(Diff_prev_year_state>0, "+", ifelse(round(Diff_prev_year_state, Digits)==0 & 
                                                                                       Diff_prev_year_state<0, "-", "")), 
                                          format(round(Diff_prev_year_state, Digits), nsmall=Digits), Perc),
         Curr_year_ECTL_text=paste0(format(round(Curr_year_ECTL, Digits), nsmall=Digits), Perc),
         Diff_prev_year_ECTL_text=paste0(ifelse(Diff_prev_year_ECTL>0, "+", ifelse(round(Diff_prev_year_ECTL, Digits)==0 & 
                                                                                     Diff_prev_year_ECTL<0, "-", "")), 
                                         format(round(Diff_prev_year_ECTL, Digits), nsmall=Digits), Perc),
         Colour_state=ifelse(Row<=3,
                             case_when(Diff_prev_year_state<0 ~ "red", Diff_prev_year_state>=0 ~ "green4"),
                             case_when(Diff_prev_year_state<=0 ~ "green4", Diff_prev_year_state>0 ~ "red")),
         Colour_ECTL=ifelse(Row<=3,
                            case_when(Diff_prev_year_ECTL<0 ~ "red", Diff_prev_year_ECTL>=0 ~ "green4"),
                            case_when(Diff_prev_year_ECTL<=0 ~ "green4", Diff_prev_year_ECTL>0 ~ "red")))
HFE_data_table_colours_state=HFE_data_table$Colour_state
HFE_data_table_colours_ECTL=HFE_data_table$Colour_ECTL
HFE_data_table=dplyr::select(HFE_data_table, Term, Type, Curr_year_state_text, Diff_prev_year_state_text, Curr_year_ECTL_text, Diff_prev_year_ECTL_text)
names(HFE_data_table)=c(paste0("Period (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))), "",
                        curr_year, paste0("vs ", curr_year-1), curr_year, paste0("vs ", curr_year-1))

HFE_data_table1=tableGrob(HFE_data_table[1], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
HFE_data_table2=tableGrob(HFE_data_table[2], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
HFE_data_table3=tableGrob(HFE_data_table[3], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
HFE_data_table4=tableGrob(HFE_data_table[4], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=HFE_data_table_colours_state)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
HFE_data_table5=tableGrob(HFE_data_table[5], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
HFE_data_table6=tableGrob(HFE_data_table[6], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=HFE_data_table_colours_ECTL)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))

HFE_data_table=gtable_combine(HFE_data_table1, HFE_data_table2, HFE_data_table3, HFE_data_table4, HFE_data_table5, HFE_data_table6)
HFE_data_table$widths = unit(c(0.3, 0.2, 0.125, 0.125, 0.125, 0.125), "npc")


# Time line HFE

HFE_results_month=group_by(HFE_data, TYPE_MODEL, YEAR, MONTH_MON, MONTH_NUM) %>% 
  filter(ENTITY_NAME==State_curr) %>% 
  summarise(Dist_achieved_km=sum(DIST_ACHIEVED_KM),
            Dist_flown_km=sum(DIST_FLOWN_KM),
            HFE_month=Dist_achieved_km/Dist_flown_km) %>%
  ungroup() %>% 
  mutate(TYPE_MODEL=factor(TYPE_MODEL, levels=c("FTFM", "CPF", "SFR")),
         MONTH_MON=factor(MONTH_MON, levels = toupper(month.abb))) %>% 
  arrange(TYPE_MODEL, YEAR, MONTH_MON)
HFE_res_ytd=filter(HFE_results_month, MONTH_NUM<=curr_month-1) %>% 
  group_by(TYPE_MODEL, YEAR) %>% 
  summarise(Dist_achieved_km=sum(Dist_achieved_km),
            Dist_flown_km=sum(Dist_flown_km),
            HFE_ytd=Dist_achieved_km/Dist_flown_km)
HFE_results_month2=left_join(HFE_results_month, select(HFE_res_ytd, TYPE_MODEL, YEAR, HFE_ytd)) %>% 
  mutate(HFE_ytd=ifelse(MONTH_NUM<=curr_month-1, HFE_ytd, NA),
         Type_month=case_when(
           TYPE_MODEL=="FTFM"~"HFE (plan) - monthly",
           TYPE_MODEL=="CPF"~"HFE (actual) - monthly",
           TYPE_MODEL=="SFR"~"HFE (shortest constrained) - monthly"
         ),
         Type_ytd=case_when(
           TYPE_MODEL=="FTFM"~
             paste0("Year to date (plan) - (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))),
           TYPE_MODEL=="CPF"~
             paste0("Year to date (actual) - (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")"))),
           TYPE_MODEL=="SFR"~
             paste0("Year to date (shortest) - (", ifelse(curr_month==1, "Jan)", paste0("Jan-", month.abb[curr_month-1], ")")))
         ))

HFE_plot=ggplot(HFE_results_month2) + 
  geom_line(aes(x=MONTH_MON, y=HFE_month, group=Type_month, colour=Type_month), size=2, linetype="dotted") +
  geom_line(aes(x=MONTH_MON, y=HFE_ytd, group=Type_ytd, colour=Type_ytd), size=2) +
  facet_grid(.~YEAR, scales = "free_x", switch="x") +
  theme_pru() +
  scale_fill_pru() + 
  scale_colour_manual(values=c("blue", "red", "green", "blue", "red", "green")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.margin = margin(1, 1, 1, 1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.spacing = unit(0.1, "cm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=10),
        legend.box = "vertical",
        axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x="", y="", 
       title = paste0("Horizontal en-route flight efficiency - ", State_curr))
add_logo(plot_name = HFE_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/HFE_timeline.png"))



# OVERVIEW HFE

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 7, 7),
             c(8, 8, 8, 8))
g=arrangeGrob(Header_table, 
              Share_add_dist_plot, HFE_actual_plot,
              HFE_flight_plan_plot, HFE_scr_plot,
              Header_table2,
              HFE_data_table,
              HFE_plot,
              as.table=TRUE,
              nrow=5,
              ncol=4,
              heights=c(0.05, 0.15, 0.05, 0.25, 0.45),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/HFE_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)





