
# En-route vertical flight efficiency

AIRAC_cycle=as.data.frame(c("1505", "1506", "1507", "1508", "1509", "1510", "1511", "1512", "1513",
                            "1601", "1602", "1603", "1604", "1605", "1606", "1607", "1608", "1609", "1610", "1611", "1612", "1613",
                            "1701", "1702", "1703", "1704", "1705", "1706", "1707", "1708", "1709", "1710", "1711", "1712", "1713",
                            "1801", "1802", "1803", "1804", "1805", "1806", "1807", "1808", "1809", "1810", "1811", "1812", "1813",
                            "1901", "1902", "1903", "1904", "1905", "1906", "1907", "1908", "1909", "1910", "1911", "1912", "1913",
                            "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                            "2101", "2102", "2103", "2104", "2105", "2106", "2107", "2108", "2109", "2110", "2111", "2112", "2113"))
colnames(AIRAC_cycle)[1]="Cycle"
Start=seq(as.POSIXct(strptime("30-apr-2015","%d-%b-%Y", tz="UTC")),
          as.POSIXct(strptime("30-apr-2015","%d-%b-%Y", tz="UTC"))+nrow(AIRAC_cycle)*28*24*3600,
          28*24*3600)
End=Start[2:length(Start)]-24*3600
Start=Start[1:(length(Start)-1)]
RAD_list=data.frame(cbind(AIRAC_cycle, Start, End))
Percentile_bucket=1
version='v14'

AIRACs=AIRAC_cycle
Results_tot=data.frame()

for (i in AIRACs$Cycle){
  
  if (file.exists(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/En-route VFE/Results/', i, '/Results_', version, '_', 
                    Percentile_bucket))) {
    
    Results=readRDS(paste0('G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/En-route VFE/Results/', i, '/Results_', version, '_', 
                           Percentile_bucket))
    
    Res_tot=Results %>%
      arrange(-Total_VFI_overall)
    Results_tot=rbind(Results_tot, cbind(Res_tot, AIRAC=i))
    
  } else {
    AIRACs=filter(AIRACs, Cycle!=i)
  }
  
}

Results_tot_curr_year=filter(Results_tot, substr(AIRAC, 1, 2)==substr(curr_year, 3, 4))
Results_tot_prev_year=filter(Results_tot, substr(AIRAC, 1, 2)==substr(curr_year-1, 3, 4) & 
                               substr(AIRAC, 3, 4) %in% unique(substr(Results_tot_curr_year$AIRAC, 3, 4)))
Results_state_curr_year=mutate(Results_tot_curr_year, ADEP=substr(APs, 1, 4), ADES=substr(APs, 6, 9)) %>% 
  filter((substr(ADEP, 1, 2)==Country_code | substr(ADES, 1, 2)==Country_code))


# Heading key figures VFE en-route

Header_VFE_ENR=data.frame(Text="", Text2="")
names(Header_VFE_ENR)=c(paste0("Key figures (AIRAC cycles ", min(as.numeric(as.character(Results_tot_curr_year$AIRAC))), " - ", 
                               max(as.numeric(as.character(Results_tot_curr_year$AIRAC))), ")"), "")

Header_VFE_ENR_table=tableGrob(Header_VFE_ENR, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                                            bg_params=list(fill="#3399cc")),
                                                                               core=list(fg_params=list(hjust=0, x=0.02, col="white", 
                                                                                                        fontsize=0.1))))
Header_VFE_ENR_table$widths = unit(c(0.95, 0.05), "npc")



# Total VFI for airport pairs (partially) in the state

Tot_VFI_state_curr_year=sum(Results_state_curr_year$Total_VFI_overall)*100

Tot_VFI_state_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, label=paste0(format(round(Tot_VFI_state_curr_year/1000000, 0), nsmall=0), "M feet"), colour="grey", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Total en-route vertical\nflight inefficiency (feet)")
add_logo(plot_name = Tot_VFI_state_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Tot_VFI_state_curr_year.png"))


# Share of total VFI
Tot_VFI_ECTL_curr_year=sum(Results_tot_curr_year$Total_VFI_overall)*100
Share_total_VFI=Tot_VFI_state_curr_year/Tot_VFI_ECTL_curr_year

Share_total_VFI_pie=data.frame(Area=c(State_curr, "EUROCONTROL"),
                                  VFI=c(Tot_VFI_state_curr_year, Tot_VFI_ECTL_curr_year-Tot_VFI_state_curr_year)) %>% 
  mutate(end = 2 * pi * cumsum(VFI)/sum(VFI)+pi/4,
         start = lag(end, default = pi/4),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

Share_total_VFI_pie_plot=ggplot() + 
  geom_arc_bar(data=Share_total_VFI_pie, aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                                                start = start, end = end, fill = Area)) +
  scale_fill_manual(values=c("lightgrey", "blue")) +
  annotate("text", x=0, y=0, label=paste0(format(round(Share_total_VFI*100, 1), nsmall=1), "%"), size=3) +
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
  labs(title = "Share of en-route vertical flight\ninefficiency with respect to the\nEUROCONTROL total")
add_logo(plot_name = Share_total_VFI_pie_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Share_total_VFI_pie.png"))



# Average VFI for airport pairs (partially) in the state

Avg_VFI_state_curr_year=Tot_VFI_state_curr_year/sum(Results_state_curr_year$Nbr_mvmts_analysed)/0.8

Avg_VFI_state_plot=ggplot() + 
  geom_rect(mapping=aes(xmin=-3, xmax=3, ymin=0, ymax=0.4), fill="blue") +
  annotate("text", x=0, y=0.2, label=paste0(format(round(Avg_VFI_state_curr_year, 0), nsmall=0), " feet"), colour="grey", size=5) +
  scale_fill_pru() +
  theme_pru() +
  theme_void()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.title = element_text(size=10)) +
  labs(title = "Average en-route vertical flight\ninefficiency per flight (feet)")
add_logo(plot_name = Avg_VFI_state_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Avg_VFI_state.png"))


# Heading results table

Header_VFE_ENR2=data.frame(Text="", Text2="", Text3="")
names(Header_VFE_ENR2)=c("", State_curr, "EUROCONTROL")

Header_VFE_ENR_table2=tableGrob(Header_VFE_ENR2, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                                              bg_params=list(fill="#3399cc")),
                                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", 
                                                                                                          fontsize=0.1))))
Header_VFE_ENR_table2$widths = unit(c(0.5, 0.25, 0.25), "npc")



# VFE ENR table

Avg_VFI_ECTL_curr_year=Tot_VFI_ECTL_curr_year/sum(Results_tot_curr_year$Nbr_mvmts_analysed)/0.8

Results_state_prev_year=mutate(Results_tot, ADEP=substr(APs, 1, 4), ADES=substr(APs, 6, 9)) %>% 
  filter((substr(ADEP, 1, 2)==Country_code | substr(ADES, 1, 2)==Country_code) & substr(AIRAC, 1, 2)==substr(curr_year-1, 3, 4))
Tot_VFI_state_prev_year=sum(Results_state_prev_year$Total_VFI_overall)*100
Avg_VFI_state_prev_year=Tot_VFI_state_prev_year/sum(Results_state_prev_year$Nbr_mvmts_analysed)/0.8
Tot_VFI_ECTL_prev_year=sum(Results_tot_prev_year$Total_VFI_overall)*100
Avg_VFI_ECTL_prev_year=Tot_VFI_ECTL_prev_year/sum(Results_tot_prev_year$Nbr_mvmts_analysed)/0.8

VFE_ENR_data_table=data.frame(Term=c("Total vertical flight inefficiency (Million feet)",
                                 "Average vertical flight inefficiency (feet/flight)"),
                          Curr_year_state=c(Tot_VFI_state_curr_year/1000000, Avg_VFI_state_curr_year),
                          Diff_prev_year_state=c((Tot_VFI_state_curr_year-Tot_VFI_state_prev_year)/1000000, 
                                                 Avg_VFI_state_curr_year-Avg_VFI_state_prev_year),
                          Curr_year_ECTL=c(Tot_VFI_ECTL_curr_year/1000000, Avg_VFI_ECTL_curr_year),
                          Diff_prev_year_ECTL=c((Tot_VFI_ECTL_curr_year-Tot_VFI_ECTL_prev_year)/1000000, 
                                                Avg_VFI_ECTL_curr_year-Avg_VFI_ECTL_prev_year),
                          Perc=c("M", ""),
                          Digits=c(0, 0)) %>% 
  mutate(Row=seq(1, 2),
         Curr_year_state_text=paste0(format(round(Curr_year_state, Digits), nsmall=Digits), Perc),
         Diff_prev_year_state_text=paste0(ifelse(Diff_prev_year_state>0, "+", ifelse(round(Diff_prev_year_state, Digits)==0 & 
                                                                                       Diff_prev_year_state<0, "-", "")), 
                                          format(round(Diff_prev_year_state, Digits), nsmall=Digits), Perc),
         Curr_year_ECTL_text=paste0(format(round(Curr_year_ECTL, Digits), nsmall=Digits), Perc),
         Diff_prev_year_ECTL_text=paste0(ifelse(Diff_prev_year_ECTL>0, "+", ifelse(round(Diff_prev_year_ECTL, Digits)==0 & 
                                                                                     Diff_prev_year_ECTL<0, "-", "")), 
                                            format(round(Diff_prev_year_ECTL, Digits), nsmall=Digits), Perc),
         Colour_state=ifelse(Row==1,
                             case_when(Diff_prev_year_state>0 ~ "red", Diff_prev_year_state<=0 ~ "green4"),
                             case_when(Diff_prev_year_state>=0 ~ "green4", Diff_prev_year_state<0 ~ "red")),
         Colour_ECTL=ifelse(Row==1,
                               case_when(Diff_prev_year_ECTL>0 ~ "red", Diff_prev_year_ECTL<=0 ~ "green4"),
                               case_when(Diff_prev_year_ECTL>=0 ~ "green4", Diff_prev_year_ECTL<0 ~ "red")))
VFE_ENR_data_table_colours_climb=VFE_ENR_data_table$Colour_state
VFE_ENR_data_table_colours_descent=VFE_ENR_data_table$Colour_ECTL
VFE_ENR_data_table=dplyr::select(VFE_ENR_data_table, Term, Curr_year_state_text, Diff_prev_year_state_text, Curr_year_ECTL_text, 
                                 Diff_prev_year_ECTL_text)
names(VFE_ENR_data_table)=c(paste0("AIRAC cycles ", substr(min(as.numeric(as.character(Results_tot_curr_year$AIRAC))), 3, 4), "-", 
                                   substr(max(as.numeric(as.character(Results_tot_curr_year$AIRAC))), 3, 4)),
                        curr_year, paste0("vs ", curr_year-1), curr_year, paste0("vs ", curr_year-1))

VFE_ENR_data_table1=tableGrob(VFE_ENR_data_table[1], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_ENR_data_table2=tableGrob(VFE_ENR_data_table[2], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_ENR_data_table3=tableGrob(VFE_ENR_data_table[3], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_ENR_data_table_colours_climb)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_ENR_data_table4=tableGrob(VFE_ENR_data_table[4], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))
VFE_ENR_data_table5=tableGrob(VFE_ENR_data_table[5], rows=NULL, 
                          theme=ttheme_minimal(core=list(fg_params=list(hjust=0, x=0.02, fontsize=10,
                                                                        col=VFE_ENR_data_table_colours_descent)),
                                               rowhead=list(fg_params=list(hjust=0, x=0.02)),
                                               colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                            bg_params=list(fill="#3399cc"))))

VFE_ENR_data_table=gtable_combine(VFE_ENR_data_table1, VFE_ENR_data_table2, VFE_ENR_data_table3, VFE_ENR_data_table4, VFE_ENR_data_table5)
VFE_ENR_data_table$widths = unit(c(0.5, 0.125, 0.125, 0.125, 0.125), "npc")



# Heading

Header3=data.frame(Text="", Text2="")
names(Header3)=c(paste0("Top airport pairs (AIRAC cycles ", 
                        min(as.numeric(as.character(Results_tot_curr_year$AIRAC))), " - ", 
                        max(as.numeric(as.character(Results_tot_curr_year$AIRAC))), ")"), "")

Header_table3=tableGrob(Header3, rows=NULL, theme=ttheme_minimal(colhead=list(fg_params=list(hjust=0, x=0.02, col="#eeece1"),
                                                                              bg_params=list(fill="#3399cc")),
                                                                 core=list(fg_params=list(hjust=0, x=0.02, col="white", fontsize=0.1))))
Header_table3$widths = unit(c(0.95, 0.05), "npc")



# Top N airport pairs in terms of total VFI

Tot_VFI_AP=group_by(Results_state_curr_year, APs) %>% 
  summarise(Tot_VFI=sum(Total_VFI_overall),
            Avg_VFI=sum(Total_VFI_overall)/sum(Nbr_mvmts_analysed)/0.8)
Top_N_tot_VFI=arrange(Tot_VFI_AP, -Tot_VFI) %>%
  head(Top_N)
Top_N_tot_VFI$APs=factor(Top_N_tot_VFI$APs, levels = rev(Top_N_tot_VFI$APs))

ENR_VFE_Tot_VFI_bar_plot=ggplot(Top_N_tot_VFI, aes(x=APs, y=Tot_VFI*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(APs, Tot_VFI*100+2, label=format(round(Tot_VFI*100, 0), nsmall=0), fill=NULL))+
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
  labs(x="", y="", title = "Top airport pairs in terms of\ntotal vertical flight inefficiency") +
  coord_flip()
add_logo(plot_name = ENR_VFE_Tot_VFI_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_VFE_Tot_VFI_bar.png"))


# Top N airport pairs in terms of average VFI per flight

Top_N_avg_VFI=arrange(Tot_VFI_AP, -Avg_VFI) %>%
  head(Top_N)
Top_N_avg_VFI$APs=factor(Top_N_avg_VFI$APs, levels = rev(Top_N_avg_VFI$APs))

ENR_VFE_Avg_VFI_bar_plot=ggplot(Top_N_avg_VFI, aes(x=APs, y=Avg_VFI*100)) + 
  geom_bar(stat = "identity", fill="blue")  +
  geom_text(aes(APs, Avg_VFI*100+2, label=format(round(Avg_VFI*100, 0), nsmall=0), fill=NULL))+
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
  labs(x="", y="", title = "Top airport pairs in terms of\naverage vertical flight inefficiency") +
  coord_flip()
add_logo(plot_name = ENR_VFE_Avg_VFI_bar_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/ENR_VFE_Avg_VFI_bar.png"))



# Time evolution for Top_N in terms of total VFI

Top_N_time=filter(Results_tot, APs %in% Top_N_tot_VFI$APs) %>% 
  left_join(RAD_list, by=c("AIRAC"="Cycle"))

Top_N_time_line_plot=ggplot(Top_N_time, aes(x=Start, y=Total_VFI_overall, colour=APs)) + 
  geom_line(size=2)  +
  theme_pru() +
  scale_y_continuous(limits=c(0, max(Top_N_time$Total_VFI_overall)+1000)) +
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
  labs(x="", y="Total VFI (feet)\n", 
       title = paste0("Total vertical flight inefficiency (AIRAC cycles ", min(as.numeric(as.character(Results_tot$AIRAC))), " - ", 
                      max(as.numeric(as.character(Results_tot$AIRAC))), ")"))
add_logo(plot_name = Top_N_time_line_plot,
         source = "Source: PRU analysis",
         width_pixels = 640,
         height_pixels = 450,
         save_filepath = paste0(dir, "Figures/", State_curr, "/Top_N_time_line.png"))



# Overview

lay <- rbind(c(1, 1, 1, 1),
             c(2, 3, 4, 5),
             c(6, 6, 6, 6),
             c(7, 7, 7, 7),
             c(8, 8, 8, 8),
             c(9, 9, 10, 10),
             c(11, 11, 11, 11))
g=arrangeGrob(Header_VFE_ENR_table,
              Tot_VFI_state_plot,
              Share_total_VFI_pie_plot,
              Avg_VFI_state_plot,
              Avg_VFI_state_plot,
              Header_VFE_ENR_table2,
              VFE_ENR_data_table,
              Header_table3,
              ENR_VFE_Tot_VFI_bar_plot,
              ENR_VFE_Avg_VFI_bar_plot,
              Top_N_time_line_plot,
              as.table=TRUE,
              nrow=7,
              ncol=4,
              heights=c(0.05, 0.15, 0.05, 0.1, 0.05, 0.2, 0.4),
              widths = c(0.25, 0.25, 0.25, 0.25),
              layout_matrix = lay)
ggsave(paste0(dir, "Figures/", State_curr, "/ENR_VFE_overview.png"), plot=g, width = 20, height = 25, units = "cm", dpi=200)

