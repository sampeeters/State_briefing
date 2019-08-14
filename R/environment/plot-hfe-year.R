all1 <- all %>%
    filter(entity_name == state_under_review) %>% 
    mutate(eff = ((dist_flown_km / dist_achieved_km) -1)*100) %>%
    select(yyyy, type_model, eff) %>%
    group_by(type_model, yyyy) %>%
    summarise(tot_eff = sum(eff)) %>%
    mutate(ef = tot_eff/100,
           perc = round(100 - ef, 2)) %>%
    filter(yyyy >= year_input_start & yyyy <= year_input) 
  
  all1$yyyy <- as.character(all1$yyyy)
    
  
  g=ggplot(all1, aes(x = yyyy, y = perc, group = type_model)) + 
    geom_line(aes(color = type_model), size = 2, alpha = 0.6) +
    geom_text(aes(label = paste0(round(perc, 1),"%")), size = 3, hjust = 1.3, angle = 90) +
    theme_pru() +
    scale_color_pru() +
    scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%", accuracy = 1),
                       expand = c(0,0)) +
    coord_cartesian(ylim=c((min(all1$perc) - 2), (max(all1$perc) + 2))) +
    scale_color_manual("", 
                       labels = c("Actual trajectory", "Flight plan", "Shortest constrained route"), 
                       values = c("#b2182b","#2166ac", "#450000")) +
    labs(y = "Efficiency", x= "") +
    theme(legend.position="bottom")
  add_logo(plot_name = g,
           source = "Source: PRU analysis",
           width_pixels = 640,
           height_pixels = 450,
           save_filepath = paste0(dir, "Figures/HFE_year_", state_under_review, ".png"))
  rm(all1)
  