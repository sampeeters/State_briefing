add_flown <- function(year_input, model){
  
  # compute indicators
  all <- all %>%
    mutate(additional_flown_km = (dist_flown_km - dist_achieved_km))

  # countries
  all_countries <- all %>%
    filter(entity_type == "State (FIR)") 
  
all_countries <- all_countries %>%
  filter(type_model == model) %>% 
  filter(entity_name != "Morocco" & entity_name != "Spain (Canarias)" & entity_name != "Spain (Continental)") %>%
  #mutate(entity_name = ifelse(entity_name == "Spain (Canarias)", "Spain", entity_name),
  #       entity_name = ifelse(entity_name == "Spain (Continental)", "Spain", entity_name)) %>%
  filter(yyyy == year_input) %>% 
  group_by(entity_name, yyyy) %>%
  summarise(additional_flown_km = sum(additional_flown_km)) 

g <- ggplot(all_countries, aes(x = entity_name, y = additional_flown_km))

add_flown_plot <- g +
  geom_bar(stat = "identity", fill = "#35978f", alpha = 0.8) +
  scale_y_continuous(label = unit_format(unit = "M", scale = 1e-6, sep = "")) +
  labs(
    x = "",
    y = "Additional km flown",
    title = "",
    subtitle = "") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

add_flown_plot

}