avg_add_flt <- function(year_input, model){

all_countries <- all %>%
  filter(entity_type == "State (FIR)") %>%
  mutate(additional_flown_km = (dist_flown_km - dist_achieved_km))

all_countries <- all_countries %>%
  filter(type_model == model) %>% 
  filter(entity_name != "Morocco" & entity_name != "Spain (Canarias)" & entity_name != "Spain (Continental)") %>%
  group_by(entity_name, yyyy) 


# Join two df ------------------------------------------------------------------
data <- left_join(all_countries, flt, by = c("date", "entity_name"))

data <- data %>%
  group_by(entity_name, yyyy.x) %>%
  summarise(additional_flown_km = sum(additional_flown_km),
            flt = sum(flt)) %>%
  mutate(avg = additional_flown_km / flt) 


data <- data %>%
  filter(yyyy.x == year_input)

g <- ggplot(data, aes(x = entity_name, y = avg))

avg_add_flt_plot <- g +
  geom_bar(stat = "identity", fill = "#d6604d", alpha = 0.8) +
  labs(
    x = "",
    y = "Avg. add. km per flight",
    title = "",
    subtitle = "") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

avg_add_flt_plot 
}