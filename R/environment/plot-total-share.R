add_total_share <- function(year_input, model){

# compute indicators
all <- all %>%
  mutate(additional_flown_km = (dist_flown_km - dist_achieved_km))

# countries
all_countries <- all %>%
  filter(entity_type == "State (FIR)") %>%
  filter(type_model == model) %>% 
  filter(entity_name != "Morocco" & entity_name != "Spain (Canarias)" & entity_name != "Spain (Continental)") %>%
  filter(yyyy == year_input) %>% 
  group_by(entity_name, yyyy) %>%
  summarise(additional_flown_km = sum(additional_flown_km)) 

total <- rep(sum(all_countries$additional_flown_km), nrow(all_countries))

all_countries <- data.frame(all_countries, total)

all_countries <- all_countries %>%
  mutate(total_share = (additional_flown_km / total)*100)

g <- ggplot(all_countries, aes(x = entity_name, y = total_share))

add_tot_share <- g +
  geom_bar(stat = "identity", fill = "#35978f", alpha = 0.8) +
  geom_text(aes(label = paste0(round(total_share, 1),"%")),
            position = position_dodge(0.9),
            vjust = 0.4, hjust = -0.1, angle = 90, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%", accuracy = 1)) +
  coord_cartesian(ylim=c(0,25)) +
  labs(
    x = "",
    y = "Share of total add. km",
    title = "",
    subtitle = "") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

add_tot_share
}
