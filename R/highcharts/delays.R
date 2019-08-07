library(highcharter)

entity_details_mm12 <- all_details %>% filter(entity_name == "Avinor") %>%
  #filter(date >= "2015-01-01" & date <= "2019-01-31") %>%
  group_by(yyyy, mm) %>%
  summarise(delay = sum(delay))

entity_summaries_mm12 <- all_summaries %>% filter(entity_name == "Avinor") %>%
  #filter(date >= "2015-01-01" & date <= "2019-01-31") %>%
  group_by(yyyy, mm) %>%
  summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), flt_dly_gt15 = sum(flights_delayed_gt15))

entity_mm12 <- inner_join(entity_details_mm12, entity_summaries_mm12, by = c("yyyy", "mm")) %>%
  mutate(date = ymd(str_c(yyyy, mm, "01", sep = "-")))


h2 <- highchart(type = "stock") %>% 
  hc_title(text = "En-route ATFM delay - Avinor") %>% 
  #hc_subtitle(text = "") %>%
  hc_add_series(name = "En-route ATFM delay",
                data = entity_mm12,
                hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = delay),
                type = "line",
                color = hex_to_rgba("red", 0.8)) %>%
  hc_yAxis(title = list(text = "En-route ATFM delay")) %>%
  hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %Y}'))
