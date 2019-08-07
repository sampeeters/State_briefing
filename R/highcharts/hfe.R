library(highcharter)
library(dplyr)
library(quantmod)
library(tidyverse)

h1 <- highchart(type = "stock") %>% 
  hc_title(text = "Horizontal en-route flight efficiency") %>% 
  #hc_subtitle(text = "") %>%
  hc_add_series(name = "Actual trajectory",
                data = dataset,
                hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = CPF),
                type = "line",
                color = hex_to_rgba("red", 0.5)) %>%
  hc_add_series(name = "Flight plan",
                data = dataset,
                hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = FTFM), 
                type = "line",
                color = hex_to_rgba("blue", 0.5),
                showInNavigator = TRUE) %>%
  hc_add_series(name = "Shortest constraint route",
                data = dataset,
                hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = SCR), 
                type = "line",
                color = hex_to_rgba("green", 0.5),
                showInNavigator = TRUE) %>%
  hc_yAxis(min = 90, title = list(text = "efficiency (%)")) %>%
  hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
  hc_tooltip(valueDecimals = 2)
  # hc_rangeSelector(enabled = FALSE) %>%
  # hc_navigator(enabled = FALSE)

h1

# h2 <- hchart(sample(head(letters), size = 100, prob = 1:6, replace = TRUE))

# combine multiple charts in the same page
# library(htmltools)
# hw_grid(h1, h2) %>% browsable()

# navigator ------------------------
# The navigator is a small series below the main series, displaying a view of the entire data set.
# It provides tools to zoom in and out on parts of the data as well as panning across the dataset.

#library(htmlwidgets)

# hc <- hchart(cbind(fdeaths, mdeaths), separate = FALSE)
# saveWidget(h1, file="Norway.html")
