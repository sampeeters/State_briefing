library(stringr)
library(purrr)

# ert dly ansp

year <- "2008"
download_data <- function(year) {
  filename <- str_glue("ert_dly_ansp_{year}.csv.bz2", year = year)
  baseurl <- "https://raw.githack.com/euctrl-pru/website/master/data/csv/"
  url <- paste0(baseurl, filename)
  download.file(url, here::here("data", filename))
}

seq(2008, 2019) %>% 
  map(~ download_data(.x))
