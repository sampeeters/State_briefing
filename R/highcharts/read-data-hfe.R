library(readxl)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)

hfe <- read_xlsx("data/Horizontal_Flight_Efficiency.xlsm", sheet = "DATA", col_names = TRUE)

# rename 
all <- hfe %>% rename(
  date = ENTRY_DATE,
  yyyy = YEAR, mm = MONTH_NUM,
  entity_name = ENTITY_NAME, entity_type = ENTITY_TYPE,
  type_model = TYPE_MODEL,
  dist_flown_km = DIST_FLOWN_KM_1,
  dist_direct_km = DIST_DIRECT_KM,
  dist_achieved_km = DIST_ACHIEVED_KM_1) %>%
  mutate(
    dd = day(date)) %>%
  select(-MONTH_MON) %>%
  select(date, yyyy, mm, dd, everything()) 

all$dist_direct_km <- as.integer(all$dist_direct_km)
all$dist_achieved_km <- as.integer(all$dist_achieved_km)

########################################################################

allnew <- all %>%
  filter(entity_name == "Armenia") %>% 
  mutate(var = ((dist_flown_km / dist_achieved_km) -1)*100) %>%
  select(date, type_model, var) %>%
  mutate(perc = 100 - var) %>%
  filter(date >= "2016-01-01" & date <= "2019-03-31") %>%
  select(date, type_model, perc)

dataset <- spread(allnew, type_model, perc)
