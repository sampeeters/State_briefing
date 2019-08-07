library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)
library(openxlsx)


all=read.xlsx("https://coll.eurocontrol.int/sites/pru/dashboard/Data/Horizontal_Flight_Efficiency.xlsx", sheet="DATA") %>% 
  mutate(YEAR = as.integer(YEAR),
         MONTH_NUM = as.integer(MONTH_NUM),
         MONTH_MON = as.character(MONTH_MON),
         ENTRY_DATE = as.Date(ENTRY_DATE, origin="1899-12-30", tz="UTC"),
         ENTITY_NAME = as.character(ENTITY_NAME),
         ENTITY_TYPE = as.character(ENTITY_TYPE),
         TYPE_MODEL = as.character(TYPE_MODEL),
         DIST_FLOWN_KM_1 = as.integer(DIST_FLOWN_KM_1),
         DIST_DIRECT_KM = as.integer(DIST_DIRECT_KM),
         DIST_ACHIEVED_KM_1 = as.integer(DIST_ACHIEVED_KM_1),
  )

# rename 
all <- all %>% rename(
  date = ENTRY_DATE,
  yyyy = YEAR, 
  mm = MONTH_NUM,
  entity_name = ENTITY_NAME,
  entity_type = ENTITY_TYPE,
  type_model = TYPE_MODEL,
  dist_flown_km = DIST_FLOWN_KM_1,
  dist_direct_km = DIST_DIRECT_KM,
  dist_achieved_km = DIST_ACHIEVED_KM_1) %>%
  mutate(dd = day(date)) %>%
  select(-MONTH_MON) %>%
  select(date, yyyy, mm, dd, everything()) 

# all$dist_direct_km <- as.integer(all$dist_direct_km)
# all$dist_achieved_km <- as.integer(all$dist_achieved_km)