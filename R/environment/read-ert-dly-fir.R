library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)

flights <- read.xlsx("https://coll.eurocontrol.int/sites/pru/dashboard/Data/En-Route_ATFM_Delay_FIR.xlsx", sheet = "DATA") %>%
    select(YEAR, MONTH_NUM, MONTH_MON, FLT_DATE, ENTITY_NAME, ENTITY_TYPE, FLT_ERT_1) %>%
    mutate(YEAR = as.integer(YEAR),
           MONTH_NUM = as.integer(MONTH_NUM),
           MONTH_MON = as.character(MONTH_MON),
           FLT_DATE = as.Date(FLT_DATE, origin="1899-12-30"),
           ENTITY_NAME = as.character(ENTITY_NAME),
           ENTITY_TYPE = as.character(ENTITY_TYPE),
           FLT_ERT_1 = as.integer(FLT_ERT_1)) %>% 
    rename(yyyy = YEAR,
           mm = MONTH_NUM,
           entity_name = ENTITY_NAME, 
           entity_type = ENTITY_TYPE,
           date = FLT_DATE,
           flt = FLT_ERT_1) %>%
    mutate(dd = day(date)) %>%
  select(-MONTH_MON) %>%
  select(date, yyyy, mm, dd, everything()) %>%
  filter(entity_type == "COUNTRY (FIR)")

flt <- flights %>%
  filter(entity_name != "Morocco" & entity_name != "Spain Canarias" & entity_name != "Spain Continental" &
           entity_name != "Portugal Santa Maria" & entity_name != "Portugal") %>%
  mutate(entity_name = ifelse(entity_name == "United Kingdom", "United Kingdom (Continental)", entity_name),
         entity_name = ifelse(entity_name == "North Macedonia", "Republic of North Macedonia", entity_name),
         entity_name = ifelse(entity_name == "Portugal Continental", "Portugal (Continental)", entity_name)) %>%
  filter(yyyy != "2013",
         entity_name != "UK Continental",
         entity_name != "UK Oceanic") %>%
  group_by(entity_name, yyyy)
