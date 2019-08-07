library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)


all=read.xlsx("https://coll.eurocontrol.int/sites/pru/dashboard/Data/Airport_Arrival_ATFM_Delay.xlsx", sheet="DATA") %>% 
  mutate(MONTH_MON = as.character(MONTH_MON),
         FLT_DATE = as.Date(FLT_DATE, origin="1899-12-30"),
         APT_ICAO = as.character(APT_ICAO),
         APT_NAME = as.character(APT_NAME),
         STATE_NAME = as.character(STATE_NAME),
         ATFM_VERSION = as.character(ATFM_VERSION)
  )


## ---- apt-dly-renaming
apt_all <- all %>% rename(
  date = FLT_DATE,
  yyyy = YEAR, 
  mm = MONTH_NUM,
  apt_icao = APT_ICAO,
  apt_name = APT_NAME,
  state_name = STATE_NAME,
  num_arrivals = FLT_ARR_1,
  arrival_delay = DLY_APT_ARR_1,
  A = DLY_APT_ARR_A_1,
  C = DLY_APT_ARR_C_1,
  D = DLY_APT_ARR_D_1,
  E = DLY_APT_ARR_E_1,
  G = DLY_APT_ARR_G_1,
  I = DLY_APT_ARR_I_1,
  M = DLY_APT_ARR_M_1,
  N = DLY_APT_ARR_N_1,
  O = DLY_APT_ARR_O_1,
  P = DLY_APT_ARR_P_1,
  R = DLY_APT_ARR_R_1,
  S = DLY_APT_ARR_S_1,
  `T` = DLY_APT_ARR_T_1,
  V = DLY_APT_ARR_V_1,
  W = DLY_APT_ARR_W_1,
  `NA` = DLY_APT_ARR_NA_1,
  num_dly_arrivals = FLT_ARR_1_DLY,
  num_dly_arrivals_15 = FLT_ARR_1_DLY_15
) %>%
  mutate(
    dd = day(date),
    num_dly_arrivals = ifelse(is.na(num_dly_arrivals), 0, num_dly_arrivals),
    num_dly_arrivals_15 = ifelse(is.na(num_dly_arrivals_15), 0, num_dly_arrivals_15),
    arrival_delay = ifelse(is.na(arrival_delay), 0, arrival_delay)
  ) %>%
  select(- c(MONTH_MON, ATFM_VERSION)) %>%
  select(date, yyyy, mm, dd, everything())

## ---- apt-dly-tidy
apt_all_details <- apt_all %>%
  gather(delay_type, delay, A:`NA`) %>%
  mutate(delay_type = factor(delay_type, 
                             levels = c("A","C","D","E","G","I","M","N","O","P","R","S","T","V","W","NA"))) %>%
  mutate(delay_group = case_when(
    delay_type %in% c("C","S", "I","T")     ~ "ATC attributed",
    delay_type %in% c("D","W")              ~ "W",
    delay_type %in% c("G")                  ~ "G",
    TRUE ~ "Other"
  )) %>%
  mutate(
    delay_group = factor(delay_group, levels = c("ATC attributed", "W", "G", "Other")),
    delay = ifelse(is.na(delay), 0, delay)
  )

apt_all_summaries <- apt_all %>%
  select(date, yyyy, mm, dd, apt_name, state_name ,arrival_delay, num_arrivals, num_dly_arrivals, num_dly_arrivals_15)

