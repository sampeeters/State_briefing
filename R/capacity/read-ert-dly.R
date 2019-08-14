
all=read.xlsx("https://coll.eurocontrol.int/sites/pru/dashboard/Data/En-Route_ATFM_Delay_AUA.xlsx", sheet="DATA") %>% 
  mutate(YEAR = as.integer(YEAR),
         MONTH_NUM = as.integer(MONTH_NUM),
         MONTH_MON = as.character(MONTH_MON),
         FLT_DATE = as.Date(FLT_DATE, origin="1899-12-30"),
         ENTITY_NAME = as.character(ENTITY_NAME),
         ENTITY_TYPE = as.character(ENTITY_TYPE),
         FLT_ERT_1 = as.integer(FLT_ERT_1),
         DLY_ERT_1 = as.integer(DLY_ERT_1),
         DLY_ERT_A_1 = as.integer(DLY_ERT_A_1),
         DLY_ERT_C_1 = as.integer(DLY_ERT_C_1),
         DLY_ERT_D_1 = as.integer(DLY_ERT_D_1),
         DLY_ERT_E_1 = as.integer(DLY_ERT_E_1),
         DLY_ERT_G_1 = as.integer(DLY_ERT_G_1),
         DLY_ERT_I_1 = as.integer(DLY_ERT_I_1),
         DLY_ERT_M_1 = as.integer(DLY_ERT_M_1),
         DLY_ERT_N_1 = as.integer(DLY_ERT_N_1),
         DLY_ERT_O_1 = as.integer(DLY_ERT_O_1),
         DLY_ERT_P_1 = as.integer(DLY_ERT_P_1),
         DLY_ERT_R_1 = as.integer(DLY_ERT_R_1),
         DLY_ERT_S_1 = as.integer(DLY_ERT_S_1),
         DLY_ERT_T_1 = as.integer(DLY_ERT_T_1),
         DLY_ERT_V_1 = as.integer(DLY_ERT_V_1),
         DLY_ERT_W_1 = as.integer(DLY_ERT_W_1),
         DLY_ERT_NA_1 = as.integer(DLY_ERT_NA_1)
  ) %>%
  arrange(FLT_DATE, ENTITY_NAME)


## ---- ert-dly-renaming
enr_all <- all %>% rename(
  date = FLT_DATE,
  yyyy = YEAR,
  mm = MONTH_NUM,
  entity_name = ENTITY_NAME, 
  entity_type = ENTITY_TYPE,
  flights = FLT_ERT_1,
  delay_total = DLY_ERT_1,
  A = DLY_ERT_A_1,
  C = DLY_ERT_C_1,
  D = DLY_ERT_D_1,
  E = DLY_ERT_E_1,
  G = DLY_ERT_G_1,
  I = DLY_ERT_I_1,
  M = DLY_ERT_M_1,
  N = DLY_ERT_N_1,
  O = DLY_ERT_O_1,
  P = DLY_ERT_P_1,
  R = DLY_ERT_R_1,
  S = DLY_ERT_S_1,
  `T` = DLY_ERT_T_1,
  V = DLY_ERT_V_1,
  W = DLY_ERT_W_1,
  `NA` = DLY_ERT_NA_1,
  flights_delayed = FLT_ERT_1_DLY,
  flights_delayed_gt15 = FLT_ERT_1_DLY_15
) %>%
  mutate(
    dd = day(date),
    flights_delayed = ifelse(is.na(flights_delayed), 0, flights_delayed),
    flights_delayed_gt15 = ifelse(is.na(flights_delayed_gt15), 0, flights_delayed_gt15),
    delay_total = ifelse(is.na(delay_total), 0, delay_total)
  ) %>%
  select(-MONTH_MON) %>%
  select(date, yyyy, mm, dd, everything())

## ---- ert-dly-summaries
enr_all_summaries <- enr_all %>%
  select(date, yyyy, mm, dd, entity_name, entity_type, flights, flights_delayed, flights_delayed_gt15, delay_total)

enr_all <- enr_all %>%
  select(-flights, -flights_delayed, -flights_delayed_gt15, -delay_total)

## ---- ert-dly-tidy
enr_all_details <- enr_all %>%
  gather(delay_type, delay, A:`NA`) %>%
  mutate(delay_type = factor(delay_type, 
    levels = c("A","C","D","E","G","I","M","N","O","P","R","S","T","V","W","NA"))) %>%
  mutate(delay_group = case_when(
    delay_type %in% c("A","E","N","O","NA","G","M","R","V","P") ~ "Other",
    delay_type %in% c("C")                  ~ "Ca",
    delay_type %in% c("D","W")              ~ "W",
    delay_type %in% c("I","T")              ~ "Da",
    delay_type %in% c("S")                  ~ "Sa"
  )) %>%
  mutate(
    delay_group = factor(delay_group, levels = c("W", "Da", "Sa", "Ca", "Other")),
    delay = ifelse(is.na(delay), 0, delay)
    )


# Area
ectrl_summaries <- enr_all_summaries %>% filter(grepl("EUROCONTROL Area", entity_name))
ectrl_details <- enr_all_details %>% filter(grepl("EUROCONTROL Area", entity_name))

ses_summaries <- enr_all_summaries %>% filter(grepl("SES Area", entity_name))
ses_details <- enr_all_details %>% filter(grepl("SES Area", entity_name))

