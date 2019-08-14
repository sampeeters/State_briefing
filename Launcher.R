
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(ROracle)
library(pander)
library(ggplot2)
library(viridis)
library(scales)
library(ectrlplot)
library(tidyquant)
library(knitr)
library(chron)
library(ggforce)
library(gridExtra)

dir="G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/"
Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
curr_year=as.numeric(format(Sys.Date(), "%Y"))
curr_month=as.numeric(format(Sys.Date(), "%m"))

State_curr="Germany"
STATFOR_version="Feb. 2019"
STATFOR_start_year=2008


source("Read_data.R")
source("Traffic_characteristics.R")

# En-route ATFM delay

state_under_review <- "LFV"
year_start <- "2015-01-01"
year_end <- "2019-01-31"
year_input <- "2018"
ref_year <- "2017"

# Airport arrival ATFM delays

airport <- "Stockholm"
since <- "2014"
to <- "2019"

source("R/capacity/read-ert-dly.R")
source("R/capacity/read-apt-dly.R")
source("R/capacity/plot-flights.R")
source("R/capacity/plot-delays.R")
source("R/capacity/plot-avg-dly-flt.R")
source("R/capacity/plot-avg-grouped-dly-flt.R")

# Horizontal flight efficieny
year_input <- "2018"
year_input_start <- "2014"

model <- "CPF"
state_under_review <- "Germany"
year_start <- "2014-01-01"
year_end <- "2018-12-31"

source("R/environment/read-hfe.R")
source("R/environment/plot-hfe-year.R")
source("R/environment/plot-hfe-daily.R")
source("R/environment/plot-flt-efficiency.R")


rmarkdown::render(paste0(dir, 'State_briefing_pdf.Rmd'),
                  output_file = paste0(dir, 'Test/State briefing ', State_curr, '.pdf'))
