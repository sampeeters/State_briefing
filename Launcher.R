
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
library(ggrepel)
library(reshape2)

dir="G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/"
Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
curr_year=as.numeric(format(Sys.Date(), "%Y"))
curr_month=as.numeric(format(Sys.Date(), "%m"))

State_curr="Germany"
ANSP <- "DFS"
STATFOR_version="Feb. 2019"
STATFOR_start_year=2008
BADA_curr="3.13.1"


source("Read_data.R")
source("Traffic_characteristics.R")
source("Capacity.R")
source("Environment.R")


rmarkdown::render(paste0(dir, 'State_briefing_pdf.Rmd'),
                  output_file = paste0(dir, 'Test/State briefing ', State_curr, '.pdf'))
