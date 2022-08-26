##########################################################
# x_R079_create_historic.R
# Karen Hotopp
# 26/08/2022
# Script x1 of 2
# Call in historic R079 RDS file and create counts of appointments
# allocated v attended
# Completed data file starts at Jan 2018
# Written/run on R Studio Server
# R version 3.6.1
##########################################################


#### 1: Housekeeping ####
## Packages
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(tidylog)


## Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")

today <- Sys.Date()


#### 2: Import data ####
brest <- read_rds(paste0(wd, "/Output/SBSS_R079_historic_2022-08-26.rds"))

## Define start date for each month -- this is just 1st of the month
brest <- brest %>%
  #arrange(WorklistDate, BSC) %>%
  mutate(start_date = floor_date(as_date(WorklistDate), "month")) %>% 
  glimpse()


#### 3: Set up counts ####



















