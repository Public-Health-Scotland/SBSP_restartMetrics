##########################################################
# x3_R079_write_Excel.R
# Karen Hotopp
# 29/08/2022
# Script x3 of ?
# Call in historic R079 RDS file and create counts of appointments
# allocated v attended
# Completed data file starts at Jan 2018
# Written/run on R Studio Server
# R version 3.6.1
# **This is a scratch pad for writing out to Excel and will be 
# deleted/incorporated into the processing file!**
##########################################################


#### 1: Housekeeping ####
## Packages
library(dplyr)
library(magrittr)
library(openxlsx)
library(tidylog)


## Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")

today <- Sys.Date()


## Data
hist <- readRDS(paste0(wd, "/Output/SBSS_R079_historic.rds"))


#### 2: Set up data for Excel ####
## Br - Attendances tab
## Historical average (2018/19) ---
hist_1819 <- hist %>% 
  filter(BSCName == "Scotland") %>% 
  select(BSCName:`Dec-2019`) %>% 
  glimpse()

hist_1819 %<>% 
  # find monthly average for the two years
  # Excel file starts at Aug, then loops continuously
  group_by(BSCName, appt_type) %>% 
  summarize(`Aug 18/19` = (`Aug-2018` + `Aug-2019`)/2,
            `Sep 18/19` = (`Sep-2018` + `Sep-2019`)/2,
            `Oct 18/19` = (`Oct-2018` + `Oct-2019`)/2,
            `Nov 18/19` = (`Nov-2018` + `Nov-2019`)/2,
            `Dec 18/19` = (`Dec-2018` + `Dec-2019`)/2,
            `Jan 18/19` = (`Jan-2018` + `Jan-2019`)/2,
            `Feb 18/19` = (`Feb-2018` + `Feb-2019`)/2,
            `Mar 18/19` = (`Mar-2018` + `Mar-2019`)/2,
            `Apr 18/19` = (`Apr-2018` + `Apr-2019`)/2,
            `May 18/19` = (`May-2018` + `May-2019`)/2,
            `Jun 18/19` = (`Jun-2018` + `Jun-2019`)/2,
            `Jul 18/19` = (`Jul-2018` + `Jul-2019`)/2) %>% 
  ungroup() %>% 
  glimpse()

# Full month historic
hist1819_full <- hist_1819 %>% 
  select(-c(BSCName:appt_type))
# # Active month historic
# # add months (selecting columns) to this as appropriate to extend historic loop
# hist1819_active <- hist_1819 %>% 
#   select(`Aug 18/19`)


### July 2020 to current ----
## Br - Attendances tab
## Scotland
scot <- hist %>%
  filter(BSCName == "Scotland") %>% 
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse

## Centres--Allocated
allocated <- hist %>%
  filter(BSCName != "Scotland",
         appt_type == "allocated") %>%
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse

## Centres--Attended
attended <- hist %>%
  filter(BSCName != "Scotland",
         appt_type == "attended") %>%
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse


### January 2018 to Mar 2020 ----
## Br - Historical Attendances tab
## Scotland
scot_hist <- hist %>%
  filter(BSCName == "Scotland") %>% 
  select(c(`Jan-2018`:`Mar-2020`)) %>% 
  glimpse

## Centres--Allocated
allocated_hist <- hist %>%
  filter(BSCName != "Scotland",
         appt_type == "allocated") %>%
  select(c(`Jan-2018`:`Mar-2020`)) %>% 
  glimpse

## Centres--Attended
attended_hist <- hist %>%
  filter(BSCName != "Scotland",
         appt_type == "attended") %>%
  select(c(`Jan-2018`:`Mar-2020`)) %>% 
  glimpse


#### 3: Write to Excel ####
wb <- loadWorkbook(paste0(wd, "/Output/NSOB Recovery Metrics Breast Screening_KHtest.xlsx"))

### July 2020 to current ----
writeData(wb, sheet = "Br - Attendances", scot, 
          startCol = "B", startRow = 1, colNames = T)
writeData(wb, sheet = "Br - Attendances", allocated, 
          startCol = "B", startRow = 13, colNames = T)
writeData(wb, sheet = "Br - Attendances", attended, 
          startCol = "B", startRow = 22, colNames = T)

### Historical average (2018/19) ---
writeData(wb, sheet = "Br - Attendances", hist1819_full, 
          startCol = "B", startRow = 7, colNames = T)
writeData(wb, sheet = "Br - Attendances", hist1819_full, 
          startCol = "N", startRow = 7, colNames = T)
# uncomment below to add historic loop months to historic average (2018/19)
# writeData(wb, sheet = "Br - Attendances", hist1819_active, 
#           startCol = "Z", startRow = 7, colNames = T)

writeData(wb, sheet = "Br - Attendances", paste0("Updated ", today),
          startCol = "A", startRow = 42)


### January 2018 to Mar 2020 ----
writeData(wb, sheet = "Br - Historical Attendances", scot_hist, 
          startCol = "B", startRow = 1, colNames = T)
writeData(wb, sheet = "Br - Historical Attendances", allocated_hist, 
          startCol = "B", startRow = 6, colNames = T)
writeData(wb, sheet = "Br - Historical Attendances", attended_hist, 
          startCol = "B", startRow = 15, colNames = T)

writeData(wb, sheet = "Br - Attendances", paste0("Updated ", today),
          startCol = "A", startRow = 26)


### Save ----
saveWorkbook(wb, paste0(wd, 
                        "/Output/NSOB Recovery Metrics Breast Screening_DidItWork.xlsx"),
             overwrite = T)




