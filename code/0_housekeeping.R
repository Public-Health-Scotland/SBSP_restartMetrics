##########################################################
# 0_housekeeping.R
# Karen Hotopp
# 18/02/2022
# Script 1 of 2
# Define some housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### Values ####
## Set root directory for screening reports
root <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
              "/20201203-Breast-Screening-NSOB-Restart-Metrics",
              "/Weekly Appts Reports/Invite Screen Reports")

## Set folder dates
# May
folder1 <- "20220606"
folder2 <- "20220613"
folder3 <- "20220620"
folder4 <- "20220627"
#folder5 <- "20220502" # Comment out if only four folders this month


## Set report dates
# May
report1 <- "06062022"
report2 <- "13062022"
report3 <- "20062022"
report4 <- "27062022"
#report5 <- "02052022" # Comment out if only four folders this month

## Set month
# Numeric value for target month
MM <- 6
YYYY <- 2022
# Define cut off date (first of the month you are updating data for)
cut_off_date <- "2022-06-01"


#### Function ####
## Function sets pathways within each folder, reads in data,
## and outputs combined data from centres
import_folder <- function(folder, report) {
  
  # Write file paths
  # Using readxl
  # eos_path <- (paste0(root, "/Reports ", folder,
  #                     "/EastScotland_Report_", report, ".xls"))
  # neos_path <- (paste0(root, "/Reports ", folder,
  #                      "/NorthEastScotland_Report_", report, ".xls"))
  # nos_path <- (paste0(root, "/Reports ", folder,
  #                     "/NorthScotland_Report_", report, ".xls"))
  # seos_path <- (paste0(root, "/Reports ", folder,
  #                      "/SouthEastScotland_Report_", report, ".xls"))
  # swos_path <- (paste0(root, "/Reports ", folder,
  #                      "/SouthWestScotland_Report_", report, ".xls"))
  # wos_path <- (paste0(root, "/Reports ", folder,
  #                     "/WestScotland_Report_", report, ".xls"))

  # Use this section if readxl not working and importing .csv files
  eos_path <- (paste0(root, "/Reports ", folder,
                      "/EastScotland_Report_", report, ".csv"))
  neos_path <- (paste0(root, "/Reports ", folder,
                       "/NorthEastScotland_Report_", report, ".csv"))
  nos_path <- (paste0(root, "/Reports ", folder,
                      "/NorthScotland_Report_", report, ".csv"))
  seos_path <- (paste0(root, "/Reports ", folder,
                       "/SouthEastScotland_Report_", report, ".csv"))
  swos_path <- (paste0(root, "/Reports ", folder,
                       "/SouthWestScotland_Report_", report, ".csv"))
  wos_path <- (paste0(root, "/Reports ", folder,
                      "/WestScotland_Report_", report, ".csv"))
  
  # Import individual centre reports
  # Using readxl
  # eos <- read_excel(eos_path)
  # neos <- read_excel(neos_path)
  # nos <- read_excel(nos_path)
  # seos <- read_excel(seos_path)
  # swos <- read_excel(swos_path)
  # wos <- read_excel(wos_path)

  # Use this section if readxl not working and importing .csv files
  eos <- read.csv(eos_path)
  neos <- read.csv(neos_path)
  nos <- read.csv(nos_path)
  seos <- read.csv(seos_path)
  swos <- read.csv(swos_path)
  wos <- read.csv(wos_path)


 combined <- bind_rows(eos, neos, nos, seos, swos, wos)
  
}


#### Pathways for full database data ####
full_db_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening",
                        "/Investigations/20201203-Breast-Screening-NSOB-Restart-Metrics",
                        "/Temp/SBSS Appts Report_database.rds"))


# ####################
# # Create full database in RDS format
# 
# library(dplyr)
# library(lubridate)
# library(tidylog)
# 
# 
# repo <- read.csv(here::here("Weekly Appts Reports",
#                             "SBSS Appts Report - Database_20220204.csv"))
# glimpse(repo)
# 
# repo <- repo %>%
#   mutate(WorklistDate = dmy(WorklistDate)) %>%
#   mutate(month = month(WorklistDate), .after = WorklistDate) %>%
#   mutate(year = year(WorklistDate), .after = month) %>%
#   rename(Total.Appts = total.Appts)
#   glimpse()
# 
# hist(repo$WorklistDate, breaks = 5000)
# 
# range(repo$WorklistDate)
# 
# repo_test <- repo %>%
#   filter(WorklistDate < "2022-01-01")
# 
# range(repo_test$WorklistDate)
# 
# ## Save this to the Temp folder so that it can be joined with new monthly data
# saveRDS(repo_test, here::here("Temp", "SBSS Appts Report_database.rds"))


