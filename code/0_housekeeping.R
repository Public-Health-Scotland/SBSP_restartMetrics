##########################################################
# 0_housekeeping.R
# Karen Hotopp
# 18/02/2022
# Updated August 2022
# Script 1 of ?
# Define some housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### Pathways ####
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
root <- paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
               "/20201203-Breast-Screening-NSOB-Restart-Metrics",
               "/Weekly Appts Reports/Invite Screen Reports/R079")
fname <- "/Scotland_R079_20"

today <- Sys.Date()


## Set month
# Define cut off date (first of the month you are updating data for)
cut_off_date <- "2022-06-01"






