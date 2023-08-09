##########################################################
# 0_housekeeping.R
# Karen Hotopp
# 18/02/2022
# Updated August 2022
# Script 1 of 3
# Define some housekeeping variables used by subsequent scripts
# Written on R Studio Server; run on Posit WB
# R version 4.1.2
##########################################################

#### Pathways ####
proj_folder <- paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
                      "/20201203-Breast-Screening-NSOB-Restart-Metrics")

r079_path <- paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
                    "/20201203-Breast-Screening-NSOB-Restart-Metrics",
                    "/Weekly Appts Reports/Invite Screen Reports/R079")

file_name <- "/Scotland_R079_"
YYMM <- 202306 # current year and month to add to data
today <- Sys.Date()


