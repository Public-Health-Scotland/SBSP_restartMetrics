##########################################################
# x_compare_reports.R
# Karen Hotopp
# 24/06/2022
# Script 1 of 2
# Checking Aug 2021 report
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

##!! To delete once numbers discrepancy is resolved!


#### 1: Housekeeping ####
## Packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidylog)


## Directories
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
root <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
              "/20201203-Breast-Screening-NSOB-Restart-Metrics",
              "/Weekly Appts Reports/Invite Screen Reports")


## Set month
# Define cut off date (first of the month you are updating data for)
cut_off_date <- "2021-08-01"



#### 2: Import data ####
## Call in whole month file (downloaded R079)
combined <- read.csv(paste0(root, "/Scotland_R079_202108.csv"))


#### 3: Process data ####
## Update date data
combined <- combined %>%
  mutate(WorklistDate = dmy(WorklistDate),
         WorklistID = as.character(WorklistID)) %>%
  mutate(month = month(WorklistDate), .after = WorklistDate) %>%
  mutate(year = year(WorklistDate), .after = month) %>%
  rename(BSC = BSCName) %>% 
  glimpse()

names(combined)
range(combined$WorklistDate)
hist(combined$WorklistDate, breaks = 1000)


#### 4: Summarize ####
## Define start date for each month -- this is just 1st of the month
combined <- combined %>%
  arrange(WorklistDate, BSC) %>%
  mutate(start_date = floor_date(as_date(WorklistDate), "month")) %>% 
  glimpse()

## Create current month counts by centres
current <- combined %>%
  filter(start_date == cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

current_scot <- combined %>%
  filter(start_date == cut_off_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Allocated)

current <- bind_rows(current, current_scot)
View(current)


## Create full database counts by centres
full <- combined %>%
  filter(start_date <= cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

full_scot <- combined %>%
  filter(start_date <= cut_off_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Allocated)

full <- bind_rows(full, full_scot)
View(full)

# GC - checks for duplicates/missing dates
# Look at full database via histogram and table
range(combined$WorklistDate)

ggplot(combined, aes(x = WorklistDate)) +
  geom_histogram()

# Check any dates that look odd from visual inspection
date_check <- combined %>%
  count(WorklistDate)

ggplot(date_check, aes(x = WorklistDate)) +
  geom_histogram(binwidth = 30)

# Check for duplicates
nrow(combined)
#22,642
nrow(distinct(combined))
#22,642 - no duplicates

