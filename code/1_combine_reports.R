##########################################################
# 1_combine_reports.R
# Karen Hotopp & Calum Purdie & Gavin Clark
# 18/02/2022
# Script 2 of 2
# Create monthly numbers counts, as well as archive of 
# complete data from 2020-08-03
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### 1: Housekeeping ####
# Packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidylog)


# Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
             "/20201203-Breast-Screening-NSOB-Restart-Metrics")
setwd(wd)
source(paste0(wd, "/Code/0_housekeeping.R"))



#### 2: Import data ####
## Import data by Report folder
## The file that is sent to us is protected(?), so readxl is not able to import.
## File therefore rewritten as CSV file when initially saved.

week_1 <- import_folder(folder1, report1)
week_2 <- import_folder(folder2, report2)
week_3 <- import_folder(folder3, report3)
week_4 <- import_folder(folder4, report4)
#week_5 <- import_folder(folder5, report5) # If month only has four weeks, comment out


## Combine to create month extract
combined <- bind_rows(week_1, 
                      week_2, 
                      week_3, 
                      week_4
                      #,week_5 # If month only has four weeks, comment out
                      )


#### 3: Process data ####
## Update date data
combined <- combined %>%
  mutate(Worklist.Date = dmy(Worklist.Date)) %>%
  rename(WorklistDate = Worklist.Date,
         WorklistDisplayName = Worklist.Display.Name,
         OrganisationName = Organisation.Name) %>%
  glimpse()

range(combined$WorklistDate)

hist(combined$WorklistDate, breaks = 1000)

combined <- combined %>%
  mutate(month = month(WorklistDate), .after = WorklistDate) %>%
  mutate(year = year(WorklistDate), .after = month) %>%
  glimpse()



#### 4: Combine archive data ####
## Add current data extract to full database (from 2020-08-03)
full_db <- readRDS(full_db_path) %>%
  glimpse()

# GC - added backup
saveRDS(full_db, paste0(wd, "/Temp/SBSS Appts Report_database_backup.rds"))

full_db <- bind_rows(full_db, combined) %>%
  distinct(WorklistDisplayName,
           # GC - added .keep_all = TRUE
           .keep_all = TRUE)

## Define start date for each month -- this is just 1st of the month
full_db <- full_db %>%
  arrange(WorklistDate, BSC) %>%
  mutate(start_date = floor_date(as_date(WorklistDate), "month")) %>% 
  glimpse()

## Create current month counts by centres
current <- full_db %>%
  filter(start_date == cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Appts = sum(Appts),
            Attended = sum(Attended)) %>%
  ungroup

current_scot <- full_db %>%
  filter(start_date == cut_off_date) %>%
  summarize(Appts = sum(Appts),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Appts)

current <- bind_rows(current, current_scot)
View(current)


## Create full database counts by centres
full <- full_db %>%
  filter(start_date <= cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Appts = sum(Appts),
            Attended = sum(Attended)) %>%
  ungroup

full_scot <- full_db %>%
  filter(start_date <= cut_off_date) %>%
  summarize(Appts = sum(Appts),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Appts)

full <- bind_rows(full, full_scot)
View(full)

# GC - checks for duplicates/missing dates
# Look at full database via histogram and table
ggplot(full_db, aes(x = WorklistDate)) +
  geom_histogram()

# Check any dates that look odd from visual inspection
date_check <- full_db %>%
  count(WorklistDate)

ggplot(date_check, aes(x = WorklistDate)) +
  geom_histogram(binwidth = 20)

# Check for duplicates
nrow(full_db)
#22,642
nrow(distinct(full_db))
#22,642 - no duplicates

#### 5: Write archive data ####
## Re-save as full database, from 2020-08-03 to current
saveRDS(full_db, paste0(wd, "/Temp/SBSS Appts Report_database.rds"))


