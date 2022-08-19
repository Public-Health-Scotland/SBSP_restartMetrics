##########################################################
# x_R079_process.R
# Karen Hotopp
# 24/06/2022
# Script x1 of 2
# Checking downloaded reports to weekly sent reports covering 
# June 2022
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

##!! Replace as main report processing pathway once numbers discrepancy issue
##!! is resolved!


#### 1: Housekeeping ####
# Packages
library(readxl)
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidylog)


# Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
# setwd(wd)
# source(paste0(wd, "/Code/0_housekeeping.R"))
source(here("code","0_housekeeping.R"))

rm(folder1, folder2, folder3, folder4,
   report1, report2, report3, report4)


#### 2: Import data ####
## Call in whole month file (downloaded R079)
combined <- read.csv(paste0(root, "/Scotland_R079_202206.csv"))


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


#### 4: Combine archive data ####
## Add current data extract to full database (from 2020-08-03)
full_db <- readRDS(full_db_path) %>%
  filter(start_date < "2022-06-01") %>% # remove any records added from weekly run
  glimpse()

# # GC - added backup
# saveRDS(full_db, paste0(wd, "/Temp/SBSS Appts Report_database_backup.rds"))

## Update variable names to match
names(combined)
names(full_db)


full_db <- full_db %>%
  rename(WorklistID = WorklistDisplayName,
         Facility = OrganisationName,
         TotalAppts = Total.Appts,
         TotalUnalloc = Total.Free,
         Allocated = Appts,
         DNAReappt = Reappointed,
         CancelledReappt = Cancelled.Reppointed,
         CancelledReused = Cancelled.Reused) %>%
  mutate(Gap = as.integer(""),
         Free = as.integer(""),
         Bulk = as.integer(""),
         WorklistID = as.character(WorklistID)) %>% 
  select(BSC, WorklistID, WorklistDate, month, year,
         Site, Facility, TotalAppts, TotalUnalloc,
         Gap, Free, Bulk,
         Allocated, Attended,
         DNA, DNAReappt,
         Cancelled, CancelledReappt, CancelledReused) %>% 
  glimpse()



full_db <- bind_rows(full_db, combined) #%>%
#   distinct(WorklistID,
#            # GC - added .keep_all = TRUE
#            .keep_all = TRUE)
length(unique(full_db$WorklistID))

## Define start date for each month -- this is just 1st of the month
full_db <- full_db %>%
  arrange(WorklistDate, BSC) %>%
  mutate(start_date = floor_date(as_date(WorklistDate), "month")) %>% 
  glimpse()

## Create current month counts by centres
current <- full_db %>%
  filter(start_date == cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

current_scot <- full_db %>%
  filter(start_date == cut_off_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Allocated)

current <- bind_rows(current, current_scot)
View(current)


## Create full database counts by centres
full <- full_db %>%
  filter(start_date <= cut_off_date) %>%
  group_by(BSC) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

full_scot <- full_db %>%
  filter(start_date <= cut_off_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  mutate(BSC = "Scotland", .before = Allocated)

full <- bind_rows(full, full_scot)
View(full)

# GC - checks for duplicates/missing dates
# Look at full database via histogram and table
range(full_db$WorklistDate)

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
saveRDS(full_db, paste0(wd, "/Temp/SBSS Appts Report_database_KH.rds"))


