##########################################################
# x_R079_create_historic.R
# Karen Hotopp
# 26/08/2022
# Script 1 of 2
# Call in new monthly R079 .csv file, add to historic R079 database,
# create counts of allocated v attended appointments, and write
# to Excel file.
# Written/run on R Studio Server
# R version 3.6.1
##########################################################


#### 1: Housekeeping ####
## Packages
library(dplyr)
library(magrittr)
library(lubridate)
library(forcats)
library(tidylog)


## Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
source(paste0(wd, "/SBSP_restartMetrics/code/0_housekeeping.R"))


#### 2: Import data ####
report <- read_csv(paste0(root, file_name, YYMM, ".csv"))

## Define start date for each month -- this is just 1st of the month
brest %<>%
  mutate(start_date = floor_date(as_date(WorklistDate), "month")) %>% 
  glimpse()


#### 3: Set up counts ####
## Create month counts by centres
# There are 55 months, so each centre should have 55 records
full_db <- brest %>%
  group_by(BSCName, start_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

full_db_scot <- brest %>%
  group_by(start_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup() %>% 
  mutate(BSCName = "Scotland", .before = start_date)

full_db <- bind_rows(full_db, full_db_scot)
View(full_db)


#### What are the missing months?
table(brest$BSCName)
east <- brest[brest$BSCName == "East of Scotland",]
noreast <- brest[brest$BSCName == "North East of Scotland",]
north <- brest[brest$BSCName == "North of Scotland",]
soeast <- brest[brest$BSCName == "South East of Scotland",]
sowest <- brest[brest$BSCName == "South West of Scotland",]
west <- brest[brest$BSCName == "West of Scotland",]

table(east$start_date) # missing May/June 2020
table(noreast$start_date) # missing June/July 2020
table(soeast$start_date) # missing May/June/July 2020
table(sowest$start_date) # missing July 2020
table(west$start_date) # missing May/June 2020
# These centres have no records for the months above (COVID-19 program stop)

rm(east, noreast, soeast, sowest, west)


## Restructure db
full_db %<>%
  mutate(month = format(start_date, "%h-%Y"), .after=BSCName) %>%
  mutate(BSCName = factor(BSCName)) %>% 
  mutate(BSCName = fct_relevel(BSCName, c("East of Scotland",
                                 "North East of Scotland",
                                 "North of Scotland",
                                 "South East of Scotland",
                                 "South West of Scotland",
                                 "West of Scotland",
                                 "Scotland"))) %>% 
  arrange(BSCName, start_date) %>% 
  glimpse()


## Separate out two counts  
## Allocated
allocated <- full_db %>%
  select(BSCName, month, Allocated) %>% 
  pivot_wider(names_from = month, values_from = Allocated) %>% 
  mutate(appt_type = "allocated", .after = BSCName) %>% 
  relocate(`May-2020`, `Jun-2020`, .after = `Apr-2020`) %>% 
  glimpse()
# replce all NAs with 0
allocated[is.na(allocated)] <- 0

## Attended
attended <- full_db %>%
  select(BSCName, month, Attended) %>% 
  pivot_wider(names_from = month, values_from = Attended) %>% 
  mutate(appt_type = "attended", .after = BSCName) %>% 
  relocate(`May-2020`, `Jun-2020`, .after = `Apr-2020`) %>% 
glimpse()
# replce all NAs with 0
attended[is.na(attended)] <- 0


## Create totals counts
sum <- full_db %>% 
  group_by(BSCName) %>% 
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup %>% 
  rename(total_alloc = Allocated,
         total_attend = Attended) %>% 
  glimpse


## Combine counts and totals
sum_all <- select(sum, BSCName, total_alloc) %>% 
  rename(Total = total_alloc)
sum_att <- select(sum, BSCName, total_attend) %>% 
  rename(Total = total_attend)

allocated <- left_join(allocated, sum_all)
attended <- left_join(attended, sum_att)


#### 4: Rejoin and Save ####
## Rejoin counts by stacking
full_metrics <- bind_rows(allocated, attended)

## Save
saveRDS(full_metrics, paste0(wd, "/Output/SBSS_R079_historic.rds"))

