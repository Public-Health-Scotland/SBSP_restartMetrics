##########################################################
# 1_R079_process.R
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
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(forcats)
library(tidylog)
library(openxlsx)


## Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
source(paste0(wd, "/SBSP_restartMetrics/code/0_housekeeping.R"))


#### 2: Import data ####
## Full records (Jan 2018 - most recent month)
full_db <- read_rds(paste0(wd, "/Output/SBSS_R079_complete.rds"))
## current month's records
current <- read_csv(paste0(root, file_name, YYMM, ".csv"))

## Define start date for each month -- this is just 1st of the month
current %<>%
  mutate(WorklistDate = dmy(WorklistDate),
         start_date = floor_date(as_date(WorklistDate), "month"), 
         .after = WorklistDate) %>% 
  arrange(BSCName, WorklistDate) %>% 
  glimpse()


## Add new records onto full database
new_db <- bind_rows(full_db, current) %>% 
  arrange(WorklistDate, BSCName)

table(new_db$start_date)
ggplot(new_db, aes(x = WorklistDate)) +
  geom_histogram()

# Check any dates that look odd from visual inspection
date_check <- new_db %>%
  count(WorklistDate)

ggplot(date_check, aes(x = WorklistDate)) +
  geom_histogram(binwidth = 20)


## Write out complete db for a back-up copy
write_rds(new_db, paste0(wd, "/Output/SBSS_R079_complete.rds"))


#### 3: Set up counts ####
## Create month counts by centres
counts <- new_db %>%
  group_by(BSCName, start_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

counts_scot <- new_db %>%
  group_by(start_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup() %>% 
  mutate(BSCName = "Scotland", .before = start_date)

counts <- bind_rows(counts, counts_scot)
View(counts)


## Restructure table
counts %<>%
  mutate(month = format(start_date, "%h-%Y"), .after=BSCName) %>%
  mutate(BSCName = factor(BSCName)) %>% 
  mutate(BSCName = fct_relevel(BSCName, c("East of Scotland",
                                 "North East of Scotland",
                                 "North of Scotland",
                                 "South East of Scotland",
                                 "South West of Scotland",
                                 "West of Scotland",
                                 "Scotland"))) %>% 
  arrange(BSCName) %>% 
  glimpse()


## Separate out two appointment types  
## Allocated
allocated <- counts %>%
  select(BSCName, month, Allocated) %>% 
  pivot_wider(names_from = month, values_from = Allocated) %>% 
  mutate(appt_type = "allocated", .after = BSCName) %>% 
  relocate(`May-2020`, `Jun-2020`, .after = `Apr-2020`) %>% 
  glimpse()
# replce all NAs with 0
allocated[is.na(allocated)] <- 0

## Attended
attended <- counts %>%
  select(BSCName, month, Attended) %>% 
  pivot_wider(names_from = month, values_from = Attended) %>% 
  mutate(appt_type = "attended", .after = BSCName) %>% 
  relocate(`May-2020`, `Jun-2020`, .after = `Apr-2020`) %>% 
glimpse()
# replce all NAs with 0
attended[is.na(attended)] <- 0


## Create appt type totals counts
sum <- counts %>% 
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


## Rejoin counts by stacking
full_metrics <- bind_rows(allocated, attended)

## Save
saveRDS(full_metrics, paste0(wd, "/Output/SBSS_R079_counts.rds"))

rm(allocated, attended, counts, counts_scot, current, 
   full_db, new_db, sum, sum_all, sum_att)


#### 4: Set up data for Excel ####
## Br - Attendances tab
## Historical average (2018/19) ---
hist_1819 <- full_metrics %>% 
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

# # Active month historic
# # add months (selecting columns) to this as appropriate to extend historic loop
# hist1819_active <- hist_1819 %>% 
#   select(`Aug 18/19`)


### Aug 2020 to current ----
## Br - Attendances tab
## Scotland
scot <- full_metrics %>%
  filter(BSCName == "Scotland") %>% 
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse

## Centres--Allocated
allocated <- full_metrics %>%
  filter(BSCName != "Scotland",
         appt_type == "allocated") %>%
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse

## Centres--Attended
attended <- full_metrics %>%
  filter(BSCName != "Scotland",
         appt_type == "attended") %>%
  select(-c(BSCName:`Jul-2020`)) %>% 
  glimpse


#### 5: Write to Excel ####
wb <- loadWorkbook(paste0(wd, "/Output/NSOB Recovery Metrics Breast Screening_temp.xlsx"))

### July 2020 to current ----
writeData(wb, sheet = "Br - Attendances", scot, 
          startCol = "B", startRow = 1, colNames = T)
writeData(wb, sheet = "Br - Attendances", allocated, 
          startCol = "B", startRow = 13, colNames = T)
writeData(wb, sheet = "Br - Attendances", attended, 
          startCol = "B", startRow = 22, colNames = T)

### Historical average (2018/19) ---
# uncomment below to add historic loop months to historic average (2018/19)
# writeData(wb, sheet = "Br - Attendances", hist1819_active, 
#           startCol = "Z", startRow = 7, colNames = T)

writeData(wb, sheet = "Br - Attendances", paste0("Updated ", today),
          startCol = "A", startRow = 42)


writeData(wb, sheet = "Br - Historical Attendances", paste0("Updated ", today),
          startCol = "A", startRow = 26)


### Save ----
saveWorkbook(wb, paste0(wd, 
                        "/Output/NSOB Recovery Metrics Breast Screening_", 
                        YYMM, ".xlsx"),
             overwrite = T)