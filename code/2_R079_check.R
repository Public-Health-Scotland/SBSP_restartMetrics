##########################################################
# 2_R079_check.R
# Karen Hotopp
# 29/05/2023
# Script 3 of 3
# Check processing of R079 by second person.
# Written on R Studio Server; run on Posit WB
# R version 4.1.2
##########################################################


#### 1: Housekeeping ####
## Packages
library(readr)
library(dplyr)
library(magrittr)
library(forcats)
library(ggplot2)
library(tidylog)


## Pathways
rm(list = ls())
source(here::here("code/0_housekeeping.R"))


#### 2: Import data ####
## Current month's records
current <- read_csv(paste0(r079_path, file_name, YYMM, ".csv"))
count(current)

## Full records should include Jan 2018 to most recent month
full_db <- read_rds(paste0(proj_folder,"/Output/SBSS_R079_complete.rds"))
## current month should match number of observations in object `current`.
table(full_db$start_date)


## Check any dates that look odd from visual inspection
date_check <- full_db %>%
  count(WorklistDate)

ggplot(date_check, aes(x = WorklistDate)) +
  geom_histogram(binwidth = 20)


#### 3: Set up counts ####
## Create month counts by centres
counts <- full_db %>%
  group_by(BSCName, start_date) %>%
  summarize(Allocated = sum(Allocated),
            Attended = sum(Attended)) %>%
  ungroup

counts_scot <- full_db %>%
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


rm(allocated, attended, counts, counts_scot, current, 
   full_db, sum, sum_all, sum_att, date_check)


#### 4: Set up data for Excel ####
### Br - Attendances tab
## Scotland
scot <- full_metrics %>%
  filter(BSCName == "Scotland") %>% 
  select(-c(`Jan-2018`:`Jul-2020`, Total)) %>% 
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  glimpse

## Centres--Allocated
allocated <- full_metrics %>%
  filter(BSCName != "Scotland",
         appt_type == "allocated") %>%
  select(-c(`Jan-2018`:`Jul-2020`, Total)) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  glimpse

## Centres--Attended
attended <- full_metrics %>%
  filter(BSCName != "Scotland",
         appt_type == "attended") %>%
  select(-c(`Jan-2018`:`Jul-2020`, Total)) %>% 
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  glimpse

rm(full_metrics)

#### 5: Check against Excel output ####
## Only the most recent data month and totals need to be compared.
# Check that the `attended` and `allocated` outputs match the entries on
# `Br - Attendances` tab of the Excel file. Uptake and historical comparisons
# are calculated automatically within Excel.

View(scot)
View(allocated)
View(attended)


