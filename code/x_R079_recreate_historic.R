##########################################################
# x_R079_recreate_historic.R
# Karen Hotopp
# 26/08/2022
# Script x of ?
# Pulling in historic R079 reports to create a complete historic file
# that uses downloaded R079 reports (vs weekly sent reports)
# Completed data file starts at Jan 2018
# Updated August 2022
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

#### 1: Housekeeping ####
## Packages
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(tidylog)


## Pathways
rm(list = ls())
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
root <- paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
               "/20201203-Breast-Screening-NSOB-Restart-Metrics",
               "/Weekly Appts Reports/Invite Screen Reports/R079")
fname <- "/Scotland_R079_20"

today <- Sys.Date()

## Function
# Imports correct file, defines date variable, and exports report
import_report <- function(YYMM) {
  
  report <- read_csv(paste0(root, fname, {{YYMM}}, ".csv")) 
  
  report %<>%
    mutate(WorklistDate = dmy(WorklistDate)) %>%
    arrange(BSCName, WorklistDate) %>% 
    glimpse()
  
  report
  
}


#### 2: Import data ####
# Import each data file and combine into years
# Check ranges and plot for visual confirmation
# Note that Feb & Mar 2018 sit in the same data file

### 2018 ----
df1801 <- import_report(1801)
df1802 <- import_report(1802)
df1804 <- import_report(1804)
df1805 <- import_report(1805)
df1806 <- import_report(1806)
df1807 <- import_report(1807)
df1808 <- import_report(1808)
df1809 <- import_report(1809)
df1810 <- import_report(1810)
df1811 <- import_report(1811)
df1812 <- import_report(1812)

## Check monthly ranges
range(df1801$WorklistDate)
range(df1802$WorklistDate)
range(df1804$WorklistDate)
range(df1805$WorklistDate)
range(df1806$WorklistDate)
range(df1807$WorklistDate)
range(df1808$WorklistDate)
range(df1809$WorklistDate)
range(df1810$WorklistDate)
range(df1811$WorklistDate)
range(df1812$WorklistDate)

## Combine into annual
df2018 <- rbind(df1801, df1802, df1804, df1805, df1806,
                df1807, df1808, df1809, df1810, df1811, df1812)

rm(df1801, df1802, df1804, df1805, df1806,
   df1807, df1808, df1809, df1810, df1811, df1812)


### 2019 ----
df1901 <- import_report(1901)
df1902 <- import_report(1902)
df1903 <- import_report(1903)
df1904 <- import_report(1904)
df1905 <- import_report(1905)
df1906 <- import_report(1906)
df1907 <- import_report(1907)
df1908 <- import_report(1908)
df1909 <- import_report(1909)
df1910 <- import_report(1910)
df1911 <- import_report(1911)
df1912 <- import_report(1912)

## Check monthly ranges
range(df1901$WorklistDate)
range(df1902$WorklistDate)
range(df1903$WorklistDate)
range(df1904$WorklistDate)
range(df1905$WorklistDate)
range(df1906$WorklistDate)
range(df1907$WorklistDate)
range(df1908$WorklistDate)
range(df1909$WorklistDate)
range(df1910$WorklistDate)
range(df1911$WorklistDate)
range(df1912$WorklistDate)

## Combine into annual
df2019 <- rbind(df1901, df1902, df1903, df1904, df1905, df1906,
                df1907, df1908, df1909, df1910, df1911, df1912)

rm(df1901, df1902, df1903, df1904, df1905, df1906,
   df1907, df1908, df1909, df1910, df1911, df1912)


### 2020 ----
df2001 <- import_report(2001)
df2002 <- import_report(2002)
df2003 <- import_report(2003)
df2004 <- import_report(2004)
df2005 <- import_report(2005)
df2006 <- import_report(2006)
df2007 <- import_report(2007)
df2008 <- import_report(2008)
df2009 <- import_report(2009)
df2010 <- import_report(2010)
df2011 <- import_report(2011)
df2012 <- import_report(2012)

## Check monthly ranges
range(df2001$WorklistDate)
range(df2002$WorklistDate)
range(df2003$WorklistDate)
range(df2004$WorklistDate)
range(df2005$WorklistDate)
range(df2006$WorklistDate)
range(df2007$WorklistDate)
range(df2008$WorklistDate)
range(df2009$WorklistDate)
range(df2010$WorklistDate)
range(df2011$WorklistDate)
range(df2012$WorklistDate)

## Combine into annual
df2020 <- rbind(df2001, df2002, df2003, df2004, df2005, df2006,
                df2007, df2008, df2009, df2010, df2011, df2012)

rm(df2001, df2002, df2003, df2004, df2005, df2006,
   df2007, df2008, df2009, df2010, df2011, df2012)


### 2021 ----
df2101 <- import_report(2101)
df2102 <- import_report(2102)
df2103 <- import_report(2103)
df2104 <- import_report(2104)
df2105 <- import_report(2105)
df2106 <- import_report(2106)
df2107 <- import_report(2107)
df2108 <- import_report(2108)
df2109 <- import_report(2109)
df2110 <- import_report(2110)
df2111 <- import_report(2111)
df2112 <- import_report(2112)

## Check monthly ranges
range(df2101$WorklistDate)
range(df2102$WorklistDate)
range(df2103$WorklistDate)
range(df2104$WorklistDate)
range(df2105$WorklistDate)
range(df2106$WorklistDate)
range(df2107$WorklistDate)
range(df2108$WorklistDate)
range(df2109$WorklistDate)
range(df2110$WorklistDate)
range(df2111$WorklistDate)
range(df2112$WorklistDate)

## Combine into annual
df2021 <- rbind(df2101, df2102, df2103, df2104, df2105, df2106,
                df2107, df2108, df2109, df2110, df2111, df2112)

rm(df2101, df2102, df2103, df2104, df2105, df2106,
   df2107, df2108, df2109, df2110, df2111, df2112)


### 2022 ----
df2201 <- import_report(2201)
df2202 <- import_report(2202)
df2203 <- import_report(2203)
df2204 <- import_report(2204)
df2205 <- import_report(2205)
df2206 <- import_report(2206)
df2207 <- import_report(2207)
# df2208 <- import_report(2208)
# df2209 <- import_report(2209)
# df2210 <- import_report(2210)
# df2211 <- import_report(2211)
# df2212 <- import_report(2212)

## Check monthly ranges
range(df2201$WorklistDate)
range(df2202$WorklistDate)
range(df2203$WorklistDate)
range(df2204$WorklistDate)
range(df2205$WorklistDate)
range(df2206$WorklistDate)
range(df2207$WorklistDate)
# range(df2208$WorklistDate)
# range(df2209$WorklistDate)
# range(df2210$WorklistDate)
# range(df2211$WorklistDate)
# range(df2212$WorklistDate)

## Combine into annual
df2022 <- rbind(df2201, df2202, df2203, df2204, df2205, df2206,
                df2207)#, df2208, df2209, df2210, df2211, df2212)

rm(df2201, df2202, df2203, df2204, df2205, df2206,
   df2207)#, df2208, df2209, df2210, df2211, df2212)


#### 3: Check data ####
# Quick check of annual data
### 2018
range(df2018$WorklistDate)
ggplot(df2018, aes(x = WorklistDate)) +
  geom_histogram()

### 2019
range(df2019$WorklistDate)
ggplot(df2019, aes(x = WorklistDate)) +
  geom_histogram()

### 2020
range(df2020$WorklistDate)
ggplot(df2020, aes(x = WorklistDate)) +
  geom_histogram()

### 2021
range(df2021$WorklistDate)
ggplot(df2021, aes(x = WorklistDate)) +
  geom_histogram()

### 2022
range(df2022$WorklistDate)
ggplot(df2022, aes(x = WorklistDate)) +
  geom_histogram()


#### 4: Combine and write out ####
## Create an overall data set
complete <- rbind(df2018, df2019, df2020,
                  df2021, df2022)
range(complete$WorklistDate)


## Export
write_rds(complete, paste0(wd, "/Output/SBSS_R079_historic_", today, ".rds"))



##########################################################
# x_R079_create_historic.R
# Karen Hotopp
# 26/08/2022
# Script x1 of 2
# Call in historic R079 RDS file and create counts of appointments
# allocated v attended
# Completed data file starts at Jan 2018
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


#### 2: Import data ####
brest <- readRDS(paste0(wd, "/Output/SBSS_R079_historic_2022-08-26.rds"))

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
