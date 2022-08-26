##########################################################
# x_R079_remake.R
# Karen Hotopp
# 26/08/2022
# Script x1 of 2
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
#library(here)
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


















df2018 <- rbind(df1801, df1802, , df1804, df1805, df1806,
                df1807, df1808, df1809, df1810, df1811, df1812)














