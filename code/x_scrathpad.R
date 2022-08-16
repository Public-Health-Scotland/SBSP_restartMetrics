##########################################################
# x_scratchpad.R
# Karen Hotopp
# 24/06/2022
# Script 1 of 2
# Checking downloaded reports to weekly sent reports
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

## Packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidylog)

#### Values ####
## Set root directory for screening reports
wd <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
            "/20201203-Breast-Screening-NSOB-Restart-Metrics")
root <-paste0("/PHI_conf/CancerGroup1/Topics/BreastScreening/Investigations",
              "/20201203-Breast-Screening-NSOB-Restart-Metrics",
              "/Weekly Appts Reports/Invite Screen Reports")

## Set folder dates
folder1 <- "20220613"

## Set report dates
report1 <- "13062022"


#### Function ####
## Function sets pathways within each folder, reads in data,
## and outputs combined data from centres
import_folder <- function(folder, report) {
  
  eos_path <- (paste0(root, "/Reports ", folder,
                      "/EastScotland_Report_", report, ".csv"))
  neos_path <- (paste0(root, "/Reports ", folder,
                       "/NorthEastScotland_Report_", report, ".csv"))
  nos_path <- (paste0(root, "/Reports ", folder,
                      "/NorthScotland_Report_", report, ".csv"))
  seos_path <- (paste0(root, "/Reports ", folder,
                       "/SouthEastScotland_Report_", report, ".csv"))
  swos_path <- (paste0(root, "/Reports ", folder,
                       "/SouthWestScotland_Report_", report, ".csv"))
  wos_path <- (paste0(root, "/Reports ", folder,
                      "/WestScotland_Report_", report, ".csv"))
  
  eos <- read.csv(eos_path)
  neos <- read.csv(neos_path)
  nos <- read.csv(nos_path)
  seos <- read.csv(seos_path)
  swos <- read.csv(swos_path)
  wos <- read.csv(wos_path)
  
  
  combined <- bind_rows(eos, neos, nos, seos, swos, wos)
  
}


#### 1: Process weekly sent data ####
sent <- import_folder(folder1, report1)
str(sent)

## Update date data
sent <- sent %>%
  mutate(Worklist.Date = dmy(Worklist.Date)) %>%
  rename(WorklistDate = Worklist.Date,
         WorklistDisplayName = Worklist.Display.Name,
         OrganisationName = Organisation.Name) %>%
  glimpse()

range(sent$WorklistDate)
table(sent$WorklistDate)

## Update variables to match R079 variables
sent <- sent %>%
  rename(WorklistID = WorklistDisplayName,
         Facility = OrganisationName,
         TotalAppts = Total.Appts,
         TotalUnalloc = Total.Free,
         Allocated = Appts,
         DNAReappt = Reappointed,
         CancelledReappt = Cancelled.Reppointed,
         CancelledReused = Cancelled.Reused) %>%
  mutate(Gap = "",
         Free = "",
         Bulk = "",
         WorklistID = as.character(WorklistID)) %>% 
  select(BSC, WorklistID, WorklistDate,
         Site, Facility, TotalAppts, TotalUnalloc,
         #Gap, Free, Bulk,
         Allocated, Attended,
         DNA, DNAReappt,
         Cancelled, CancelledReappt, CancelledReused) %>% 
  arrange(WorklistID) %>% 
  glimpse()


#### 2: Process downloaded (R079) data ####
scot <- read.csv(paste0(root, "/Reports ", folder1, 
                        "/Scotland_R079_202206.csv"))
str(scot)

## Update date data
scot <- scot %>%
  mutate(WorklistDate = dmy(WorklistDate),
         WorklistID = as.character(WorklistID)) %>%
  rename(BSC = BSCName) %>%
  select(-c(Gap, Free, Bulk)) %>% 
  arrange(WorklistID) %>% 
  glimpse()

range(scot$WorklistDate)
table(scot$WorklistDate)


## Data sets have different variable names and variables are in a different
## order, so not able to align exactly; also, 'sent' has 14 variables
## when first imported into R, but 'scot' has 17.
## Check that variable names match
names(sent)
names(scot)


#### 3: Compare two data sets ####
library(waldo)
compare(sent, scot)

## Nested way to find differences in specific variables...
compare(sort(levels(sent$Facility)), sort(levels(scot$Facility)))
compare(sort(levels(sent$Site)), sort(levels(scot$Site)))
compare(sort(levels(sent$WorklistID)), sort(levels(scot$WorklistID)))

library(arsenal)
summary(comparedf(sent, scot))


setdiff(sent, scot)


## Both provide good detail on what is missing, but datasets don't align;
## need to remove 14 extra records in 'scot'
## problem could also be that I can't seem to get 'sent' to properly 
## arrange by 'sent$WorklistID'


## Use dplyr::anti_join to pull out extra 14 files
extra_obs <- anti_join(scot, sent, by="WorklistID")

# Remove extra observations from the scot
scot1 <- anti_join(scot, extra_obs, by="WorklistID")

matched_obs <- inner_join(scot1, sent) # only 20 observations match!

setdiff(sent, scot1) # 234 observations not matching (confirm above)


#### 4: Screening Centres ####
table(sent$BSC)
table(scot$BSC)


### East of Scotland ----
sent_e <- sent[sent$BSC == "East of Scotland",]
scot_e <- scot[scot$BSC == "East of Scotland",]

compare(sent_e, scot_e)
summary(comparedf(sent_e, scot_e))

write.csv(sent_e, paste0(wd, "/Checks/R079_EoS_sent.csv"))
write.csv(scot_e, paste0(wd, "/Checks/R079_EoS_scot.csv"))



























