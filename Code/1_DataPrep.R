# DataPrep.R
library("readxl")
library(dplyr)
library(tidyverse)
X1997_2022_Phocadata <- read_excel("Data/1997_2022_Phocadata.xls", 
                                   col_types = c("date", "text", "numeric", 
                                                 "numeric", "skip", "text", "numeric", 
                                                 "skip", "skip", "skip"))
X1997_2022_Phocadata
## now add the older phoca data

pupping_season_1976_1999_010523 <- read_excel("Data/2023_Analysis/HARBOR SEALS  POINT REYES 1976-1999 010523.xlsx", 
                                                        sheet = "ready_PUPPING SURVEYS", col_types = c("text", 
                                                                                                       "date", "text", "text", "date", "date", 
                                                                                                       "text", "text", "skip", "skip"))

molting_season_1976_1999_010523 <- read_excel("Data/2023_Analysis/HARBOR SEALS  POINT REYES 1976-1999 010523.xlsx", 
                                                        sheet = "ready_MOLTING SURVEYS")
## need to spread

molt_1976_1999 <- molting_season_1976_1999_010523 %>%
  pivot_longer(cols = c("DP", "DE",    "BL",    "TP",    "TB",   "PRH"),
              names_to = "Site",
              values_to = "Count") 
molt_1976_1999$Season = "Molting"
molt_1976_1999$Age = "Adult"
molt_1976_1999  # ready!

## removed the mold season (June-July Counts from this file in excel)
pup_season_1976_1999 <- pupping_season_1976_1999_010523[c(2, 3, 7, 8)]
pup_season_1976_1999$Season <- "Breeding"

pup_season_1976_1999 <- pup_season_1976_1999 %>%
  rename(c("Site" = "SUBSITE", "Count" = "COUNT", "Age" = "AGE", "Date" = "DATE"))
#move column names for stacking
pup_season_1976_1999 <- pup_season_1976_1999 %>% relocate("Age", .after = "Season")

View(pup_season_1976_1999)

all_season_1976_1999 <- rbind(pup_season_1976_1999, molt_1976_1999)
View(all_season_1976_1999)
#looks good

#now merge with 1997-2022 data




