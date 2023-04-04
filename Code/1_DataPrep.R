# DataPrep.R
library("readxl")
library(dplyr)
library(tidyverse)
library(lubridate)
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

#View(molting_season_1976_1999_010523)

## need to spread

molt_1976_1999 <- molting_season_1976_1999_010523 %>%
  pivot_longer(cols = c("DP", "DE",    "BL",    "TP",    "TB",   "PRH"),
              names_to = "Site",
              values_to = "Count") 
molt_1976_1999$Season = "Molting"
molt_1976_1999$Age = "Adult"
molt_1976_1999  # ready!

## removed the molt season (June-July Counts from this file in excel)
pup_season_1976_1999 <- pupping_season_1976_1999_010523[c(2, 3, 7, 8)]
pup_season_1976_1999$Season <- "Breeding"

# pup_season_1976_1999 <- pup_season_1976_1999 %>%
#   rename(c("Site" = "SUBSITE", "Count" = "COUNT", "Age" = "AGE", "Date" = "DATE"))

pup_season_1976_1999 <- pup_season_1976_1999 %>%
  rename(c("SUBSITE" = "Site", "COUNT" = "Count", "AGE" = "Age", "DATE"= "Date"))


#move column names for stacking
pup_season_1976_1999 <- pup_season_1976_1999 %>% relocate("Age", .after = "Season")

#View(pup_season_1976_1999)
#View(all_season_1976_1999)


all_season_1976_1999 <- rbind(pup_season_1976_1999, molt_1976_1999)
#View(all_season_1976_1999)
# Remove data after 1995 because in newer database.
all_season_1976_1999$Year <- year(all_season_1976_1999$Date)
all_season_1976_1995 <- all_season_1976_1999 %>%  filter(all_season_1976_1999$Year < 1996)
all_season_1976_1995 <- all_season_1976_1995[,-6] #remove year
all_season_1976_1995$Tide_Level <- NA
all_season_1976_1995 <- all_season_1976_1995 %>% relocate("Tide_Level", .after = "Site")




X1997_2022_Phocadata <- X1997_2022_Phocadata %>%
  rename(c("Subsite" = "Site"))
#add season
X1997_2022_Phocadata$Month <- month(X1997_2022_Phocadata$Date)
X1997_2022_Phocadata$Season <- ifelse(X1997_2022_Phocadata$Month < 6, "Breeding", "Molt")
X1997_2022_Phocadata <- X1997_2022_Phocadata[,-7]

X1997_2022_Phocadata <- X1997_2022_Phocadata %>% relocate("Age", .after = "Season")
X1997_2022_Phocadata <- X1997_2022_Phocadata[,-4] #remove tide time

## combine data
PhocaData <- rbind(all_season_1976_1995, X1997_2022_Phocadata)
#View(PhocaData)
## get ages consistenf
PhocaData$Age <- ifelse(PhocaData$Age == "AD", "ADULT", PhocaData$Age)
PhocaData$Age <- ifelse(PhocaData$Age == "Adult", "ADULT", PhocaData$Age)
PhocaData$Season <- ifelse(PhocaData$Season == "Molt", "Molting", PhocaData$Season)
PhocaData$Count <- as.numeric(PhocaData$Count)
PhocaData$Date <- as_date(PhocaData$Date)
#PhocaData$Age <- as.factor(PhocaData$Age)
#PhocaData$Season <- as.factor(PhocaData$Season)
#PhocaData$Site <- as.factor(PhocaData$Site)
PhocaData$Site <- ifelse(PhocaData$Site == "PRH", "PR", PhocaData$Site)

#remove counts = NA
PhocaData <- PhocaData %>% filter(!is.na(Count))
#remove Tide_level
PhocaData <- PhocaData[,-3]


# remove dead pups and dead adults
PhocaData <- filter(PhocaData, Age != "DEADPUP" & Age != "DEADADULT" )
#histogram
ggplot(PhocaData, aes(Count)) +
                       geom_histogram() + facet_grid(Site~Age)
           


t1 <- PhocaData %>%
  dplyr::group_by(Date, Site, Season, Age) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
t1

#View(PhocaData)

#pivot wider
PhocaData_Wide <- PhocaData %>% 
  distinct(Date, Site, Season, Age, .keep_all = TRUE) %>% #remove any duplicate rows
  pivot_wider(
  names_from = c(Site, Age),
  values_from = Count)
PhocaData_Wide



ggplot(PhocaData, aes(x = Date, y = Count, color = Age)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_grid(Season~Site)


