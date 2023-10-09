# DataPrep.R
library("readxl")
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
  rename(c("Site" = "SUBSITE", "Count" = "COUNT", "Age" = "AGE", "Date"= "DATE"))


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

# newer data
X1997_2022_Phocadata <- X1997_2022_Phocadata %>%
  rename(c("Site" = "Subsite"))
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


# deal with dead pups and dead adults 
# Add in deadpups (represent productivity and deadadults
PhocaData <- filter(PhocaData, Age != "DEADADULT" )

## get top of each year*season
PhocaData$Year <- year(PhocaData$Date)
PhocaData <- PhocaData %>% relocate("Year", .before = "Date")



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
  names_from = c(Age),
  values_from = Count)
PhocaData_Wide # for analysis of pup:adult ratios

ggplot(PhocaData, aes(x = Date, y = Count, color = Age)) +
  geom_point(alpha = 0.1) +
  geom_smooth( level = 0.8) +
  facet_grid(Season~Site)


## get top of each year*season
PhocaData$Year <- year(PhocaData$Date)

top1 <- as_tibble(PhocaData) %>% 
  group_by(Site, Year, Age, Season) %>%
  top_n(n = 1, wt = Count)

#sum to look at total population change
top1_sum <- top1 %>%
  distinct(Year, Site, Season, Age, .keep_all = TRUE) %>% #remove any duplicate rows
  select(-Date) %>%
  group_by(Year, Age, Season) %>%
  dplyr::summarize(Total = sum(Count)) 
top1_sum

ggplot(top1_sum, aes(x = Year, y = Total, color = Age)) +
  geom_point() +
  geom_smooth() +
  facet_grid(.~Season)

#for site analyses
top1_wide <- top1 %>% 
  distinct(Year, Site, Season, Age, .keep_all = TRUE) %>% #remove any duplicate rows
  select(-Date) %>%
  pivot_wider(
    names_from = c(Age),
    values_from = Count)
top1_wide # for analysis of pup:adult ratios


ggplot(top1, aes(x = Year, y = Count, color = Age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(span = 1) +
  #xlim(1997,2022) +
  facet_grid(Season~Site)


##########################
## let's add in the covariates
# BEAUTI, MOCI, MEI, NPGO, Disturbance, Coyote
##########################

# BEAUTI
BEUTI_monthly <- read_csv("Data/BEUTI_monthly.csv")
BEUTI_monthly
#View(BEUTI_monthly) #starting in 1988, use 38N
#use Feb-May data
BEUTI_2_5 <- 
  BEUTI_monthly %>%
      select(year, month, `38N`) %>%
          filter(month >= 2 & month <=5) %>%
              group_by(year) %>%
                dplyr::summarize(Spring_BEUTI = mean(`38N`)) #remember conflicts between dplyr and plyr!
BEUTI_2_5 <- BEUTI_2_5 %>%
  rename(c("Year" = "year")) 

BEUTI_2_5  #ready

#MOCI

MOCI <- read_csv("Data/CaliforniaMOCI.csv")
MOCI[,-1]

MOCI_Nor_Cen <- 
  MOCI %>%
  select(`Year`, `Season`, `North California (38-42N)`, `Central California (34.5-38N)`) %>%
  filter(Season == "JFM" | Season == "AMJ") %>%
  group_by(Year, Season) %>%
  dplyr::summarize(MOCI_Nor_Cen = (`North California (38-42N)` + `Central California (34.5-38N)` / 2)) %>%
  pivot_wider(names_from = c(Season),
                values_from = (MOCI_Nor_Cen))
MOCI_Nor_Cen

MOCI_Nor_Cen <- MOCI_Nor_Cen %>%
  rename(c("MOCI_AMJ" = "AMJ", "MOCI_JFM" = "JFM")) %>%  #rename
  relocate("MOCI_AMJ", .after = "MOCI_JFM")             #move colums
    
MOCI_Nor_Cen #ready

#MEI
MEI <- read_excel("Data/2023_Analysis/MEI.xlsx")
MEI

MEI <- MEI %>% select("YEAR", "DJ",  "JF", "FM", "MA", "AM", "MJ")

MEI <- MEI %>%
  rename(c("MEI_DJ" = "DJ", "MEI_JF" = "JF",  
           "MEI_FM" = "FM", "MEI_MA" = "MA",    
           "MEI_AM" = "AM", "MEI_MJ" = "MJ")) 
MEI <- MEI %>%
  rename(c("Year" = "YEAR")) 
MEI #ready

#NPGO
NPGO <- read_excel("Data/2023_Analysis/NPGO.xlsx")
NPGO #we'll use months 2-5

NPGO_2_5 <- 
  NPGO %>%
  filter(MONTH >= 2 & MONTH <=5) %>%
  group_by(YEAR) %>%
  dplyr::summarize(Spring_NPGO = mean(NPGO)) #remember conflicts between dplyr and plyr!

NPGO_2_5 <- NPGO_2_5 %>%
  rename(c("Year" = "YEAR")) 
NPGO_2_5  #ready


#Disturbance
#need to combine older and newer disturbance data

PHOCA_DISTURBANCE_1983_1996_99 <- read_excel("Data/2023_Analysis/PHOCA DISTURBANCE 1983 and 1996-99 FINAL 020223.xlsx")
PHOCA_DISTURBANCE_1983_1996_99

PHOCA_DISTURBANCE_1983_1996_99 <- PHOCA_DISTURBANCE_1983_1996_99[,-c(6, 7)]
PHOCA_DISTURBANCE_1983_1996_99$NumberOfDisturbances <- as.numeric(PHOCA_DISTURBANCE_1983_1996_99$NumberOfDisturbances)

PHOCA_DISTURBANCE_1983_1996_99 <- PHOCA_DISTURBANCE_1983_1996_99 %>%
  rename(c("Season" = "Seaon")) #rename


PHOCA_DISTURBANCE_2000To2022 <- read_excel("Data/2023_Analysis/DisturbanceRateBySeasonBySite_2000To2022.xlsx")
PHOCA_DISTURBANCE_2000To2022

PHOCA_DISTURBANCE_2000To2022 <- PHOCA_DISTURBANCE_2000To2022 %>%
  rename(c("Year" = "YEAR")) #rename

PHOCA_DISTURBANCE_2000To2022 <- PHOCA_DISTURBANCE_2000To2022[,-c(6, 7, 9)]

#stack 'em

Phoca_Disturbance <- rbind(PHOCA_DISTURBANCE_1983_1996_99, PHOCA_DISTURBANCE_2000To2022)
Phoca_Disturbance$DisturbanceRate <- Phoca_Disturbance$NumberOfDisturbances / Phoca_Disturbance$NSurveys
Phoca_Disturbance <- Phoca_Disturbance[,-c(2,4)] #remove season and SiteName...all are breeding
Phoca_Disturbance <- Phoca_Disturbance %>%
  rename(c("Site" = "SiteCode")) 
Phoca_Disturbance$Site <- ifelse(Phoca_Disturbance$Site == "PRH", "PR", Phoca_Disturbance$Site)

Phoca_Disturbance #Ready

#Coyote
Coyote <- read_excel("Data/2023_Analysis/CoyoteSightings.xlsx")
Coyote 

Coyote$Site <- ifelse(Coyote$Site == "PRH", "PR", Coyote$Site)

Coyote$CoyoteDays <- Coyote$DaysWithSightings / Coyote$Surveys
hist(Coyote$CoyoteDays)  #Ready

#Join covariates to top1_wide

# BEUTI_2_5
# MOCI_Nor_Cen
# NPGO_2_5
# MEI
# Phoca_Disturbance
# Coyote

top1_wide

top1_wide_A <- left_join(top1_wide, BEUTI_2_5, by = "Year")
top1_wide_A

top1_wide_B <- left_join(top1_wide_A, MOCI_Nor_Cen, by = "Year")
top1_wide_C <- left_join(top1_wide_B, NPGO_2_5, by = "Year")
top1_wide_D <- left_join(top1_wide_C, MEI, by = "Year")
top1_wide_E <- left_join(top1_wide_D, Phoca_Disturbance, by = c("Year", "Site"))
top1_wide_F <- left_join(top1_wide_E, Coyote, by = c("Year", "Site"))
#View(top1_wide_F)


top1_wide <- top1_wide_F #ready for GLMMS


## Prep data for MARSS -----------------
top1_wide
top1_wide_wide_ADULT <- top1_wide %>% 
  select(Year, Site, Season, ADULT) %>%
  distinct(Year, Site, Season, .keep_all = TRUE) %>% #remove any duplicate rows
  pivot_wider(
    names_from = c(Site),
    values_from = c(ADULT)
                    )

top1_wide_wide_ADULT


top1_wide_wide_ADULT_Molt <- top1_wide_wide_ADULT %>%
  select(Year, Season, DP, DE, PR, BL, TP, TB, DR, PB) %>%
  filter(Season == "Molting")

top1_wide_wide_ADULT_Molt_2020 <- top1_wide_wide_ADULT %>%
  select(Year, Season, DP, DE, PR, BL, TP, TB, DR, PB) %>%
  filter(Season == "Molting") %>%
  filter(Year >=2000)



top1_wide_wide_ADULT_Breeding <- top1_wide_wide_ADULT %>%
  select(Year, Season, DP, DE, PR, BL, TP, TB, DR, PB) %>%
  filter(Season == "Breeding")

## end prep MARSS data-----------





#glmms ------------------------------------------

library(lme4)
library(sjPlot)

m1.ADULT <- glmer(ADULT ~ 
                scale(Year) *
                Site + 
               Season +
               #scale(Spring_BEUTI) +
               #scale(MOCI_AMJ) +
               scale(Spring_NPGO) +
               #scale(MEI) +
               scale(DisturbanceRate) +
               scale(CoyoteDays) +
               (Site||Year) + 
               (1|Season), 
                 family = negative.binomial(1), data = top1_wide)
summary(m1.ADULT)
sjPlot::plot_model(m1.ADULT, type = "eff") 
plot_model(m1.ADULT, type = "eff", terms = "CoyoteDays [all]") #+ ylim(0.28,0.35)
plot_model(m1.ADULT, type = "resid")
plot_model(m1.ADULT) + ylim(0.1,3.5) 
plot_model(m1.ADULT, type = "int")
plot(m1.ADULT)
plot(m1.ADULT)


unique(top1_wide$Site)

#for pup analysis, select only breeding season and use main 5 colonies


target <- c("DP", "DE", "BL", "TP", "TB")


top1_wide_pup <- top1_wide %>%
                 filter(Season == "Breeding") %>%
                           filter(Site %in% target) %>%
                              filter(!is.na(PUP))  # were 3 NA in the pups
                                 # remove NA
#lazy filtering of NAs
top1_wide_pup <- top1_wide_pup %>% filter(!is.na(DisturbanceRate))
top1_wide_pup <- top1_wide_pup %>% filter(!is.na(CoyoteDays))

unique(top1_wide_pup$Year)
unique(top1_wide_pup$Site)
unique(top1_wide_pup$Spring_NPGO)
unique(top1_wide_pup$DisturbanceRate)
unique(top1_wide_pup$CoyoteDays)


ggplot(top1_wide_pup, aes(x = Year, y = PUP/ADULT)) +
  geom_point(alpha = 0.5) +
  geom_smooth(span = 1) +
  ylim(0,1) +
  facet_grid(~Site)




m1.PUP <- glmer(PUP ~ 
                    scale(Year) + 
                    Site + 
                    #Season +
                    #scale(Spring_BEUTI) +
                    #scale(MOCI_AMJ) +
                    scale(Spring_NPGO) +
                    #scale(MEI) +
                    scale(DisturbanceRate) +
                    scale(CoyoteDays) +
                    (1|Site), 
                  family = negative.binomial(1), data = top1_wide_pup)
summary(m1.PUP)
sjPlot::plot_model(m1.PUP, type = "eff") 
plot(m1.PUP)



m1.PUP.ratio <- glmer(cbind(PUP, ADULT) ~ 
                        scale(Year) + 
                        Site +
                  #Spring_BEUTI +
                  #MOCI_AMJ +
                  Spring_NPGO + 
                  #MEI + 
                  scale(DisturbanceRate) +
                  scale(CoyoteDays) +
                  (Year||Site), family = binomial,# negative.binomial(1),  #poisson overdispersed
                  #data = top1_wide)
                  data = subset(top1_wide_pup))

summary(m1.PUP.ratio)

plot_model(m1.PUP.ratio, type = "eff")
plot_model(m1.PUP.ratio, type = "eff", terms = "CoyoteDays [all]") #+ ylim(0.28,0.35)
plot_model(m1.PUP.ratio, type = "resid")
plot_model(m1.PUP.ratio) + 
  ylim(0.49,1.2) + 
  geom_hline(yintercept = 1, lty = 2)
plot_model(m1.PUP.ratio, type = "pred", terms = c("neg_c_7", "c172code"))
plot(m1.PUP.ratio)


## gamm
library(mgcv)
m1.gamm <- gamm(cbind(PUP, ADULT) ~ 
       s(Year) + 
       Site +
       #s(Spring_BEUTI) +
       #s(MOCI_AMJ) +
       s(Spring_NPGO) + 
       s(scale(CoyoteDays)),
       random=list(Site=~1), 
       family = binomial, 
     #data = top1_wide)
     data = subset(top1_wide_pup, Season == "Breeding"))
plot(m1.gamm$gam)



m1.gamm <- gamm((PUP/ADULT) ~ 
                  s(Year) + 
                  Site +
                  #s(Spring_BEUTI) +
                  #s(MOCI_AMJ) +
                  s(Spring_NPGO) + 
                  s(scale(CoyoteDays)),
                random=list(Site=~1, Year=~1), 
                family = Gamma, 
                #data = top1_wide)
                data = subset(top1_wide, Season == "Breeding"))

plot(m1.gamm$gam,pages=1)
summary(m1.gamm$gam)
sjPlot::plot_model(m1.gamm$gam)
plot_model(m1.gamm, type = "pred")


adult.stan.glmm <- stan_glmer(ADULT ~ 
                                Year + 
                                Site +
                                Spring_BEUTI +
                                MOCI_AMJ +
                                Spring_NPGO + 
                                CoyoteDays + 
                              (1|Site), 
                              family = neg_binomial_2, 
                              #data = top1_wide)
                              data = subset(top1_wide, Season == "Breeding"),
                              chains = 3, iter = 1000)

summary(adult.stan.gamm, digits = 3)
plot_model(adult.stan.gamm, type = "pred")





## rstan
library(rstanarm)
library(bayesplot)

adult.stan.gamm <- stan_gamm4(ADULT ~ 
                              s(Year) + 
                              Site +
                              #s(Spring_BEUTI) +
                              s(MOCI_AMJ) +
                              #Spring_NPGO + 
                              s(CoyoteDays),
                            random= ~(1|Site), 
                            family = neg_binomial_2, 
                            #data = top1_wide)
                            data = subset(top1_wide_pup),
                            chains = 3, iter = 1000)

summary(adult.stan.gamm, digits = 3)
plot_model(adult.stan.gamm, type = "pred")



pup.stan.gamm <- stan_gamm4(PUP ~ 
                  s(Year) + 
                  #Site +
                  s(Spring_BEUTI) +
                  #MOCI_AMJ +
                  #Spring_NPGO + 
                  s(CoyoteDays),
                random= ~(1|Site), 
                family = neg_binomial_2, 
                #data = top1_wide)
                data = subset(top1_wide, Season == "Breeding"),
                chains = 3, iter = 1000)

summary(pup.stan.gamm, digits = 3)

plot_model(pup.stan.gamm)




ratio.stan.gamm <- stan_gamm4(cbind(PUP, ADULT) ~ 
                              s(Year) + 
                              Site +
                              s(Spring_BEUTI) +
                              s(MOCI_AMJ) +
                              s(Spring_NPGO) + 
                              CoyoteDays,
                            random= ~(1|Site), 
                            family = binomial, 
                            #data = top1_wide)
                            data = subset(top1_wide_pup),
                            chains = 1, iter = 1000, thin = 2, adapt_delta = 0.96)

summary(ratio.stan.gamm, digits = 3)
plot_model(ratio.stan.gamm, type = "pred")
plot_nonlinear(ratio.stan.gamm, smooths = "s(Spring_NPGO)", alpha = 2/3)

p.ratio.stan.gamm <- plot_nonlinear(ratio.stan.gamm, group = top1_wide$Site, prob = 0.8) #+



CHAINS <- 1
CORES <- 16
SEED <- 123
ITER <- 1000
WARMUP <- 400
THIN <- 2

#PRIOR <- normal(-0.022, 0.007)
PRIOR <- normal(0, 0.05)

ADULT.stan.gamm <- stan_gamm4(ADULT) ~ 
    s(scale(Year) + 
    Site + 
    #s(Spring_BEUTI) + 
    s(Spring_NPGO) + 
    s(DisturbanceRate) +
    s(scale(CoyoteDays)),
  random = ~ ~(1|Site) + (1|Year), 
  data = subset(top1_wide_pup), 
  family = binomial, 
  prior = PRIOR, chains = CHAINS, cores = CORES, iter = ITER, thin = THIN, warmup = WARMUP, 
  adapt_delta = 0.97)


## pup stan glmm
## 2 min ART

PUP.stan.glmm <- stan_glmer(PUP ~ 
                                scale(Year) + 
                                Site + 
                                #s(Spring_BEUTI) + 
                                scale(Spring_NPGO) + 
                                scale(DisturbanceRate) +
                                scale(CoyoteDays) +
                               # (Year||Site), # + #uncorrelated intercepts and slopes among sites
                               (1|Site) + (0+Year|Site),
                              data = top1_wide_pup,
                              family = neg_binomial_2,
                              prior = PRIOR, chains = CHAINS, cores = CORES, iter = ITER, 
                              thin = THIN, warmup = WARMUP, adapt_delta = 0.98)

beepr::beep(4)

summary(PUP.stan.glmm, digits = 3)
performance::r2(PUP.stan.glmm)

theme_set(theme_grey())  ## make ggplot look good
color_scheme_set("brightblue")  # best = brightblue and pink
ppc_intervals_grouped(
  y = top1_wide_pup$PUP,
  yrep = posterior_predict(PUP.stan.glmm),
  x = top1_wide_pup$Year, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "Year", 
    y = "Pup Count"
  ) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  geom_smooth(method = "gam", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600, 100)) 


ppc_intervals_grouped(
  y = top1_wide_pup$PUP,
  yrep = posterior_predict(PUP.stan.glmm),
  x = top1_wide_pup$CoyoteDays, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "CoyoteDays", 
    y = "Pup Count"
  ) +
  xlim(0, 0.4) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  geom_smooth(method = "gam", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600, 100)) 







## ADULT stan glmm  
## not running well  REDO
## problems when random slopes (Year|Site) or (Year||Site)

ADULT.stan.glmm <- stan_glmer(ADULT ~ 
                              scale(Year) * 
                              Site + 
                              #s(Spring_BEUTI) + 
                              scale(Spring_NPGO) + 
                              scale(DisturbanceRate) +
                              scale(CoyoteDays) +
                              # (Year||Site), # + #uncorrelated intercepts and slopes among sites
                              (1|Site) + 
                                (1|Year),
                            data = top1_wide_pup,
                            family = neg_binomial_2,
                            prior = PRIOR, chains = CHAINS, cores = CORES, iter = ITER, 
                            thin = THIN, warmup = WARMUP, adapt_delta = 0.96)



beepr::beep(4)

summary(ADULT.stan.glmm, digits = 3)
performance::r2(ADULT.stan.glmm)


## ADULT YEAR
theme_set(theme_grey())  ## make ggplot look good
color_scheme_set("brightblue")  # best = brightblue and pink
ppc_intervals_grouped(
  y = top1_wide_pup$ADULT,
  yrep = posterior_predict(ADULT.stan.glmm),
  x = top1_wide_pup$Year, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "Year", 
    y = "Adult Breeding Season Count"
  ) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  geom_smooth(method = "gam", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200, 200)) 

## ADULT COYOTE
theme_set(theme_grey())  ## make ggplot look good
color_scheme_set("brightblue")  # best = brightblue and pink
ppc_intervals_grouped(
  y = top1_wide_pup$ADULT,
  yrep = posterior_predict(ADULT.stan.glmm),
  x = top1_wide_pup$CoyoteDays, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "Coyote sightings per survey", 
    y = "Adult Breeding Season Count"
  ) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  geom_smooth(method = "lm", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200, 200)) +
  xlim(0, 0.4)


## ADULT NPGO
theme_set(theme_grey())  ## make ggplot look good
color_scheme_set("brightblue")  # best = brightblue and pink
ppc_intervals_grouped(
  y = top1_wide_pup$ADULT,
  yrep = posterior_predict(ADULT.stan.glmm),
  x = top1_wide_pup$Spring_NPGO, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "NPGO", 
    y = "Adult Breeding Season Count"
  ) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  geom_smooth(method = "lm", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200, 200))








## pup RATIO stan glmm
## 4 min ART with random slopes

ratio.stan.glmm <- stan_glmer(cbind(PUP, ADULT) ~ 
                                scale(Year) * 
                                Site + 
                                #s(Spring_BEUTI) + 
                                scale(Spring_NPGO) + 
                                scale(DisturbanceRate) +
                                scale(CoyoteDays) +
                                (Year||Site), 
                                  data = top1_wide_pup, 
                                  family = binomial, 
                                  prior = PRIOR, chains = CHAINS, cores = CORES, iter = ITER, thin = THIN, warmup = WARMUP, 
                                  adapt_delta = 0.97)
save(ratio.stan.glmm, file = "ratio.stan.glmm.RData")


beepr::beep(6)
  
summary(ratio.stan.glmm, digits = 3)
performance::r2(ratio.stan.glmm)
plot_model(ratio.stan.glmm, type = "pred")


theme_set(theme_grey())  ## make ggplot look good
color_scheme_set("brightblue")  # best = brightblue and pink
ppc_intervals_grouped(
  y = (top1_wide_pup$PUP/top1_wide_pup$ADULT),
  yrep = posterior_linpred(ratio.stan.glmm, transform = TRUE),  #use posterior_epred() for binomial back transform
  x = top1_wide_pup$Year, 
  group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
  #facet_args = list(ncol = 2)
) +
  labs(
    x = "Year", 
    y = "Ratio"
  ) +
  #panel_bg(fill = "gray95", color = NA) +
  #grid_lines(color = "white") +
  #geom_smooth(method = "gam", se = TRUE, span = 10) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .25)) # +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  
  #geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = "none") +
  theme_grey(base_size = 16)


  ## coyote 
  ppc_intervals_grouped(
    y = (top1_wide_pup$PUP/top1_wide_pup$ADULT),
    yrep = posterior_linpred(ratio.stan.glmm, transform = TRUE),  #use posterior_epred() for binomial back transform
    x = top1_wide_pup$CoyoteDays, 
    group = top1_wide_pup$Site #paste(CSIA_Joined$Species, "-", CSIA_Joined$SeasonGrown), #CSIA_Joined$Species,
    #facet_args = list(ncol = 2)
  ) +
    labs(
      x = "CoyoteDays", 
      y = "Ratio"
    ) +
    #panel_bg(fill = "gray95", color = NA) +
    #grid_lines(color = "white") +
    #geom_smooth(method = "gam", se = TRUE, span = 10) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .25)) + # +
  scale_x_continuous(limits = c(0, 0.4))# +
    
    #geom_hline(yintercept = 0, lty = 2) +
    theme(legend.position = "none") +
    theme_grey(base_size = 16)  
  
  







                    
                    
                  



