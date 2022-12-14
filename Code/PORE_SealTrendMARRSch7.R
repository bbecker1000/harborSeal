##edited code from MARSS UserGuide chapter 7

library("MARSS")
library(ggplot2)
library("readxl")

## B in
#  terms of the interaction strengths between species; bij
#  equals dfi /dXj, the change in the log population growth
#  rate of species i with respect to changes in the log
#  population abundance of species j. T

##%######################################################%##
#                                                          #
####                   data wrangling                   ####
#                                                          #
##%######################################################%##

## get PRNS data
Phoca <- read_excel("Data/1997_2019_Phocadata.xls")

Phoca <- Phoca[-c(3:4, 8:10)]

## need some year and julian date fields to plot by year and day of year
library(lubridate)
## tell lubridate the format
Phoca$Date2 <- ymd(Phoca$Date) #separates out month,day and year
## add year column for faceting plots
Phoca$Year <- year(Phoca$Date2)
## get Julian Date (yday) for within year dates
Phoca$Julian <- yday(Phoca$Date2)

## Looks like need to remove Dead adult and deadpup
library(plyr)
library(dplyr)

Phoca <- subset(Phoca, Age != "DEADPUP" | Age != "DEADADULT")

## and need to convert HPUP (typo in database!) to PUP
Phoca$Age[Phoca$Age == "HPUP"] <- "PUP"
Phoca$Yearf <- as.factor(Phoca$Year)

## now within year plots to look at breeding and molting season peaks
Phoca.Adult <- dplyr::filter(Phoca, Age == "ADULT")

seasonal.plot.adult <- plot.breeding <- ggplot(Phoca.Adult, aes(Julian, Count, colour = Year)) + geom_point() + geom_smooth(aes(colour = Year))
seasonal.plot.adult + facet_grid(. ~ Subsite)

## previous hard to see patterns due to molt and pups becoming big(= adults), so just look at pups?
Phoca.PUP <- dplyr::filter(Phoca, Age == "PUP")
## subset to the peak seal breeding season about April 20 - May 10
# (Silas) should this be April 15 to May 15 or April 20 to May 10??  right now using wider interval
Phoca.breeding <- dplyr::filter(Phoca, Julian > 105 & Julian < 135)

if (FALSE) { #remove this line and 80 to get this plot again
d1 <- Phoca.PUP %>% 
  group_by(Subsite, Year) %>% 
  summarise_each(funs(MaxCount = max), Count)
d1

## julian is meaningless here, just for practice  
d2 <- Phoca.PUP %>% 
  group_by(Subsite, Year) %>% 
  summarise_each(funs(MaxJulian = max), Julian)
d2
## combine d1 and d2
d3 <- dplyr::bind_cols(d2, d1)
d3
## rename duplicate rows
## need to fix this... must have been a dplyr update
d3 <- d3[ -c(4:5)]
d3
d3 <- rename(d3, Subsite = Subsite...1) 
d3 <- rename(d3, Year = Year...2)
## plot just the max
p1<-ggplot(d3, aes(Year, MaxCount)) +
  geom_point() +
  geom_smooth(se = TRUE)
p1 + facet_grid(. ~ Subsite) 
}

Phoca.Adult.Breed <- dplyr::filter(Phoca.Adult, Julian > 105 & Julian < 135) #April 15 to May 15

Phoca.breed.seas <- dplyr::filter(Phoca, Julian > 105 & Julian < 135)
top1 <- tbl_df(Phoca.breed.seas) %>% 
  group_by(Subsite, Yearf, Age) %>%
  top_n(n = 1, wt = Count)

##remove any duplicate rows (some problem in the above queries!)
top1 <- dplyr::distinct(top1)

#(silas) pr and pb missing lines connecting the dots 
plot.top1 <- ggplot(top1, aes(Year, Count, shape = Age, colour = Age)) + 
  geom_point() + 
  geom_smooth()
plot.top1 + facet_wrap(~ Subsite) + labs(title = "Top 1 Data Point") #PB and PR don't have line; remove deadpup??
#ggsave("plot.top1.jpg", width = 8, height = 6, units = "in")

## now make 3 files of "top 1 for breeding season for pups, adults, and pups + adults (later)
top1.adult.breed <- dplyr::filter(top1, Age == "ADULT")
top1.pup.breed <- dplyr::filter(top1, Age == "PUP")
## MARSS only needs year, site and count
## dplyr:filter won't let me filter out fYear, so use base R
myvars <- c("Year", "Subsite", "Count")
top1.adult.breed <- top1.adult.breed[myvars]
top1.pup.breed <- top1.pup.breed[myvars]

## remove Point Bonita and Duxbury since no pups and some duplicates in the dataset!
top1.adult.breed <- subset(top1.adult.breed, Subsite != "DR" & Subsite != "PB")
top1.pup.breed <- subset(top1.pup.breed, Subsite != "DR" & Subsite != "PB")
## and for the trial runs, lets get rid of PRH
#(Silas) should we add this back in ???
top1.adult.breed <- dplyr::filter(top1.adult.breed, Subsite != "PR")
top1.pup.breed <- dplyr::filter(top1.pup.breed, Subsite != "PR")

# make sure no duplicate rows
top1.adult.breed <- dplyr::distinct(top1.adult.breed)
top1.pup.breed <- dplyr::distinct(top1.pup.breed)

## log all the counts, # log = ln in R
top1.adult.breed$Count <- log(top1.adult.breed$Count) 
top1.pup.breed$Count <- log(top1.pup.breed$Count)

## now need to make the data wide for MARSS
top1.adult.breed.spread <- tidyr::spread(top1.adult.breed, Subsite, Count)
top1.pup.breed.spread <- tidyr::spread(top1.pup.breed, Subsite, Count)

dat2 <- top1.adult.breed.spread ## for use in SealPopStructure Script

## now use names in the basic MARSS code 
dat <- t(top1.adult.breed.spread)
## OR FOR PUPS
## dat <- t(top1.pup.breed.spread)


##########################################################################
### 8.3 Single population model with independent and (non)identical errors
##########################################################################
#Code to fit the single population model with i.i.d. errors
#Read in data
#Transpose since MARSS needs time ACROSS columns
years = dat[1,]     # remove years
n = nrow(dat)-1
dat = dat[2:nrow(dat),]
legendnames = (unlist(dimnames(dat)[1]))

########################################################################

## Parameter key ########
## R = observation errors          equal
## U = growth parameter            unequal  
## Z = design matrix 
## Q = hidden state process        diagonal and unequal
## B = effect of column on row     unequal (these are the interactions)

#####################################################################

df_aic <- data.frame(model=character(), aic=integer())

###NOW TO MODELS ##########
#estimate parameters
Z.model = factor(c(1,1,1,1,1))
R.model = "diagonal and unequal" #error variance not the same
kem_onepop = MARSS(dat, model=list(Z=Z.model, R=R.model))

#make figure
par(mfrow=c(1,1))
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem_onepop$states-1.96*kem_onepop$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_onepop$states+1.96*kem_onepop$states.se,type="l",
      lwd=1,lty=2,col="red")
lines(years,kem_onepop$states,type="l",lwd=2)
title("Observations and total population estimate",cex.main=.9)

coef(kem_onepop, type="vector")  #show the estimated parameter elements as a vector
coef(kem_onepop, type="matrix")$R
#show estimated elements for each parameter matrix as a list
#coef(kem_onepop) 

kem_onepop$logLik   #show the log-likelihood
kem_onepop$AIC  #show the AIC

df_aic <- df_aic %>% add_row(model = "one pop - r unequal", aic = kem_onepop$AIC)

#plot residuals, from 8.3 (page 99)
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem_onepop, type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
xs = matrix(kem_onepop$states,
            nrow=dim(plotdat)[1],ncol=dim(plotdat)[2],byrow=F)
resids = plotdat-matrix.of.biases-xs
par(mfrow=c(2,3))
for(i in 1:n){
  plot(resids[!is.na(resids[,i]),i],ylab="residuals")
  title(paste("One Population", legendnames[i]))
}
#bad residuals: BL, DP
#good-ish residuals: DE, TB, TP(?)


##########################################################
### 8.2 A single well-mixed population with i.i.d. errors
##########################################################
#Code to fit the single population model with independent and equal errors
#(silas) model is linear if R. model = "diagonal and equal" changed to unequal 6/6


Z.model = factor(c(1,1,1,1,1))
R.model = "diagonal and equal" 
kem2 = MARSS(dat, model=list(Z=Z.model, R=R.model))

coef(kem2) #the estimated parameter elements
kem2$logLik #log likelihood
kem2$AIC  #AICs

df_aic <- df_aic %>% add_row(model = "one pop - r equal", aic = kem2$AIC)


#plot residuals from page 99
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem2, type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
xs = matrix(kem2$states,
            nrow=dim(plotdat)[1],ncol=dim(plotdat)[2],byrow=F)
resids = plotdat-matrix.of.biases-xs
par(mfrow=c(2,3))
for(i in 1:n){
  plot(resids[!is.na(resids[,i]),i],ylab="residuals")
  title(paste("One Population", legendnames[i]))
}
 ## BL and DP residuals look a bit problematic
 ## (Silas) I think most look somewhat problematic, maybe TB and TP are ok

par(mfrow=c(1,1))

###################################################
### code chunk number 23: Cs2_Code4
### Each population is independent

#(silas) BL and DE clearly nonlinear but DP, TB, and TP aren't
#(silas) is this because of the convergence problems??
#(Silas) tried running with maxit=10000 and it didn't change the plots (except the confidence estimates for TB??)


###
# 5 independent populations with unequal observation variances, unequal growth, equal hidden process variance, not too sure about B being identity
###
Z.model=factor(c(1,2,3,4,5))
R.model="diagonal and unequal"
U.model="unequal"
Q.model="diagonal and equal"
B.Model="identity"
kem4_ind1=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
                    control=list(maxit=1000, safe=TRUE)) 
kem4_ind1$AIC #3.995799

df_aic <- df_aic %>% add_row(model = "all ind 1", aic = kem4_ind1$AIC)

#same as above but now B.Model is "unequal instead of identity"
#DOESN'T REACH CONVERGENCE
Z.model=factor(c(1,2,3,4,5))
R.model="diagonal and unequal"
U.model="unequal"
Q.model="diagonal and equal"
B.Model="unequal"
kem4_ind2=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
               control=list(maxit=5000, safe=TRUE)) 
kem4_ind2$AIC #4.71455

df_aic <- df_aic %>% add_row(model = "all ind 2", aic = kem4_ind2$AIC)


#change Q to unequal
#DOESN'T REACH CONVERGENCE
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and unequal"
#U.model="unequal"
#Q.model="diagonal and unequal"
#B.Model="identity"
#kem4_ind3=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind3$AIC #11.94

#change u to equal (reset Q back to equal)
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and unequal"
#U.model="equal"
#Q.model="diagonal and equal"
#B.Model="identity"
#kem4_ind4=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind4$AIC #16.13822

#change both u and Q
#DOESN'T CONVERGE
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and unequal"
#U.model="equal"
#Q.model="diagonal and unequal"
#B.Model="identity"
#kem4_ind5=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind5$AIC #15.13663

#change Q, u, and B
#DOESN'T CONVERGE, probably because the model isn't specified correctly mathematically (i.e. If an element of the diagonal of Q is 0, the corresponding row and col of B must be fixed.)
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and unequal"
#U.model="equal"
#Q.model="diagonal and unequal"
#B.Model="unequal"
#kem4_ind6=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind6$AIC #4.962436

#same as first but this time R is equal
Z.model=factor(c(1,2,3,4,5))
R.model="diagonal and equal"
U.model="unequal"
Q.model="diagonal and equal"
B.Model="identity"
kem4_ind7=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
                control=list(maxit=1000, safe=TRUE)) 
kem4_ind7$AIC # 4.137058

df_aic <- df_aic %>% add_row(model = "all ind 7", aic = kem4_ind7$AIC)


#from first, change R and B
# DOESN'T CONVERGE (again due to a wrongly specified model)
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and equal"
#U.model="unequal"
#Q.model="diagonal and equal"
#B.Model="unequal"
#kem4_ind8=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind8$AIC #15.41875

#change R and u
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and equal"
#U.model="equal"
#Q.model="diagonal and equal"
#B.Model="identity"
#kem4_ind9=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind9$AIC #20.79055

#change R and Q
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and equal"
#U.model="unequal"
#Q.model="diagonal and unequal"
#B.Model="identity"
#kem4_ind10=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#               control=list(maxit=1000, safe=TRUE)) 
#kem4_ind10$AIC #13.19429

#change R and Q and u
#DOESN'T CONVERGE
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and equal"
#U.model="equal"
#Q.model="diagonal and unequal"
#B.Model="identity"
#kem4_ind11=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind11$AIC #19.85212

#change R and Q and u and B
#hypothesis: doesn't converge due to badly specified model
#above is correct
#Z.model=factor(c(1,2,3,4,5))
#R.model="diagonal and equal"
#U.model="equal"
#Q.model="diagonal and unequal"
#B.Model="unequal"
#kem4_ind12=MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.Model),
#                control=list(maxit=1000, safe=TRUE)) 
#kem4_ind12$AIC #19.23859

###CONSIDER ONLY 1, 2 and 7


#plot residuals
plotdat = t(dat)
matrix.of.biases = matrix(coef(kem4_ind1,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,3))
for(i in 1:n){
  j=c(1,2,3,4,5)
  xs = kem4_ind1$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste(legendnames[i]))
}

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem4_ind2,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,3))
for(i in 1:n){
  j=c(1,2,3,4,5)
  xs = kem4_ind2$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste(legendnames[i]))
}

plotdat = t(dat)
matrix.of.biases = matrix(coef(kem4_ind7,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
par(mfrow=c(2,3))
for(i in 1:n){
  j=c(1,2,3,4,5)
  xs = kem4_ind7$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Independent Populations 7", legendnames[i]))
}

##2 has the best residuals

####### THIS INDEPENDENT HAS BEST RESIDUALS SO FAR!! #######
# (Silas) I don't like DP and TB too much but definite improvement

#oef(kem4_ind2, type="vector")  #show the estimated parameter elements as a vector
coef(kem4_ind1, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kem4_ind1) 

## try opepop plot
par(mfrow=c(2,3))
#make figure
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem4_ind1$states[1,]-1.96*kem4_ind1$states.se[1,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[1,]+1.96*kem4_ind1$states.se[1,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[1,],type="l",lwd=2)
title("Observations and total population estimate for BL",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem4_ind1$states[2,]-1.96*kem4_ind1$states.se[2,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[2,]+1.96*kem4_ind1$states.se[2,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind2$states[2,],type="l",lwd=2, col="red")
title("Observations and total population estimate for DE",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem4_ind1$states[3,]-1.96*kem4_ind1$states.se[3,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[3,]+1.96*kem4_ind1$states.se[3,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[3,],type="l",lwd=2, col="green")
title("Observations and total population estimate for DP",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem4_ind1$states[4,]-1.96*kem4_ind1$states.se[4,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[4,]+1.96*kem4_ind1$states.se[4,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[4,],type="l",lwd=2, col="blue")
title("Observations and total population estimate for TB",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3","4","5"), ylim=c(3,9), bty="L")
lines(years,kem4_ind1$states[5,]-1.96*kem4_ind1$states.se[5,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[5,]+1.96*kem4_ind1$states.se[5,],type="l",
      lwd=1,lty=2,col="purple")
lines(years,kem4_ind1$states[5,],type="l",lwd=2, col="cadetblue1")
title("Observations and total population estimate for TP",cex.main=.9)
##all plots are nonlinear for independent try 2

####################################
## let's try ocean vs bay model
## Most similar to 8.4 Two Sub-populations
###################################

#convergence warning due to badly defined model
#Z.model=factor(c(1,1,2,1,2))
#U.model="unequal" 
#Q.model="diagonal and unequal"
#R.model="diagonal and unequal"
#B.model="unconstrained" 
#kemOB1 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
#              control=list(maxit=500, safe=TRUE)) 
#kemOB1$AIC #39.23521

#change B
#unequal growth parameters, unequal hidden state process variances, unequal observation error variances
Z.model=factor(c(1,1,2,1,2))
U.model="unequal" 
Q.model="diagonal and unequal"
R.model="diagonal and unequal"
B.model="identity" 
kemOB2 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
              control=list(maxit=500, safe=TRUE)) 
kemOB2$AIC #35.93709

df_aic <- df_aic %>% add_row(model = "ocean vs bay", aic = kemOB2$AIC)


#using the same as for the best independent
#unequal observation error variances, unequal growth parameters, equal hidden state process variances
#Z.model=factor(c(1,1,2,1,2))
#R.model="diagonal and unequal"
#U.model="unequal"
#Q.model="diagonal and equal"
#B.Model="unequal"
#kemOB3 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
#               control=list(maxit=500, safe=TRUE)) 
#kemOB3$AIC #37.39974

#Z.model=factor(c(1,1,2,1,2))
#R.model="diagonal and equal"
#U.model="unequal"
#Q.model="diagonal and equal"
#B.Model="unequal"
#kemOB4 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
#               control=list(maxit=500, safe=TRUE)) 
#kemOB4$AIC #40.95777


#Z.model=factor(c(1,1,2,1,2))
#R.model="diagonal and equal"
#U.model="unequal"
#Q.model="diagonal and unequal"
#B.Model="unequal"
#kemOB5 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),
#               control=list(maxit=500, safe=TRUE)) 
#kemOB5$AIC #43.09786


#plot residuals
plotdat = t(dat)
par(mfrow=c(2,3))
matrix.of.biases = matrix(coef(kemOB2,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)

for(i in 1:n){
  j=c(1,1,2,1,2)
  xs = kemOB2$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("Bay vs Ocean", legendnames[i]))
}

## kem 4 still looks best residual and AIC wise.
coef(kemOB2, type="vector")  #show the estimated parameter elements as a vector
coef(kemOB2, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kemOB2) 

##########################################################
#Two subpopulations with different population parameters  
## (VNorth and South)
##########################################################
#(silas) no plots are nonlinear

#convergence warning because of badly defined model
#Z.model=factor(c(1,1,1,2,2))
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and equal"
#B.model="diagonal and unequal"
#kemNS1 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model) ) 
#kemNS1$AIC #60.09191

#convergence warning 
#Z.model=factor(c(1,1,1,2,2))
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and equal"
#B.model="identity"
#kemNS2 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model) ) 
#kemNS2$AIC #56.24401

#Z.model=factor(c(1,1,1,2,2))
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and unequal"
#B.model="identity"
#kemNS3 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model) ) 
#kemNS3$AIC #44.34793

#unequal observation errors but the same hidden state process errors
Z.model=factor(c(1,1,1,2,2))
U.model="unequal"
Q.model="diagonal and equal"
R.model="diagonal and unequal"
B.model="identity"
kemNS4 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model) ) 
kemNS4$AIC #40.47371

df_aic <- df_aic %>% add_row(model = "north vs south", aic = kemNS4$AIC)


#plot residuals
par(mfrow=c(2,3))
plotdat = t(dat)
matrix.of.biases = matrix(coef(kemNS4,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)
for(i in 1:n){
  j=c(1,1,1,2,2)
  xs = kemNS4$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("North vs South", legendnames[i]))
}



## N and S model not so good
coef(kemNS4, type="vector")  #show the estimated parameter elements as a vector
coef(kemNS4, type="matrix")$R
#show estimated elements for each parameter matrix as a list
coef(kemNS4) 


##########################################################
#Three subpopulations with different population parameters  
## (BL + DE/DP + TP/TB)
##########################################################

#convergence warning for Q(1,1) and Q(2,2)
#all plots linear
#Z.model=factor(c(1,2,2,3,3)) 
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and equal" 
#B.model= "identity"   # "diagonal and unequal"
#kem3pop1 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model) )
#kem3pop1$AIC #6.946689

#convergence warning for Q(2,2)
#BL nonlinear, other two are linear
Z.model=factor(c(1,2,2,3,3)) 
U.model="unequal"
Q.model="diagonal and unequal"
R.model="diagonal and unequal" 
B.model= "identity"  
kem3pop2 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model), control=list(maxit=5000, safe=TRUE))
kem3pop2$AIC #6.304194

df_aic <- df_aic %>% add_row(model = "3 pop - convergence warning", aic = kem3pop2$AIC)

#badly specified -> convergence error
#all nonlinear
#Z.model=factor(c(1,2,2,3,3)) 
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and equal" 
#B.model= "unequal"   
#kem3pop3 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),control=list(maxit=1000, safe=TRUE))
#kem3pop3$AIC #12.24963

#badly specified -> convergence warnings
#all nonlinear, looks very similar to 3
#Z.model=factor(c(1,2,2,3,3)) 
#U.model="unequal"
#Q.model="diagonal and unequal"
#R.model="diagonal and unequal" 
#B.model= "unequal"   
#kem3pop4 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),control=list(maxit=1000, safe=TRUE))
#kem3pop4$AIC #11.12605

#convergence warnings, badly specified
#very similar to 3 and 4
#Z.model=factor(c(1,2,2,3,3)) 
#U.model="unequal"
#Q.model="diagonal and equal"
#R.model="diagonal and unequal" 
#B.model= "unequal"   
#kem3pop5 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model, B=B.model),control=list(maxit=1000, safe=TRUE))
#kem3pop5$AIC #6.980729




par(mfrow=c(1,3))
matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3", "4", "5"), ylim=c(3,9), bty="L")
lines(years,kem3pop2$states[1,]-1.96*kem3pop2$states.se[1,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[1,]+1.96*kem3pop2$states.se[1,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[1,],type="l",lwd=2)
title("Bolinas Lagoon",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3", "4", "5"), ylim=c(3,9), bty="L")
lines(years,kem3pop2$states[2,]-1.96*kem3pop2$states.se[2,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[2,]+1.96*kem3pop2$states.se[2,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[2,],type="l",lwd=2)
title("Drakes Estero, Double Point",cex.main=.9)

matplot(years, t(dat),xlab="",ylab="index of log abundance",
        pch=c("1","2","3", "4", "5"), ylim=c(3,9), bty="L")
lines(years,kem3pop2$states[3,]-1.96*kem3pop2$states.se[3,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[3,]+1.96*kem3pop2$states.se[3,],type="l",
      lwd=1,lty=2,col="red")
lines(years,kem3pop2$states[3,],type="l",lwd=2)
title("Tomales Bay and Tomales Point",cex.main=.9)

#not sure what this plot is
par(mfrow=c(1,1))
plot(years,kem3pop2$states[1,])


plotdat = t(dat)
matrix.of.biases = matrix(coef(kem3pop2,type="matrix")$A,
                          nrow=nrow(plotdat),ncol=ncol(plotdat),byrow=T)

#residuals
par(mfrow=c(2,3))
for(i in 1:n){
  j=c(1,2,2,3,3)
  xs = kem3pop2$states[j[i],]
  resids = plotdat[,i]-matrix.of.biases[,i]-xs
  plot(resids[!is.na(resids)],ylab="residuals")
  title(paste("3 pops", i))
}

## get parameters
coef(kem3pop2, type="matrix")$R
coef(kem3pop2, type="matrix")$Q
coef(kem3pop2, type="matrix")$U
coef(kem3pop2, type="matrix")$B

## AICs
#c(kem_onepop$AIC,kem2$AIC,kem4_ind$AIC,kemNS$AIC, kemOB$AIC, kem3pop$AIC)

#Best AIC is 3 groups, then all independent 


##NEW PLOTS
##NOT UPDATED AS OF 6/16/2022
df <- as.data.frame(t(dat))
mod <- as.data.frame(t(kem4_ind1$states))
names(mod) <- c("BL_m", "DE_m", "DP_m", "TB_m", "TP_m")
se_a <- as.data.frame(t(kem4_ind1$states.se))
df$BL_m <- mod$BL_m
#df$BL_lower <- df$BL_m-1.96*se[,1]
#df$BL_upper <- df$BL_m+1.96*se[,1]
df$year <- 1996:2019
p1a <-
  ggplot(df) + 
  geom_point(aes(years, exp(BL)), colour="salmon") + 
  geom_line(aes(years, exp(BL_m))) + 
  #geom_line(aes(years, exp(BL_lower)), linetype = "dashed", color = "red") + 
  #geom_line(aes(years, BL_upper), linetype = "dashed", color = "red") + 
  #ylim(c(4.5,7)) +
  labs(title = "Bolinas Lagoon", y = "Abundance", x = "Year") +
  ylim(175, 1200) + xlim(1995, 2021)

df$DE_m <- mod$DE_m
#df$DE_lower <- df$DE_m-1.96*se[,2]
#df$DE_upper <- df$DE_m+1.96*se[,2]


p2a <- ggplot(df) + geom_point(aes(years, exp(DE)), colour="salmon")  + geom_line(aes(years, exp(DE_m))) + 
  labs(title="Drakes Estero", y="Abundance", x = "Year") + theme(legend.title = element_text(face = "bold")) +
  ylim(175, 1200) + xlim(1995, 2021)
  #+ geom_line(aes(years, exp(DE_lower)), linetype = "dashed", color = "red") + geom_line(aes(years, DE_upper), linetype = "dashed", color = "red")

#p2 <- ggplot(df) + 
#  geom_point(aes(years, DE)) + 
#  geom_point(aes(years, DP)) + 
#  geom_line(aes(years, DEDP_m)) + 
  #geom_line(aes(years, DEDP_lower), linetype="dashed", color="red") + 
  #geom_line(aes(years, DEDP_upper), linetype="dashed", color="red") + 
  #ylim(c(4.5,7)) +
#  labs(title="Drakes Estero and Double Point", y="index of log abundance")


df$DP_m <- mod$DP_m
#SE is all 0 for some reason 
#df$TBTP_lower <- df$TBTP_m-1.96*se[,3]
#df$TBTP_upper <- df$TBTP_m-1.96*se[,3]

p3a <- ggplot(df) + geom_point(aes(years, exp(DP)), colour="salmon") + geom_line(aes(years, exp(DP_m))) + 
  labs(title="Double Point", y="Abundance", x = "Year") +
  ylim(175, 1200) + xlim(1995, 2021)
  #+ geom_line(aes(years, TBTP_lower), linetype="dashed", color="red") + geom_line(aes(years, TBTP_upper), linetype="dashed", color="red") 


df$TB_m <- mod$TB_m
p4a <- ggplot(df) + geom_point(aes(years, exp(TB)), colour="salmon") + geom_line(aes(years, exp(TB_m))) + 
  labs(title="Tomales Bay", y="Abundance", x = "Year") + ylim(175, 1200) + xlim(1995, 2021)

df$TP_m <- mod$TP_m
p5a <- ggplot(df) + geom_point(aes(years, exp(TP)), colour="salmon") + geom_line(aes(years, exp(TP_m))) + 
  labs(title="Tomales Point", y="Abundance", x = "Year")+ ylim(175, 1200) + xlim(1995, 2021)


## BL DE DP TB TP
plot1 <- cowplot::plot_grid(p1a, p2a, p3a, p4a, p5a)

title <- ggdraw() + 
  draw_label(
    "5 Independent #1",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, plot1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

df <- as.data.frame(t(dat))
mod <- as.data.frame(t(kem4_ind2$states))
names(mod) <- c("BL_m", "DE_m", "DP_m", "TB_m", "TP_m")
se_b <- as.data.frame(t(kem4_ind2$states.se)) #nonzero but incredibly small
se_b <- exp(se_b) #backtransform
df$BL_m <- mod$BL_m
df$BL_lower <- exp(df$BL_m)-1.96*se_b[,1]
df$BL_upper <- exp(df$BL_m)+1.96*se_b[,1]

df$year <- 1996:2019
p1b <- ggplot(df) + 
  geom_point(aes(years, exp(BL)), colour="salmon") + 
  geom_line(aes(years, exp(BL_m))) + 
  labs(title = "Bolinas Lagoon", y = "Abundance", x = "Year") +
  ylim(175, 1200) + xlim(1995, 2021) 
  #geom_line(aes(years, BL_lower), linetype="dashed", color="red") + geom_line(aes(years, BL_upper), linetype="dashed", color="red") 

df$DE_m <- mod$DE_m
df$DE_lower <- exp(df$DE_m)-1.96*se_b[,2]
df$DE_upper <- exp(df$DE_m)+1.96*se_b[,2]
p2b <- ggplot(df) + geom_point(aes(years, exp(DE)), colour="salmon")  + geom_line(aes(years, exp(DE_m))) + 
  labs(title="Drakes Estero", y="Abundance", x ="Year") + theme(legend.title = element_text(face = "bold")) +
  ylim(175, 1200) + xlim(1995, 2021)
  #geom_line(aes(years, DE_lower), linetype="dashed", color="red") + geom_line(aes(years, DE_upper), linetype="dashed", color="red") 

df$DP_m <- mod$DP_m
df$DP_lower <- exp(df$DP_m)-1.96*se_b[,3]
df$DP_upper <- exp(df$DP_m)+1.96*se_b[,3]
p3b <- ggplot(df) + geom_point(aes(years, exp(DP)), colour="salmon") + geom_line(aes(years, exp(DP_m))) + 
  labs(title="Double Point", y="Abundance", x = "Year") + 
  ylim(175, 1200) + xlim(1995, 2021)
  #geom_line(aes(years, DP_lower), linetype="dashed", color="red") + geom_line(aes(years, DP_upper), linetype="dashed", color="red") 


df$TB_m <- mod$TB_m
df$TB_lower <- exp(df$TB_m)-1.96*se_b[,4]
df$TB_upper <- exp(df$TB_m)+1.96*se_b[,4]
p4b <- ggplot(df) + geom_point(aes(years, exp(TB)), colour="salmon") + geom_line(aes(years, exp(TB_m))) + 
  labs(title="Tomales Bay", y="Abundance", x = "Year") +
  ylim(175, 1200) + xlim(1995, 2021)
  #geom_line(aes(years, TB_lower), linetype="dashed", color="red") + geom_line(aes(years, TB_upper), linetype="dashed", color="red") 

df$TP_m <- mod$TP_m
df$TP_lower <- exp(df$TP_m)-1.96*se_b[,5]
df$TP_upper <- exp(df$TP_m)+1.96*se_b[,5]
p5b <- ggplot(df) + geom_point(aes(years, exp(TP)), colour="salmon") + geom_line(aes(years, exp(TP_m))) + 
  labs(title="Tomales Point", y="Abundance", x = "Year") +
  ylim(175, 1200) + xlim(1995, 2021)
  #geom_line(aes(years, TP_lower), linetype="dashed", color="red") + geom_line(aes(years, TP_upper), linetype="dashed", color="red") 


## BL DE DP TB TP
##ggplot to facet: DF = site, year, Count, Flag for raw vs model
#pivot_longer look up in dplyr

plot2 <- cowplot::plot_grid(p1b, p2b, p3b, p4b, p5b)

title <- ggdraw() + 
  draw_label(
    "5 Independent #2",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, plot2,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

##%######################################################%##
#                                                          #
####                 OK, now got models                 ####
####         working. On to hypothesis testing          ####
#                                                          #
##%######################################################%##


