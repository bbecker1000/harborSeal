library(coda)
library(rjags)
library(R2jags)



#from DataPrep.R
unique(top1_wide_wide_ADULT_Molt$Year) #every year 1976 - 2022  start with this
unique(top1_wide_wide_ADULT_Breeding$Year) #(missing 1985, 1986, 1987) #need to do all years and make a left Join

sites <- c( "DP",    "DE",    "PR",    "BL",    "TP",    "TB",    "DR",    "PB")
#  <- top1_wide_wide_ADULT_Molt[,sites]
Y <- top1_wide_wide_ADULT_Molt_2020[,sites] # has data all sites all years, mostly

Y <- t(Y)  # time across columns


jagsscript <- cat("
model {  
   U ~ dnorm(0, 0.01);
   tauQ~dgamma(0.001,0.001);
   Q <- 1/tauQ;

   # Estimate the initial state vector of population abundances
   for(i in 1:nSites) {
      X[i,1] ~ dnorm(3,0.01); # vague normal prior 
   }

   # Autoregressive process for remaining years
   for(t in 2:nYears) {
      for(i in 1:nSites) {
         predX[i,t] <- X[i,t-1] + U;
         X[i,t] ~ dnorm(predX[i,t], tauQ);
      }
   }

   # Observation model

   # The Rs are different in each site
   for(i in 1:nSites) {
     tauR[i]~dgamma(0.001,0.001);
     R[i] <- 1/tauR[i];
   }
   for(t in 1:nYears) {
     for(i in 1:nSites) {
       Y[i,t] ~ dnorm(X[i,t],tauR[i]);
     }
   }
}  

", 
                  file = "marss-jags.txt")



jags.data <- list(Y = Y, nSites = nrow(Y), nYears = ncol(Y))  # named list
jags.params <- c("X", "U", "Q", "R")
model.loc <- "marss-jags.txt"  # name of the txt file
mod_1 <- jags(jags.data, parameters.to.save = jags.params, model.file = model.loc, 
              n.chains = 3, n.burnin = 5000, n.thin = 1, n.iter = 10000, 
              DIC = TRUE)


summary(mod_1)
mod_1

# attach.jags attaches the jags.params to our workspace
attach.jags(mod_1)
means <- apply(X, c(2, 3), mean)
upperCI <- apply(X, c(2, 3), quantile, 0.975)
lowerCI <- apply(X, c(2, 3), quantile, 0.025)
par(mfrow = c(4, 2))
nYears <- ncol(Y)
for (i in 1:nrow(means)) {
  plot(means[i, ], lwd = 3, ylim = range(c(lowerCI[i, ], upperCI[i, 
  ])), type = "n", main = colnames(Y)[i], ylab = "log abundance", 
  xlab = "time step")
  polygon(c(1:nYears, nYears:1, 1), c(upperCI[i, ], rev(lowerCI[i, 
  ]), upperCI[i, 1]), col = "skyblue", lty = 0)
  lines(means[i, ], lwd = 3)
  title(rownames(Y)[i])
}

# m hidden states

jagsscript <- cat("
model {  
   # process model priors
   inv.q~dgamma(0.001,0.001);
   q <- 1/inv.q; # one q
   for(i in 1:n) {
      u[i] ~ dnorm(0, 0.01); 
      X0[i] ~ dnorm(Y1[i],0.001); # initial states
   }
   # process model likelihood
   for(i in 1:n) {
     EX[i,1] <- X0[i] + u[i];
     X[i,1] ~ dnorm(EX[i,1], inv.q);
   }
   for(t in 2:N) {
      for(i in 1:n) {
         EX[i,t] <- X[i,t-1] + u[i];
         X[i,t] ~ dnorm(EX[i,t], inv.q);
      }
   }

   # observation model priors
   for(i in 1:n) { # The r's are different by site
     inv.r[i]~dgamma(0.001,0.001);
     r[i] <- 1/inv.r[i];
   }
   # observation model likelihood
   for(t in 1:N) {
     for(i in 1:n) {
       EY[i,t] <- X[i,t]
       Y[i,t] ~ dnorm(EY[i,t], inv.r[i]);
     }
   }
}  

", 
                  file = "marss-jags2.txt")

dat <- Y # already transposed above
jags.data <- list(Y = dat, n = nrow(dat), N = ncol(dat), Y1 = dat[, 
                                                                  1])
jags.params <- c("EY", "u", "q", "r")
model.loc <- "marss-jags2.txt"  # name of the txt file

# need initial values for each site, the NAs ok -------------------
mod_marss1 <- R2jags::jags(jags.data, parameters.to.save = jags.params, 
                           model.file = model.loc, n.chains = 3, n.burnin = 5000, n.thin = 1, 
                           n.iter = 10000, DIC = TRUE)


## ----jags-marss1-plot-fun, warning=FALSE, message=FALSE----------------------------------------
make.ey.plot <- function(mod, dat){
  library(ggplot2)
  EY <- mod$BUGSoutput$sims.list$EY
  n <- nrow(dat); N <- ncol(dat)
  df <- c()
  for(i in 1:n){
    tmp <- data.frame(n = paste0("Y",i),
                      x = 1:N, 
                      ey=apply(EY[,i,, drop=FALSE],3,median),
                      ey.low=apply(EY[,i,, drop=FALSE],3,quantile,probs=0.25),
                      ey.up=apply(EY[,i,, drop=FALSE],3,quantile,probs=0.75),
                      y=dat[i,]
    )
    df <- rbind(df, tmp)
  }
  ggplot(df, aes(x=x+1999, y=ey)) + geom_line() +
    geom_ribbon(aes(ymin=ey.low, ymax=ey.up), alpha=0.25) +
    geom_point(data=df, aes(x=x+1999, y=y)) +
    facet_wrap(~n) + theme_bw() +
    xlab("Year")
}


## ----jags-marss1-plot, warning=FALSE, message=FALSE--------------------------------------------
make.ey.plot(mod_marss1, dat)


