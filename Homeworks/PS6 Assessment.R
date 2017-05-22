##### Problem Set 6 Assessment
##### Xiner Zhou
##### 3/17/2016

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 6")

# Problem 1: Negative Binomial II

# This represents the number of failures which occur in a sequence of Bernoulli trials (size=theta, prob=lamda)
# before a target number of successes is reached.


#### 1.C: Optimize the model to find the maximum likelihood estimates  

# NegBinII log-likelihood function
NegBinII.ll <-function(par,outcome,covariates){
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
  beta <-par[-length(par)] # parameter for the mean
  gamma1 <- log(par[length(par)]) # repameterize fot theta
  
  xb<-covariates%*%beta
  sum(log(gamma(exp(gamma1)+outcome))+outcome*xb+gamma1*exp(gamma1)-log(gamma(outcome+1))-log(gamma(exp(gamma1)))-(exp(gamma1)+outcome)*log(exp(xb)+exp(gamma1)))
}

# read in dataset
dat2<-read.csv("revolutions.csv")

design.matrix<-cbind(1,dat2$GenStrikes,dat2$GovCrises,dat2$Riots,dat2$polity2)

res<-lm(Revolutions~GenStrikes+GovCrises+Riots+polity2,data=dat2)
summary(res)
# estimate the MLE:
MLE <- optim(par=rep(1,ncol(design.matrix)+1),
             fn=NegBinII.ll,
             covariates=design.matrix,
             outcome=dat2$Revolutions,
             control = list(fnscale=-1),
             hessian=T,
             method = "BFGS")
 
# MLE coefficient estimates are:
coefs <- MLE$par
coefs[6]<-sqrt(exp(coefs[6]))
names(coefs)<-c("Intercept",
                "GEnStrikes: general strikes",
                "GovCrises: government crises",
                "Riots: riots",
                "polity2: polity score",
                "Theta/Size: target for number of successful trials, or dispersion parameter")
coefs

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
varcov <- solve(obs_fisher_info) 
se<-sqrt(diag(varcov)) 
names(se)<-c("Intercept",
             "GEnStrikes: general strikes",
             "GovCrises: government crises",
             "Riots: riots",
             "polity2: polity score",
             "Theta/Size: target for number of successful trials, or dispersion parameter")
se

#### 1.D: Generate a histogram of 10,000 predicted values for a democracy (polity2=10), with
####      all other variables held at their mean. Set your seed to 02138. 
####      (Hint: you can use rnbinom to draw from NegBin II).
library(dplyr)
# Get mean for covariates and re-set democracy
Xc <-dat2 %>% select(GenStrikes,GovCrises,Riots,polity2)%>% apply(MARGIN =2, FUN=mean)
Xc["polity2"]<-10
# Add 1 for intercept
Xc<-c(1,Xc)
names(Xc)[1]<-"Intercept"
Xc


# Simulate beta tildes and theta from posterior distribution 
library(mvtnorm)
set.seed(8766)
sim.par <- rmvnorm(n=10000, mean=coefs, sigma=varcov)

M<-10000 # number of draws
pred <-rep(NA, M) # place-holder for simulated predicted value

for(i in 1:M){
  lambda <- exp(Xc%*%sim.par[i,1:5]) 
  pred[i]<-rnbinom(n=1, size=sim.par[i,6], prob=lambda) #prob=lambda, size=theta
}
 
# Generate a histogram with 10,000 predicted values for a democracy (polity2=10), with
# all other variables held at their mean. Set the seed to 8766.
library(ggplot2)

as.data.frame(pred) %>% 
  mutate(class=ifelse(pred>quantile(pred,0.95),"Upper 95%","Between 0%-95%"))%>%
  ggplot(aes(pred,fill=class))+geom_histogram()+stat_bin(binwidth = 1)+
  ggtitle("Predicted Number of Revolutions for a Democracy")+xlab("Number of Revolutions")+
  geom_vline(xintercept=mean(pred),colour="red")

#### 2.E: Based on your simulations, calculate the probability that 
####      such a state has one or more revolutions in a given year.
mean(as.numeric(pred>=1))





