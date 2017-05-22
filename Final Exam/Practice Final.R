############################
# Practice Final Exam
# Xiner Zhou
# 4/28/2016
############################

# Problem 1

library(Zelig)
library(dplyr)
library(xtable)
# Data to predict when countires cooperate in enforcing sanctions
# dependent var, coop, is the number of countries that cooperated in the enforcing sanctions
data(sanction)

## 1.A Recode the dependent variable to coop1, which will be 0 when only one country cooperated (coop==1) and 1 if more than one country cooperated(coop>1)
sanction <- sanction%>%
  mutate(coop1=ifelse(coop==1,0,ifelse(coop>1,1,NA)))

## 1.B Write out the stochastic and systematic component for the probit model

## 1.C log-likelihood function
probit.ll <- function(par, outcome, covariates) {
   
  # check to see if the first column is all 1's
  # if not, add 1's for the intercept column
  if(!all(covariates[,1]==1)){covariates<-as.matrix(cbind(1,covariates))}
  
  xb <-covariates %*% par
  # if observe 1
  p1 <-pnorm(xb, mean = 0, sd = 1, lower.tail = TRUE, log.p = TRUE)
  # if observe 0
  p0 <-pnorm(xb, mean = 0, sd = 1, lower.tail = FALSE, log.p = TRUE)
  # log-likelihood
  sum(p1[which(outcome==1)])+sum(p0[which(outcome==0)])
   
}

## 1.D Use the likelihood function you coded in Part C and optim() to run a probit model of coop1 on the four variables and intercept.
##     Report coefficients and standard errors in a nicely formatted table.

# create X matrix
X<-as.matrix(cbind(1,select(sanction, import, export, target, cost)))
MLE <- optim(par=rep(1,ncol(X)),
             fn=probit.ll,
             covariates=X,
             outcome=sanction$coop1,
             control=list(fnscale=-1),
             hessian = T,
             method = "BFGS")

# MLE coefficient estimates are:
Effect <-MLE$par
# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <-solve(obs_fisher_info)
SE <-sqrt(diag(vcov))
## Vaiable names
Variable <- c("Intercept",
                  "Import",
                  "Export",
                  "Target",
                  "Cost")
## Put into a data frame
MLE.table<-data.frame(Variable, Effect, SE)
xtable(MLE.table) 

## 1.E Simulate the distributions of the expected probabilities for two types of countries: high cost (cost=4) and low cost(cost=1).
##    Keep all other covariates at their means. 
##    Plot a density plot of the distribution of first differences. What is the mean first difference?

# Step 1: Write out your model and estimate betahat and the variance-covariance maxtrix

# Step 2: Simulate beta tildes, M=10000 times, from a multivariate normal distribution with mean and variance = estimates, to account for estimation uncertainty
library(mvtnorm)
set.seed(02138)
sim.betas<-rmvnorm(n=10000, mean=Effect, sigma=vcov)
dim(sim.betas)

# Step 3: Choose mean for each explanatory variable, store as atX0 for low cost countries and atX1 for high cost countries
atX0 <- sanction %>% mutate(intercept=1, cost=1) %>%
  select(intercept,import, export, target, cost) %>%
  apply(2,mean)
 
atX1 <- sanction %>% mutate(intercept=1, cost=4) %>%
  select(intercept,import, export, target, cost) %>%
  apply(2,mean)

# Step 4: Use atX0 and atX1 and simulated beta tildes, calculate the systematic component pi tilde
  # Step 5: For each pi tilde, draw m=10000 simulations from the stochastic component, to account for fundamental uncertainty
  # Step 6: Store the mean of these m simulations for each pi tildes, average away fundamental uncertainty, as the expected probability for each simulated pi tilde
  # Step 7: Take first difference between high cost and low cost countries, for each simulated beta tildes
  # Step 8: Plot the density of first difference and calculate mean
M<-10000
m<-10000
first.diff<-rep(NA, M)
set.seed(02138)

for (i in 1:M){
  
  pi0.tilde <-pnorm(atX0%*%sim.betas[i,], mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  pi1.tilde <-pnorm(atX1%*%sim.betas[i,], mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  
  first.diff[i]<-mean(rbinom(n=m, size=1, prob=pi1.tilde))-mean(rbinom(n=m, size=1, prob=pi0.tilde))
}

library(ggplot2)
as.data.frame(first.diff) %>% ggplot(aes(first.diff)) +
  geom_density(fill="yellow")+
  ggtitle("First Difference: High cost vs Low cost countries")+
  xlab("first diff in probability of cooperation")+
  geom_vline(xintercept=mean(first.diff), colour='red')



# Problem 2

data(coalition)
## 2.A log-likelihood funciton
weibull.ll <-function(par, outcome, covariates, censored){
  # the last par is the scale parameter log(alpha)
  # check to see if the first column is all 1's
  # if not, add 1's for the intercept column
  if(!all(covariates[,1]==1)){covariates<-as.matrix(cbind(1,covariates))}
  
  xb <-covariates %*% par[-length(par)]
  
  # re-paramenterize alpha >0 to unbounded, use log
  alpha<-exp(par[ncol(X)+1])
  sum(censored*(log(alpha)-alpha*(xb)+(alpha-1)*log(outcome))-(outcome/exp(xb))^alpha)
  
}

## 2.B Use the likelihood function you coded in Part C and optim() to run a probit model of coop1 on the four variables and intercept.
##     Report coefficients and standard errors in a nicely formatted table.

# create X matrix
X<-as.matrix(cbind(1,select(coalition, invest, fract, polar, numst2, crisis)))
MLE <- optim(par=rep(0,1+ncol(X)),
             fn=weibull.ll,
             covariates=X,
             outcome=coalition$duration,
             censored=coalition$ciep12,
             control=list(fnscale=-1),
             hessian = T,
             method = "BFGS")

# MLE coefficient estimates are:
Effect <-MLE$par
# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <-solve(obs_fisher_info)
SE <-sqrt(diag(vcov))
## Vaiable names
Variable <- c("Intercept",
              "invest",
              "fract",
              "polar",
              "numst2",
              "crisis",
              "log(alpha)")
## Put into a data frame
MLE.table<-data.frame(Variable, Effect, SE)

## 2.c Simulate the distributions of expected coalition duration times for two types of countries: majority (numst2=1)
##    and minority(numst2=0). Keep all other covariates at their means. Plot the two dsitributions on the same plot and 
##    provide a legend. Do 10000 simulations. (Hint: USe the formula for the expected value of the Weibull distribution provided to you)

# Step 1: Write out your model and estimate betahat and the variance-covariance maxtrix

# Step 2: Simulate beta tildes, M=10000 times, from a multivariate normal distribution with mean and variance = estimates, to account for estimation uncertainty
library(mvtnorm)
set.seed(02138)
sim.betas<-rmvnorm(n=10000, mean=Effect, sigma=vcov)
dim(sim.betas)

# Step 3: Choose mean for each explanatory variable, store as atX0 and atX1 for majority and minority
atX0 <- coalition %>% mutate(intercept=1, numst2=0) %>%
  select(intercept, invest, fract, polar, numst2, crisis) %>%
  apply(2,mean)

atX1 <- coalition %>% mutate(intercept=1, numst2=1) %>%
  select(intercept, invest, fract, polar, numst2, crisis) %>%
  apply(2,mean)

# Step 4: Use atX0 and atX1 and simulated beta tildes, calculate the systematic component pi tilde
# Step 5: For each pi tilde, draw m=10000 simulations from the stochastic component, to account for fundamental uncertainty
# Step 6: Store the mean of these m simulations for each pi tildes, average away fundamental uncertainty, as the expected probability for each simulated pi tilde
# Step 7: Take first difference between high cost and low cost countries, for each simulated beta tildes
# Step 8: Plot the density of first difference and calculate mean
M<-10000
exp.dur<-data.frame(dur=rep(NA, 2*M), country=c(rep("Minority",M),rep("Majority",M)))

for (i in 1:M){
  exp.dur$dur[i]<- exp(atX0%*%sim.betas[i,-ncol(sim.betas)])*gamma(1+1/exp(sim.betas[i,ncol(sim.betas)]))
  exp.dur$dur[M+i]<- exp(atX1%*%sim.betas[i,-ncol(sim.betas)])*gamma(1+1/exp(sim.betas[i,ncol(sim.betas)]))
}

library(ggplot2)
exp.dur %>% ggplot(aes(dur)) +
  geom_density(aes(group=country,colour=country))+
  ggtitle("Expected Duration Time : Majority vs Minority Country")+
  xlab("Duration")+
  theme(legend.position="bottom")

## 2.D Simulate the distribution of the first difference between expected coalition duration times of majority and minority governments. 
M<-10000
first.diff<-rep(NA, M)

for (i in 1:M){
  exp.dur0<- exp(atX0%*%sim.betas[i,-ncol(sim.betas)])*gamma(1+1/exp(sim.betas[i,ncol(sim.betas)]))
  exp.dur1<- exp(atX1%*%sim.betas[i,-ncol(sim.betas)])*gamma(1+1/exp(sim.betas[i,ncol(sim.betas)]))
  first.diff[i]<-exp.dur1-exp.dur0
}

library(ggplot2)
as.data.frame(first.diff) %>% ggplot(aes(first.diff)) +
  geom_density(fill="yellow")+
  ggtitle("First Difference in Expected Coalition Duration Time: Majority and Minority")+
  xlab("Duration")+
  geom_vline(xintercept=mean(first.diff), colour='red')

## 2.E What is the 95% confidence interval (empirical no Normality assumption) for the first difference from Part D? 
##     Do you reject the null hypothesis that there is no difference between expected duration times for majority and minoritu governments at the 5% significance level?
quantile(first.diff, c(0.025, 0.975)) 
# since the 95% CI does not include 0, we reject the null hypothesis at the 5% significance level

## 2.F Simulate the expected duration time as a function of polarization (polar) (using 100000 simulations).
##     Keep all other covariate at their means. You want to create a plot where the x-axis ranges from the minimum value of polar to max value of polar. 
##     The y-axis should have the expected duration of the coalition. Please include 95% confidence intervals on the plot. 
##     Make sure the plot is well labeled.
polar.range<-seq(min(coalition$polar), max(coalition$polar), 1)
atX  <- coalition %>% mutate(intercept=1) %>%
  select(intercept, invest, fract, polar, numst2, crisis) %>%
  apply(2,mean)
M<-10000

# polar hypothetical value by row, simulation M by column
exp.dur<-matrix(NA,nrow=length(polar.range),ncol=M)
# confidence interval
upper<-rep(NA,length(polar.range))
lower<-rep(NA,length(polar.range))
mean<-rep(NA,length(polar.range))

for(i in 1:length(polar.range)){
  atX[4]<-polar.range[i] 
  for (j in 1:M){exp.dur[i,j]<- exp(atX%*%sim.betas[j,-ncol(sim.betas)])*gamma(1+1/exp(sim.betas[j,ncol(sim.betas)]))}
  upper[i]<-quantile(exp.dur[i,],probs=0.975)
  lower[i]<-quantile(exp.dur[i,],probs=0.025)
  mean[i]<-mean(exp.dur[i,] )
}
 
plot(polar.range, rowMeans(exp.dur), type="l", col="purple", lwd=2 ,
     main="Expexted Coalition Duration as a Function of Polarization",
     xlab="Polarization",
     ylab="Expected Coalition Duration")
lines(polar.range, apply(exp.dur, 1, function(x) quantile(x,0.975)), col="purple", lty=2)
lines(polar.range, apply(exp.dur, 1, function(x) quantile(x,0.025)), col="purple", lty=2)


## 2.G We want to test whether an exponential model can be used for our data. 
##     Run a restricted model using a likelihood ratio test. Do you reject the null that the restricted and unrestricted models are the same
##     at the 5% significance level? What is the p-value?
exp.ll <-function(par, outcome, covariates, censored){
  # the last par is the scale parameter log(alpha)
  # check to see if the first column is all 1's
  # if not, add 1's for the intercept column
  if(!all(covariates[,1]==1)){covariates<-as.matrix(cbind(1,covariates))}
  
  xb <-covariates %*% par[-length(par)]
  
  # re-paramenterize alpha >0 to unbounded, use log
  alpha<-1
  sum(censored*(log(alpha)-alpha*(xb)+(alpha-1)*log(outcome))-(outcome/exp(xb))^alpha)
  
}

MLE.exp <- optim(par=rep(0,1+ncol(X)),
             fn=exp.ll,
             covariates=X,
             outcome=coalition$duration,
             censored=coalition$ciep12,
             control=list(fnscale=-1),
             hessian = T,
             method = "BFGS")

LR<-2*(MLE$value-MLE.exp$value)
1-pchisq(LR, df=1)
# The likelihood ratio statisitcs I obtain is 19.03. When compared against a Chisq(df=1) dsitribution we obtain p-value of 0.0000129. We reject the null at 5% significance level.


## 2.H We want to plot the predicted durations for majority and minority governments. The setup is the similar to PartC, 
##   but now we want to also estimate the fundamental uncertainty. Use the rweibull() function. Note that the shape parameter
##   is alpha, and the scale parameter is lambda. Create a plot similar to the one in Part C with two densities, but now with predicted values.


# Step 1: Write out your model and estimate betahat and the variance-covariance maxtrix

# Step 2: Simulate beta tildes, M=10000 times, from a multivariate normal distribution with mean and variance = estimates, to account for estimation uncertainty
library(mvtnorm)
set.seed(02138)
sim.betas<-rmvnorm(n=10000, mean=Effect, sigma=vcov)
dim(sim.betas)

# Step 3: Choose mean for each explanatory variable, store as atX0 and atX1 for majority and minority
atX0 <- coalition %>% mutate(intercept=1, numst2=0) %>%
  select(intercept, invest, fract, polar, numst2, crisis) %>%
  apply(2,mean)

atX1 <- coalition %>% mutate(intercept=1, numst2=1) %>%
  select(intercept, invest, fract, polar, numst2, crisis) %>%
  apply(2,mean)

# Step 4: Use atX0 and atX1 and simulated beta tildes, calculate the systematic component pi tilde
# Step 5: For each pi tilde, draw 1 simulations from the stochastic component, to account for fundamental uncertainty
# Step 6: Plot the density of first difference and calculate mean
M<-10000
pred.dur<-data.frame(dur=rep(NA, 2*M), country=c(rep("Minority",M),rep("Majority",M)))

for (i in 1:M){
  pred.dur$dur[i]<- rweibull(1,shape=exp(sim.betas[i,ncol(sim.betas)]), scale=exp(atX0%*%sim.betas[i,-ncol(sim.betas)]))
  pred.dur$dur[M+i]<-rweibull(1,shape=exp(sim.betas[i,ncol(sim.betas)]), scale=exp(atX1%*%sim.betas[i,-ncol(sim.betas)]))
}

library(ggplot2)
pred.dur %>% ggplot(aes(dur)) +
  geom_density(aes(group=country,colour=country))+
  ggtitle("Predicted Duration Time : Majority vs Minority Country")+
  xlab("Duration")+
  theme(legend.position="bottom")













 












