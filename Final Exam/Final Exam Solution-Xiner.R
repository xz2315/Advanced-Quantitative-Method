###########################
# Gov 2001: Final Exam
# Xiner Zhou
# 4/29/2016
###########################

# Problem 1: Job Lock with Logistic Regression


setwd("E:/Advanced Quantitative Method-Gary King/Final Exam")
load("employmentdata.rdata") # load data
 

library(dplyr)
library(ggplot2)
library(stargazer)
library(mvtnorm)

## 1.A 
## Estimate the probability of voluntarily changing jobs (chjob) for workers with employment-related
## health insurance (hi) using logistic regression and the following covariates: othhi, an interaction 
## bt hi and othhi, lhwage, winc, nfam, educ, exper, afram. You may use a canned regression routine to 
## answer this question. Report your coefficient estimates and standard errors below.
fit <- glm(chjob~ hi + othhi + hi*othhi + lhwage +winc + nfam + educ + exper + afram, data=finaldata1, family="binomial")
summary(fit)$coefficients
stargazer(fit)



## 1.B Calculate:
##     a) The expected probability of voluntarily changing jobs for a white married man with employer-provided 
##        health insurance but no other source of health insurance available, holding all other covariates at their
##        mean values. Report a 95% confidence interval (using simulation) for your estimate.


# Step1: Simulate M beta's from sampling distribution
# Step2: Choose one value for each explanatory variable
# Step3: Take one simulated beta's, compute pi from the systematic component (This step simulate estimation uncertainty)
# Step4: Draw m times of the outcome variable (1/0) from the stochastic component (Bernoulli) (This step simulate fundamental uncertainty)
# Step5: Average over the fundamental unceratinty by calculating the mean of the m simulations to yield one simulated expected value (probability)
# (To save computation time and improve approximation, use the same simulated beta's for 1.B(a)-(d) and 1.C )

beta <- fit$coefficients
vcov <- vcov(fit)
set.seed(02138) 
M<-10000
m<-10000
sim.betas <- rmvnorm(n=M, mean=beta, sigma=vcov)
X <- finaldata1 %>%  
     mutate(Intercept=1)%>%
     select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
     mutate(hi=1, othhi=0, afram=0)%>%
     mutate("hi*othhi"=hi*othhi) %>%
     apply(MARGIN=2, FUN=mean) 
 
exp.prob <-rep(NA, M)
for(i in 1:M){
  pi  <- 1/(1+exp(-X%*%sim.betas[i,]))
  exp.prob[i] <- mean(rbinom(n=m,size=1,prob=pi)) #n= number of obs size=number of trials
}
 
# 95% CI
quantile(exp.prob, c(0.025,0.975))  
 
# Point Estimate
mean(exp.prob)

as.data.frame(exp.prob) %>% ggplot(aes(exp.prob)) +
  geom_density(fill="yellow")+
  ggtitle("Expected Probability")+
  xlab("Expected Probability")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(exp.prob), colour='red')


##     b) The expected probability of voluntarily changing jobs for a white married man with
##        employer-provided health insurance and other sources of health insurance available,
##        holding all other covariates at their mean values. Report a 95% confidence interval
##        (using simulation) for your estimate.
 

X <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=1, othhi=1, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean) 

exp.prob <-rep(NA, M)
for(i in 1:M){
  pi  <- 1/(1+exp(-X%*%sim.betas[i,]))
  exp.prob[i] <- mean(rbinom(n=m,size=1,prob=pi)) #n= number of obs size=number of trials
}

# 95% CI
quantile(exp.prob, c(0.025,0.975))  
# Point Estimate
mean(exp.prob)

as.data.frame(exp.prob) %>% ggplot(aes(exp.prob)) +
  geom_density(fill="yellow")+
  ggtitle("Expected Probability")+
  xlab("Expected Probability")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(exp.prob), colour='red')





##    c) The expected probability of voluntarily changing jobs for a white married man without
##    employer-provided health insurance and no other sources of health insurance available,
##    holding all other covariates at their mean values. Report a 95% confidence interval
##    (using simulation) for your estimate.

X <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=0, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean) 

exp.prob <-rep(NA, M)
for(i in 1:M){
  pi  <- 1/(1+exp(-X%*%sim.betas[i,]))
  exp.prob[i] <- mean(rbinom(n=m,size=1,prob=pi)) #n= number of obs size=number of trials
}

# 95% CI
quantile(exp.prob, c(0.025,0.975))  

# Point Estimate
mean(exp.prob)


as.data.frame(exp.prob) %>% ggplot(aes(exp.prob)) +
  geom_density(fill="yellow")+
  ggtitle("Expected Probability")+
  xlab("Expected Probability")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(exp.prob), colour='red')




##     d) The expected probability of voluntarily changing jobs for a white married man without
##     employer-provided health insurance but other sources of health insurance available,
##     holding all other covariates at their mean values. Report a 95% confidence interval
##     (using simulation) for your estimate.

X <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=1, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean) 

exp.prob <-rep(NA, M)
for(i in 1:M){
  pi  <- 1/(1+exp(-X%*%sim.betas[i,]))
  exp.prob[i] <- mean(rbinom(n=m,size=1,prob=pi)) #n= number of obs size=number of trials
}

# 95% CI
quantile(exp.prob, c(0.025,0.975))  


# Point Estimate
mean(exp.prob)

as.data.frame(exp.prob) %>% ggplot(aes(exp.prob)) +
  geom_density(fill="yellow")+
  ggtitle("Expected Probability")+
  xlab("Expected Probability")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(exp.prob), colour='red')



## 1.C Calculate:
##     a) The expected difference in probability of voluntarily changing jobs between a white
##     married man with employer-provided health insurance, but no other coverage and a
##     white married man without employer-provided health insurance and no other coverage,
##     holding all other covariates at their mean values. Report a 95% confidence interval
##     (using simulation) for your estimate.

# Take the difference in the expected probabilities 
X1 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=1, othhi=0, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)   

X0 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=0, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)  

first.diff <-rep(NA, M)
for(i in 1:M){
  pi.1  <- 1/(1+exp(-X1%*%sim.betas[i,]))
  pi.0  <- 1/(1+exp(-X0%*%sim.betas[i,]))
  first.diff[i] <- mean(rbinom(n=m,size=1,prob=pi.1))-mean(rbinom(n=m,size=1,prob=pi.0))
}

# 95% CI
quantile(first.diff, c(0.025, 0.975))  
# Point Estimate
mean(first.diff)  
 

as.data.frame(first.diff) %>% ggplot(aes(first.diff)) +
  geom_density(fill="yellow")+
  ggtitle("First diference in probability of voluntarily changing jobs: Effect of Job Lock on no-other-hi")+
  xlab("First diference")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(first.diff), colour='red')


##     b) The expected diference in probability of voluntarily changing jobs between a white
##     married man without employer-provided health insurance and no other coverage and a
##     white married man without employer-provided health insurance, but with other coverage 
##     holding all other covariates at their mean values. Report a 95% confidence interval
##     (using simulation) for your estimate.

# Take the difference in the expected probabilities 
X1 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=0, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)   

X0 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=1, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)  

first.diff <-rep(NA, M)
for(i in 1:M){
  pi.1  <- 1/(1+exp(-X1%*%sim.betas[i,]))
  pi.0  <- 1/(1+exp(-X0%*%sim.betas[i,]))
  first.diff[i] <- mean(rbinom(n=m,size=1,prob=pi.0))-mean(rbinom(n=m,size=1,prob=pi.1))
}

# 95% CI
quantile(first.diff, c(0.025, 0.975))  
mean(first.diff)  


as.data.frame(first.diff) %>% ggplot(aes(first.diff)) +
  geom_density(fill="yellow")+
  ggtitle("First diference in probability of voluntarily changing jobs:Effect of other-hi on no-hi")+
  xlab("First diference")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(first.diff), colour='red')


## 1.D
## What do the results from 1.B and 1.C reveal about the existence or non-existence of job lock?
## How does having employer-provided heath insurance affect the probability of voluntarily
## changing jobs? How does having other sources of insurance moderate that relationship? 
## Is this consistent with the job lock hypothesis?

# If compare results from 1B.a with 1B.c, that is 1C.a, for employees without other sources of insurance, emploer-provided 
# health insurance lowers the probability of seeking new employment opportunities, average difference of expected probabilities
# is -0.1686. The 95% CI is (-0.2214, -0.1209), it doesn't include 0, thus we can reject the null and conclude that 
# job lock has a significant effect for employees without other health insurance, at 5% significance level.

# If compare results from 1B.b with 1B.d, for employees with other health insurance, employer-provided
# health insurance also lowers the probability of seeking new employment opportunities, avergae difference of expected probabilities
# is -0.1098. The 95% CI is (-0.1579, -0.0655 ), it doesn't include 0, thus we can reject the null and conclude that 
# job lock has a significant effect even for employees with other health insurance, at 5% significance level.

# Therefore, job lock indeed exists, employees are less likely to seek new employment opportunities if employer provide
# health insurance, regardless of whether they had other insurance coverage, and all other socio-economic factors we
# have adjusted. However, having other sources of health insurance does moderate this relationship, it weakens the 
# effect of job lock, the average effect of job lock shrinks from a 16.86% reduction to 10.98% reduction. 
# It makes sense because employees with other sources of insurance tends to less worry about losing employer-provided health insurance 
# simply because they have backup plans, thus job is less a constrain (but still is a constrain) or factor when considering 
# new employment opportunities.
 
# compare results from 1B.b with 1B.d 
X1 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=1, othhi=1, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)   

X0 <- finaldata1 %>%  
  mutate(Intercept=1)%>%
  select(Intercept, hi, othhi, lhwage, winc, nfam, educ, exper, afram) %>%
  mutate(hi=0, othhi=1, afram=0)%>%
  mutate("hi*othhi"=hi*othhi) %>%
  apply(MARGIN=2, FUN=mean)  

first.diff <-rep(NA, M)
for(i in 1:M){
  pi.1  <- 1/(1+exp(-X1%*%sim.betas[i,]))
  pi.0  <- 1/(1+exp(-X0%*%sim.betas[i,]))
  first.diff[i] <- mean(rbinom(n=m,size=1,prob=pi.1))-mean(rbinom(n=m,size=1,prob=pi.0))
}

# 95% CI
quantile(first.diff, c(0.025, 0.975))  
mean(first.diff)  


as.data.frame(first.diff) %>% ggplot(aes(first.diff)) +
  geom_density(fill="yellow")+
  ggtitle("First diference in probability of voluntarily changing jobs")+
  xlab("First diference")+
  scale_x_continuous(breaks = seq(0 , max(exp.prob) , 0.01) )+
  geom_vline(xintercept=mean(first.diff), colour='red')
 




# Problem 2: Tobit Regression


## 2.B Write a function in R that evaluates the log-likelihood.
tobit.ll <- function(par, outcome, covariates, indicator, c) {
  
  # check to see if the first column is all 1's
  # if not, add 1's for the intercept column
  if(!all(covariates[,1]==1)){covariates<-as.matrix(cbind(1,covariates))}
  
  xb <-covariates %*% par[-length(par)] 
  sigma  <- sqrt(exp(par[length(par)])) # last parameter is log(sigma^2) 
  
  # if observed that is yi>c
  p1 <-dnorm(outcome, mean = xb, sd = sigma , log=TRUE )
  # if censored tht is yi<=c
  p0 <-pnorm(c, mean = xb, sd = sigma , lower.tail = TRUE, log.p =TRUE)
  # log-likelihood
  sum(indicator*p1+(1-indicator)*p0) 
  
}
 
## 2.C Using your log-likelihood function and optim(), run a tobit regression where durable is the dependent
##     variable and age and quant are the independent variables. In this case, the dependent variable is 
##     censored at c = 0. Report estimates and standard errors for the beta's and report an estimate of
##     sigma in a table. Do not use a canned tobit routine for this part.
library(Zelig)
data(tobin)
 
# create X matrix
X<-as.matrix(cbind(1,select(tobin, age, quant)))
c<-0
d<-ifelse(tobin$durable>c,1,0)
MLE <- optim(par=rep(1,ncol(X)+1),
             fn=tobit.ll,
             covariates=X,
             outcome=tobin$durable,
             indicator=d,
             c=c,
             control=list(fnscale=-1, maxit=1000),
             hessian = T,
             method = "BFGS")

# MLE coefficient estimates are:
Effect <-MLE$par
Effect[length(Effect)] <-sqrt(exp(Effect[length(Effect)])) # last parameter is log(sigma^2)
# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <-solve(obs_fisher_info)
SE <-sqrt(diag(vcov))
SE[length(SE)] <-sqrt(exp(SE[length(SE)])) # last parameter is log(sigma^2)
## Vaiable names
Variable <- c("Intercept",
              "age",
              "quant",
              "sigma")
## Put into a data frame
MLE.table<-data.frame(Variable, Effect, SE)
library(xtable)
xtable(MLE.table) 

# check results
fit1<- zelig(durable ~ age+quant, below = 0, above = Inf, model="tobit", data = tobin) 
summary(fit1)# Correct!!!
library(AER)
fit2<-tobit(durable ~ age+quant, left = 0, right = Inf, dist = "gaussian", data = tobin) 
summary(fit2)# Correct!!!




## 2.D Generate a histogram of 10,000 expected values of Y_i with quant at it's 20th percentile value
##     and age at its mean. Set your seed to 12345 and do not set the seed again for the rest of Problem 2.

# Step1: Simulate M beta's from sampling distribution
# Step2: Choose one value for each explanatory variable
# Step3: Take one simulated beta's, compute mu and sigma from the systematic component (This step simulate estimation uncertainty)
# Step4: Draw m times of the latent outcome variable from the stochastic component (Normal distribution) (This step simulate fundamental uncertainty)
# Step5: Average over the fundamental unceratinty by calculating the mean of the m simulations to yield one simulated expected value of latent variable Y_i*
# Step6: Our quantity of interest is the left-censored dependent variable Y_i, if Y_i* >0 then expected Y_i=Y_i*; if Y_i*<=0 then Y_i=0 
# (To save computation time and improve approximation, use the same simulated beta's for 1.B(a)-(d) and 1.C )

set.seed(12345)
M<-10000 
m<-10000
beta <-MLE$par
vcov <-solve(-MLE$hessian)
sim.betas <- rmvnorm(n=M, mean=beta, sigma=vcov) # last column is unbounded log(sigma^2)

X<-tobin%>% summarise(Intercept=1,quant=quantile(quant,0.2), age=mean(age))%>%as.matrix()

exp.Y  <-rep(NA, M)
for(i in 1:M){
   
  mu <- X%*%sim.betas[i,-ncol(sim.betas)]
  sigma <-sqrt(exp(sim.betas[i,ncol(sim.betas)]))
  # draw latent dependent variable Y_i* from N(mu, sigma2), m=10000 times and then average away fundamental uncertainty
  # if expected Y_i*>0 then expected Y_i=Y_i*, otherwise expected Y_i=c=0
  exp.Y[i]<-ifelse(mean(rnorm(n=m, mean=mu, sd=sigma))>0,mean(rnorm(n=m, mean=mu, sd=sigma)), 0)
  
}
 
as.data.frame(exp.Y) %>% ggplot(aes(exp.Y)) +
  geom_histogram(fill="pink",binwidth=5)+
  ggtitle("histogram of 10,000 expected values of Y_i with quant at 20th percentile and average Age")+
  xlab("Expected Y_i") +
  geom_vline(xintercept=mean(exp.Y), colour='red')+
  stat_bin(geom="text", aes(label=..count..) , vjust = -1) + 
  scale_x_continuous(breaks = seq(0 , max(exp.Y) , 5 ) )+
  annotate("text", x = mean(exp.Y), y = 1000, label = "Mean")
   
 
quantile(exp.Y, c(0.025,0.5,0.975))
 

## 2.E Generate a histogram of 10,000 expected values with quant at it's 80th percentile value and age at its mean.
X<-tobin%>% summarise(Intercept=1,quant=quantile(quant,0.8), age=mean(age))%>%as.matrix()

exp.Y  <-rep(NA, M)
for(i in 1:M){
  
  mu <- X%*%sim.betas[i,-ncol(sim.betas)]
  sigma <-sqrt(exp(sim.betas[i,ncol(sim.betas)]))
  # draw latent dependent variable Y_i* from N(mu, sigma2), m=10000 times and then average away fundamental uncertainty
  # if expected Y_i*>0 then expected Y_i=Y_i*, otherwise expected Y_i=c=0
  exp.Y[i]<-ifelse(mean(rnorm(n=m, mean=mu, sd=sigma))>0,mean(rnorm(n=m, mean=mu, sd=sigma)), 0)
  
}

as.data.frame(exp.Y) %>% ggplot(aes(exp.Y)) +
  geom_histogram(fill="pink",binwidth=5)+
  ggtitle("histogram of 10,000 expected values of Y_i with quant at 80th percentile and average Age")+
  xlab("Expected Y_i") +
  geom_vline(xintercept=mean(exp.Y), colour='red')+
  stat_bin(geom="text", aes(label=..count..) , vjust = -1) + 
  scale_x_continuous(breaks = seq(0 , max(exp.Y) , 5 ) )+
  annotate("text", x = mean(exp.Y), y = 1000, label = "Mean")



quantile(exp.Y, c(0.025,0.5,0.975))


## 2.F 
## Setting age at its mean, generate histogram of 10,000 first differences going from the 
## 20th percentile of quant to the 80th percentile of quant. (Note: You should draw your sigma parameter on the
## log (unbounded) scale when drawing the simulated parameters)
X1<-tobin%>% summarise(Intercept=1,quant=quantile(quant,0.8), age=mean(age))%>%as.matrix()
X0<-tobin%>% summarise(Intercept=1,quant=quantile(quant,0.2), age=mean(age))%>%as.matrix()

exp.diff  <-rep(NA, M)
for(i in 1:M){
  sigma  <-sqrt(exp(sim.betas[i,ncol(sim.betas)]))
  mu1 <- X1%*%sim.betas[i,-ncol(sim.betas)]
  mu0 <- X0%*%sim.betas[i,-ncol(sim.betas)]
  
  # draw latent dependent variable from N(mu, sigma2), m=10000 times and then average away fundamental uncertainty
  # if latent dependent variable>0 then dependent variable=observe, if latent dependent variable<=0 then dependent variable=0
  exp1<-ifelse(mean(rnorm(n=m, mean=mu1, sd=sigma))>0,mean(rnorm(n=m, mean=mu1, sd=sigma)), 0)
  exp0<-ifelse(mean(rnorm(n=m, mean=mu0, sd=sigma))>0,mean(rnorm(n=m, mean=mu0, sd=sigma)), 0)
  exp.diff[i]<-exp1-exp0
}

as.data.frame(exp.diff) %>% ggplot(aes(exp.diff)) +
  geom_histogram(fill="pink",binwidth = 1)+
  ggtitle("histogram of 10,000 first differences: 80% Quant vs 20% Quant at average Age")+
  xlab("First Difference")+
  geom_vline(xintercept=mean(exp.diff), colour='red')+
  stat_bin(geom="text", aes(label=..count..) , vjust = -1) + 
  scale_x_continuous(breaks = seq(0 , max(exp.diff) , 1) )+
  annotate("text", x = mean(exp.diff), y = 1000, label = "Mean")


quantile(exp.diff, c(0.025,0.5,0.975))

## 2.G What is the average first difference and its standard error?
mean(exp.diff)
sd(exp.diff)



