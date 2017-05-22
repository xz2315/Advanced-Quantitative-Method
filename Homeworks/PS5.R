##### Problem Set 5
##### Xiner Zhou
##### 3/8/2016


# Problem 1: Preplicating Fearon and Laitin (2003)

#### 1.D

# log-likelihood funciton, logit.ll takes three arguments:
## par: the parameters
## outcome: the Y variable
## covariates: the X matrix or design matrix (includes an intercept column)
logit.ll <-function(par,outcome,covariates){
  #check to see if the first column is all 1's
  #If it isn't add the 1's for the intercept term
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
  xb<-covariates%*%par
  -sum(log(1+exp((1-2*outcome)*xb)))
}

#### 1.E

# First set working directory
setwd("/Volumes/DISK_IMG/Files/Advanced Quantitative Method-Gary King/Week 5")
# Now read Stata file using 'foreign' packages
library("foreign")
dat <-read.dta("fearondata.dta")

# Data Wrangling
##include only variales in Model 1
## delete all rows containing missing data using na.omit()
## fix the value of onset that was incorrectly coded as 4 to be 1
library(dplyr)
names(dat)
dat <-select(dat, onset, warl, gdpenl, lpopl1, lmtnest, ncontig, Oil, nwstate, instab, polity2l, ethfrac, relfrac)
dat <-na.omit(dat)
dat$onset <- ifelse(dat$onset==4,1,dat$onset)
 
# create the X matrix
design.matrix <- as.matrix(cbind(1,select(dat,-onset)))
 
# estimate the MLE:
MLE <- optim(par=rep(0,ncol(design.matrix)),
             fn=logit.ll,
             covariates=design.matrix,
             outcome=dat$onset,
             control = list(fnscale=-1),
             hessian=T,
             method = "BFGS")

# MLE coefficient estimates are:
coefs <- MLE$par
names(coefs)<-c("Intercept",
                "a dsitinct war was ongoing in previouis year",
                "lagged per capita income, thousands of US dollars",
                "lagged natural logarithm of population",
                "log % of country nountainous terrain",
                "country is non-contiguous geographically",
                "country is an oil exporter",
                "country achieved independence in past two years",
                "significant change in political score in last 3 years, lagged",
                "lagged level of democratization",
                "a measure of ethnic gragmentation",
                "a measure of religious gragmentation")

coefs

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <- solve(obs_fisher_info)
se<-sqrt(diag(vcov))
names(se)<-c("Intercept",
                "a dsitinct war was ongoing in previouis year",
                "lagged per capita income, thousands of US dollars",
                "lagged natural logarithm of population",
                "log % of country nountainous terrain",
                "country is non-contiguous geographically",
                "country is an oil exporter",
                "country achieved independence in past two years",
                "significant change in political score in last 3 years, lagged",
                "lagged level of democratization",
                "a measure of ethnic gragmentation",
                "a measure of religious gragmentation")

se

#### 1.F
 
# calculate the median for all covaries
a<-as.vector(sapply(dat[,2:12], median, 2))
# Asuume Oil exporter
a[6]<-1
 
# create X matrix
design.matrix1F <- matrix(c(1,a), nrow=1, ncol=12)
 
# Predicted probability of civil war for oil exporters, with all other covariaets set to median
pred.prob <-1/(1+exp(-design.matrix1F %*% coefs))
pred.prob #0.02730511

#### 1.G: Likelihood Ratio Test

# restricted model: exclude ethic and religious fragmentation
design.matrix1G <- design.matrix[,-c(11:12)]
restricted <-optim(par=rep(0,ncol(design.matrix1G)),
                   fn=logit.ll,
                   covariates=design.matrix1G,
                   outcome=dat$onset,
                   control = list(fnscale=-1),
                   hessian=T,
                   method = "BFGS")
# unrestricted model
unrestricted <-optim(par=rep(0,ncol(design.matrix)),
                     fn=logit.ll,
                     covariates=design.matrix,
                     outcome=dat$onset,
                     control = list(fnscale=-1),
                     hessian=T,
                     method = "BFGS")
  
# calculate LR stat
LR <-2*(unrestricted$value-restricted$value)
LR

# calculate p-value, that is, thebprobability of seeing likelihood ratio test stat more extreme or as extreme as this observed one
pchisq(LR,df=2,lower.tail=F)
# At 0.05 significance level, I Accept the null, and I conculde that ethic and religious fragmentation have no significant effect on whether a country had a civil war.

#### 1.H
# predicted probability for each observation
pred.probs  <-1/(1+exp(-design.matrix %*% coefs))
# Brier score
B<-mean((pred.probs-dat$onset)^2)
B


# Problem 2: Different Link Function

#### 2.B:
# log-likelihood funciton, p2.ll takes three arguments:
## par: the parameters
## outcome: the Y variable
## covariates: the X matrix or design matrix (includes an intercept column)
p2.ll <-function(par,outcome,covariates){
  #check to see if the first column is all 1's
  #If it isn't add the 1's for the intercept term
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
    xb<-covariates%*%par
     
    sum(outcome*log(exp(exp(xb))-1)-exp(xb))
}
 
# estimate the MLE:
 
  MLE <- optim(par=rep(-0.3194182,ncol(design.matrix)),
             fn=p2.ll,
             covariates=design.matrix,
             outcome=dat$onset,
             control = list(fnscale=-1),
             hessian=T,
             method = "BFGS")
 


# MLE coefficient estimates are:
coefs_p2 <- MLE$par
names(coefs_p2)<-c("Intercept",
                "a dsitinct war was ongoing in previouis year",
                "lagged per capita income, thousands of US dollars",
                "lagged natural logarithm of population",
                "log % of country nountainous terrain",
                "country is non-contiguous geographically",
                "country is an oil exporter",
                "country achieved independence in past two years",
                "significant change in political score in last 3 years, lagged",
                "lagged level of democratization",
                "a measure of ethnic gragmentation",
                "a measure of religious gragmentation")

coefs_p2

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <- solve(obs_fisher_info)
se_p2<-sqrt(diag(vcov))
names(se_p2)<-c("Intercept",
             "a dsitinct war was ongoing in previouis year",
             "lagged per capita income, thousands of US dollars",
             "lagged natural logarithm of population",
             "log % of country nountainous terrain",
             "country is non-contiguous geographically",
             "country is an oil exporter",
             "country achieved independence in past two years",
             "significant change in political score in last 3 years, lagged",
             "lagged level of democratization",
             "a measure of ethnic gragmentation",
             "a measure of religious gragmentation")

se_p2

#### 2.D:

# Predicted probability of civil war for oil exporters, with all other covariaets set to median
pred.prob_p2 <- 1-exp(-exp(design.matrix1F %*% coefs))
pred.prob_p2 # 0.02768126


#### 2.E:
# predicted probability for each observation
pred.probs  <- 1-exp(-exp(design.matrix %*% coefs))
# Brier score
B_p2<-mean((pred.probs-dat$onset)^2)
B_p2 # 0.01586623

#### 2.F: Compare two models

# Put coefficient estimates and standard errors, Brier Scores together
compare <- list (coefs, coefs_p2, se, se_p2, pred.prob, pred.prob_p2, B, B_p2)
compare
# Based on all the results above, the two models don't give qualitatively different results. 
# Researher might want to choose the first model using logit link, because the logit model has 
# an intuitive interpretation, which assumes that there is an unobserved data generating process
# follows a standard logistic distribution, and the mechanism we observe outcome as 1/0 depends on 
# whether the underlying measure exceeds a certain threshold or not. For example, we can think of 
# the civil tension leading to outbreak of war, which is unobservable, affected by all the covariates, 
# when the tension greater than the level the society could tolerate, a civil war occurs.
