##### Problem Set 6
##### Xiner Zhou
##### 3/15/2016


# Problem 1: Preplicating Fearon and Laitin (2003)

#### 1.C

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

 

# First set working directory
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 6")
# Now read Stata file using 'foreign' packages
library("foreign")
dat1 <-read.dta("fearondata.dta")

# Data Wrangling

##include only variales in Model 1
## delete all rows containing missing data using na.omit()
## fix the value of onset that was incorrectly coded as 4 to be 1
library(dplyr)
 
dat1 <-select(dat1, onset, warl, gdpenl, lpopl1, lmtnest, ncontig, Oil, nwstate, instab, polity2l, ethfrac, relfrac)
dat1 <-na.omit(dat1)
dat1$onset <- ifelse(dat1$onset==4,1,dat1$onset)
 
# create the X matrix
design.matrix <- as.matrix(cbind(1,select(dat1,-onset)))

# estimate the MLE:
MLE <- optim(par=rep(0,ncol(design.matrix)),
             fn=logit.ll,
             covariates=design.matrix,
             outcome=dat1$onset,
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
varcov <- solve(obs_fisher_info)
se<-sqrt(diag(varcov))
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

#### 1.D:

# To evaluate the role of various explanatory variables in increasing the risk of civil war,
# we will calculate some expected values and first differences.

# First, let's establish typical covariate values for states which are at risk of civil war.
# Using the apply() function,calculate the median value of all of the explanatory variables in states which experienced
# a civil war, using data from the year(s) of civil war onset only. 

Xc <-dat1 %>%filter(onset==1) %>% select(warl, gdpenl, lpopl1, lmtnest, ncontig, Oil, nwstate, instab, polity2l, ethfrac, relfrac)%>% apply(MARGIN =2, FUN=median)

# Add 1 for intercept
Xc<-c(1,Xc)
names(Xc)[1]<-"Intercept"

# Report the median values for warl, gdpenl, polity21, and ethfrac.
Xc[c("warl","gdpenl","polity2l","ethfrac")]


#### 1.E:

# Use simulation to calculate the expected difference in the probability of civil war onset
# between an oil producer and a non-oil producer. This is also known as a first difference.

# Step 1: Write out your model and estimate betahat and the Variance-Covariance Matrix
# Step 2: Simulate beta tildes, M=10000 times, from a multivariate normal distribution with mean and variance=our estimates,
#         to account for estimation uncertainty
library(mvtnorm)
set.seed(8766)
sim.betas <- rmvnorm(n=10000, mean=coefs, sigma=varcov)
dim(sim.betas)

# Step 3: Choose median for each explanatory variable, store as Xc1 for oil exporter and Xc0 for non-oil exporter
Xc0 <- Xc
Xc0["Oil"] <-0
Xc1 <- Xc
Xc1["Oil"] <-1

# Step 4: Use Xc0 and Xc1 and simulated beta tildes, Calculate the systematic component pi tilde
# Step 5: For each pi tilde draw m=10000 simulations from the stochastic component,
#         to account for fundamental uncertainty
# Step 6: Store the mean these m simulations for each pi tilde, average away fundamental uncertainty, 
#         as the expected value for oil exporters, for each pi tilde
# Step 7: Repeat step 4-6 for non-oil exporters and store the expected value for non-oil exporters, for each pi tilde
# Step 8: Calculate the expected differnece for each pi tilde (estimation uncertainty)
M<-10000
m<-1000000
exp.diff <-rep(NA, M)
set.seed(02138)
for(i in 1:M){
  pi0.tilde <- 1/(1+exp(-Xc0%*%sim.betas[i,]))
  pi1.tilde <- 1/(1+exp(-Xc1%*%sim.betas[i,]))
  exp.diff[i] <- mean(rbinom(n=m,size=1, prob=pi1.tilde))-mean(rbinom(n=m,size=1, prob=pi0.tilde))
}

# Look at the results
library(ggplot2)
as.data.frame(exp.diff)%>% ggplot(aes(exp.diff))+geom_histogram()+ ggtitle("First Differences")+xlab("Difference in Predicted Probability \n
          for Oil Exporter abd non-Oil Exporter")+geom_vline(xintercept=mean(exp.diff),colour="red")
 
 
# Provide a 95% confidence interval based on the appropriate quantiles of the simulations
quantile(exp.diff,c(0.025,0.975))

 
#### 1.F:

# Use simulation to calculate the expected probability of civil war onset for recently independent
# countries (according to the definition given by the authors) and non-recently independent countries, 
# as lagged GDP per capita increases from $250 to $8000 (note that the dataset has GDP in thousands,
# so this is .25 to 8 in the dataset).

gdpenl.vc <-seq(from=0.25, to=8, by=0.05)
prob1 <- matrix(data=NA,nrow=length(gdpenl.vc),ncol=M)  # store predicted probability for recently independent coutries
prob0 <- matrix(data=NA,nrow=length(gdpenl.vc),ncol=M)   # store predicted probability for non-recently independent coutries

m <-10000
for(j in 1:length(gdpenl.vc)){
  Xc0 <- Xc
  Xc0[8] <-0
  Xc0[3] <-gdpenl.vc[j]
  Xc1 <- Xc
  Xc1[8] <-1
  Xc1[3] <-gdpenl.vc[j]
  
  for(i in 1:M){
  pi0.tilde <- 1/(1+exp(-Xc0%*%sim.betas[i,]))
  pi1.tilde <- 1/(1+exp(-Xc1%*%sim.betas[i,]))
  prob1 [j,i] <- mean(rbinom(n=m,size=1, prob=pi1.tilde)) #size is number of trials for one draw
  prob0 [j,i] <- mean(rbinom(n=m,size=1, prob=pi0.tilde))
  }
}
prob <-rbind(prob0,prob1)

# calculate average predicted probability for each lagged GDP per capita increases from $250 to $8000
mean <-apply(prob,MARGIN=1,FUN=mean)
 
# calculate Lower confidence limit of predicted probability for each lagged GDP per capita increases from $250 to $8000
lower<-apply(prob ,MARGIN=1,FUN=quantile, 0.025 )

# calculate Upper confidence limit of predicted probability for each lagged GDP per capita increases from $250 to $8000
upper<-apply(prob ,MARGIN=1,FUN=quantile, 0.975)

# Plot these results as two lines on a clearly labeled graph with the probability of onset on the
# y-axis and GDP per capita on the x-axis.
dat<-data.frame(GDP=c(gdpenl.vc,gdpenl.vc)*1000, mean, lower, upper, nwstate=c(rep("non-recently independent countries",length(gdpenl.vc)),rep("recently independent countries",length(gdpenl.vc))))
pdf("Problem 1F.pdf")
dat%>% ggplot(aes(x=GDP,y=mean,colour=nwstate))+geom_point()+geom_segment(aes(x = GDP, y = lower, xend = GDP, yend = upper, colour = nwstate))+theme(legend.position = "bottom")+labs(x="lagged GDP per capita increases from $250 to $8000",y="Expected Probability of civil war onset",title="expected probability of civil war onset \n for recently independent and non-recently independent countries")
dev.off()









# Problem 2: Preplicating Logic of Political Survival

#### 2.C

# last parameter in par is the Gamma, sigma^2=exp(Gamma)
normal.ll <-function(par,outcome,covariates){
  #check to see if the first column is all 1's
  #If it isn't add the 1's for the intercept term
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
  beta <-par[-length(par)] # parameter for the mean
  gamma <- log(par[length(par)]) # repameterize for theta
  
  xb<-covariates%*%beta
  (-1/2)*sum(gamma+(outcome-xb)^2/exp(gamma))
}

# read in dataset
dat2<-read.csv("revolutions.csv")
 
design.matrix<-cbind(1,dat2$GenStrikes,dat2$GovCrises,dat2$Riots,dat2$polity2)

# estimate the MLE:
MLE <- optim(par=rep(1,ncol(design.matrix)+1),
             fn=normal.ll,
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
                "sigma: error term")
coefs

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
varcov <- solve(obs_fisher_info) 
se<-sqrt(diag(varcov)[1:5]) 
names(se)<-c("Intercept",
                "GEnStrikes: general strikes",
                "GovCrises: government crises",
                "Riots: riots",
                "polity2: polity score")
se

#### 2.D: predicted values for a democracy (polity2=10), with all other variables held at their mean

# Get mean for covariates and re-set democracy
Xc <-dat2 %>% select(GenStrikes,GovCrises,Riots,polity2)%>% apply(MARGIN =2, FUN=mean)
Xc["polity2"]<-10
# Add 1 for intercept
Xc<-c(1,Xc)
names(Xc)[1]<-"Intercept"
Xc


# Simulate beta tildes and gamma from posterior distribution 
# then transform gamma to sigma^2 to get stochastic component
library(mvtnorm)
set.seed(8766)
M<-10000000 # number of draws
sim.par <- rmvnorm(n=M, mean=coefs, sigma=varcov)
sim.par[,6]<-sqrt(exp(sim.par[,6])) # simulated sigma  for the error term


pred <-rep(NA, M) # place-holder for simulated predicted value

for(i in 1:M){
  mu <- Xc%*%sim.par[i,1:5]
  pred[i]<-rnorm(n=1,mean=mu,sd=sim.par[i,6])
}

# Generate a histogram with 10,000 predicted values for a democracy (polity2=10), with
# all other variables held at their mean. Set the seed to 8766.
library(ggplot2)

pdf("Problem 2D.pdf")
as.data.frame(pred) %>% 
  mutate(class=ifelse(pred<quantile(pred,0.025),"Below 2.5%",ifelse(pred>quantile(pred,0.975),"Upper 97.5%","Between 95% Confidence Interval")))%>%
  ggplot(aes(pred,fill=class))+geom_histogram()+stat_bin(binwidth = 0.0001)+
  ggtitle("Predicted Number of Revolutions for a Democracy")+xlab("Number of Revolutions")+
  geom_vline(xintercept=mean(pred),colour="red")
dev.off()



#### 2.E: Based on your simulations, calculate the probability that 
####      such a state has one or more revolutions in a given year.
mean(as.numeric(pred>=1))









# Problem 3: Now we will use a Poisson model to estimate the same regression as in Problem 2.


#### 3.C: Optimize the model to find the MLE for the ??s. Report the coefficients and standard errors.

# log-likelihood function for Poisson regression
poisson.ll <-function(par,outcome,covariates){
  #check to see if the first column is all 1's
  #If it isn't add the 1's for the intercept term
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
  
  xb<-covariates%*%par
  sum(outcome*xb-exp(xb))
}

# read in dataset
dat2<-read.csv("revolutions.csv")

design.matrix<-cbind(1,dat2$GenStrikes,dat2$GovCrises,dat2$Riots,dat2$polity2)

# estimate the MLE:
MLE <- optim(par=rep(0,ncol(design.matrix)),
             fn=poisson.ll,
             covariates=design.matrix,
             outcome=dat2$Revolutions,
             control = list(fnscale=-1),
             hessian=T,
             method = "BFGS")

# MLE coefficient estimates are:
coefs <- MLE$par
names(coefs)<-c("Intercept",
                "GEnStrikes: general strikes",
                "GovCrises: government crises",
                "Riots: riots",
                "polity2: polity score")
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
             "polity2: polity score")
se


#### 3.D:Generate a histogram of 10,000 predicted values for a democracy (polity2=10), 
####     with all other variables held at their mean. Set the seed to 8766.

# Get mean for covariates and re-set democracy
Xc <-dat2 %>% select(GenStrikes,GovCrises,Riots,polity2)%>% apply(MARGIN =2, FUN=mean)
Xc["polity2"]<-10
# Add 1 for intercept
Xc<-c(1,Xc)
names(Xc)[1]<-"Intercept"
Xc


# Simulate beta tildes and gamma from posterior distribution 
# then transform gamma to sigma^2 to get stochastic component
library(mvtnorm)
set.seed(8766)
sim.par <- rmvnorm(n=10000, mean=coefs, sigma=varcov)
 
M<-10000 # number of draws
pred <-rep(NA, M) # place-holder for simulated predicted value

for(i in 1:M){
  mu <- exp(Xc%*%sim.par[i,]) # expected value for each simulaton exp(X*beta)=lambda, estimation uncertainty
  pred[i]<-rpois(n=1, lambda=mu) # predicted value for each simulation, fundamental uncertainty
}

# Generate a histogram with 10,000 predicted values for a democracy (polity2=10), with
# all other variables held at their mean. Set the seed to 8766.
library(ggplot2)

pdf("Problem 3D.pdf")
as.data.frame(pred) %>% 
  ggplot(aes(pred))+geom_histogram(aes(col="pink",fill=T))+
  ggtitle("Predicted Number of Revolutions for a Democracy")+xlab("Number of Revolutions")+
  theme(legend.position="none")
dev.off()



#### 3.E: Based on your simulations, calculate the probability that 
####      such a state has one or more revolutions in a given year.
mean(as.numeric(pred>=1))


#### 3.F: Come up with another quantity of interest that describes the output from this model. 
####      Use simulation to estimate this quantity of interest, and attach a plot that displays the quantity of interest below. 
 
# Simulate expected number of revolutions for a range of polity2

# Get mean for covariates and re-set democracy
Xc <-dat2 %>% select(GenStrikes,GovCrises,Riots,polity2)%>% apply(MARGIN =2, FUN=mean)
# Add 1 for intercept
Xc<-c(1,Xc)
names(Xc)[1]<-"Intercept"
Xc


# Simulate beta tildes and gamma from posterior distribution 
# then transform gamma to sigma^2 to get stochastic component
library(mvtnorm)
set.seed(8766)
sim.par <- rmvnorm(n=10000, mean=coefs, sigma=varcov)

M<-10000 # number of draws

# hypotheticla value for polity2
polity2<-seq(min(dat2$polity2), max(dat2$polity2), 0.1)

pred <-matrix(NA, nrow=M,ncol=length(polity2)) # place-holder for simulated predicted value

for(j in 1:length(polity2)){
  Xc["polity2"]<-polity2[j]
  for(i in 1:M){
  mu <- exp(Xc%*%sim.par[i,]) # expected value for each simulaton exp(X*beta)=lambda, estimation uncertainty
  pred[i,j]<-rpois(n=1, lambda=mu) # predicted value for each simulation, fundamental uncertainty
  }
}

# Plot these results as one lines with CI on a clearly labeled graph  

# calculate average predicted value
mean <-apply(pred,MARGIN=2,FUN=mean)

dat<-data.frame(polity2, mean)

dat%>% ggplot(aes(x=polity2,y=mean,col="pink"))+geom_point()+theme(legend.position = "none")+labs(x="polity2",y="Expected number of revolutions",title="Expected number of revolutions for different level of polity2 \n set all other covariates to mean") 







# Problem 4 : Robust Standard Error

# Read in data
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 6")
load("robust.Rdata") 
dat4 <-data.frame(y,X1,X2)

#### 4.A: Use lm() to run a regression of y on X1 and X2. 
####      Report the requested coefficients and standard errors.
fmla <- as.formula(y~ X1+X2)
ols  <- lm(formula=fmla, data=dat4)
summary(ols)

#### 4.B: Using the output of your regression, you are now going to calculate the robust
####      variance-covariance matrix by hand.

# Method 1:
# Step 1. Use estfun() to calculate the meat of the sandwich
# Step 2. Use vcov() to calculate the bread of the sandwich
# Step 3. Calculate your robust variance-covariance matrix
# Step 4. Check your robust standard errors with the hccm function using option hc0.

library(sandwich)
# bread = vairance-covariance matrix
bread<-vcov(ols)
# meat=dot product of the score
est.fun <-estfun(ols)
meat<-crossprod(est.fun)/nrow(dat4)

# sandwich estimator
robust <-sandwich(ols, meat=meat)
# robustse<-sqrt(diag(robust))

# put estimate and robust standard error back in table
library(lmtest)
coeftest(ols, robust) # compare with classical standard error: coeftest(ols)

# hccm: Calculates heteroscedasticity-corrected covariance matrices linear models 
# fit by least squares or weighted least squares. These are also called "White-corrected" 
# or "White-Huber" covariance matrices.

# The classical White-corrected coefficient covariance matrix ("hc0") (for an unweighted model) is 
# V(b) = inv(X'X) X' diag(e^2) X inv(X'X) where e^2 are the squared residuals, and X is the model matrix. 
library(car)
hccm(ols,type='hc0')
robust  # they are the same!



# Method 2: Alternative way to get the score using numericGradient() 

# Step 1. Write a function that calculates the log-likelihood of each individual data point
#         within your data. Your function should take beta, X, y, and sigma.
# Step 2. Use this function and numericGradient() to calculate the gradient of the loglikelihood
#         evaluated at each point in your dataset and at the MLE for beta and sigma.
# Step 3. Use the score vector and the variance-covariance matrix from your lm output 
#         (you can use the function vcov) to calculate the robust variance-covariance
#         matrix and robust standard errors.
# Step 4. Check your robust standard errors with the hccm function using option hc0.

normal.ll <-function(par,outcome,covariates){
  #check to see if the first column is all 1's
  #If it isn't add the 1's for the intercept term
  if(!all(covariates[,1]==1)){
    covariates<-as.matrix(cbind(1,covariates))
  }
  beta <-par[-length(par)] # parameter for the mean
  gamma <- log(par[length(par)]) # repameterize for theta
  
  xb<-covariates%*%beta
  (-1/2)*sum(gamma+(outcome-xb)^2/exp(gamma))
}

design.matrix <-cbind(1,dat4$X1,dat4$X2)

# estimate the MLE:
MLE <- optim(par=rep(1,ncol(design.matrix)+1),
             fn=normal.ll,
             covariates=design.matrix,
             outcome=dat4$y,
             control = list(fnscale=-1),
             hessian=T,
             method = "BFGS")

# MLE coefficient estimates are:
coefs <- MLE$par 
coefs # same as lm()

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
varcov <- solve(obs_fisher_info) 
se<-sqrt(diag(varcov))[1:3]
se # same as lm()

library(maxLik)
score  <- numericGradient(f=normal.ll,outcome=dat4$y,covariates=design.matrix,  t0=coefs)
score.vc<-matrix(score,nrow=4,ncol=1)
# bread = vairance-covariance matrix
bread<-vcov(ols)
# meat=dot product of the score
meat<-score.vc%*%t(score.vc)
meat<-meat[1:3,1:3] 

# sandwich estimator
robust <-bread%*%meat%*%bread
robustse<-sqrt(diag(robust))

# put estimate and robust standard error back in table
library(lmtest)
coeftest(ols, robust) # compare with classical standard error: coeftest(ols)


#### 4.C: Report the calculated robust standard errors.
# put estimate and robust standard error back in table
library(lmtest)
coeftest(ols, robust)  



#### 4.D: Are your robust and regular standard errors different from one another?
# compare with classical standard error: coeftest(ols)
coeftest(ols) 

# Yes, they are different.


#### 4.E: Use residual plots to diagnose the problem with the model. 
####      Create a couple residual plots you think are interesting.
res <- resid(ols)
dat4<-data.frame(y,X1,X2,res)

# QQ-plot for residual
qqnorm(res);qqline(res)

# plot residual against X1
dat4%>%ggplot(aes(x=X1,y=res))+geom_point()+geom_hline(yintercept=0,col="red")+ggtitle("OLS residual against X1")+xlab("X1")+ylab("residual")

# plot residual against X2
dat4%>%ggplot(aes(x=X2,y=res))+geom_point()+geom_hline(yintercept=0,col="red")+ggtitle("OLS residual against X2")+xlab("X1")+ylab("residual")


#### 4.F: Alter the model to fix the misspecification. What new specification did you decide to use and why?
# From QQ-plot, we can see the residuals are not close to normal; from plot residual against X1 and X2, respectively, 
# we can see that the residuals clearly not randomly fluctuate around 0, and both X1 and X2 have extra explanatory power
# for the residual, and trends are quadratic for both X1 and X2. Hence, I would alter the model to include extra 
# quadratic term for both X1 and X2.
dat4<-data.frame(y,X1,X2,X1_2=X1^2, X2_2=X2^2)
fmla <- as.formula(y~ X1+X1_2+X2+X2_2)
ols  <- lm(formula=fmla, data=dat4)
summary(ols)


#### 4.G: Calculate the robust and regular standard errors for your newly specified model. 
####      Do they match now? Why or why not?
library(sandwich)
bread<-vcov(ols)
est.fun <-estfun(ols)
meat<-crossprod(est.fun)/nrow(dat4)
robust <-sandwich(ols, meat=meat)

# put estimate and robust standard error back in table
library(lmtest)
coeftest(ols, robust) 
# compare with classical standard error: 
coeftest(ols)

# Yes, the robust and regular standard errors match.
# Why? Because with the new model, Y conditional on all covariates, the residual is independent normally distributed,
# and have homosdakesiticy variance. We can see from the residual plots. So the assumptions for Ordinary Linear Regression hold, 
# and without model misspecification, the classic and robust standard error should be close to each other.

# Residual Diagnotic:
res <- resid(ols)
dat4<-data.frame(y,X1,X2,res)

# QQ-plot for residual
qqnorm(res);qqline(res)

# plot residual against X1
dat4%>%ggplot(aes(x=X1,y=res))+geom_point()+geom_hline(yintercept=0,col="red")+ggtitle("OLS residual against X1")+xlab("X1")+ylab("residual")

# plot residual against X2
dat4%>%ggplot(aes(x=X2,y=res))+geom_point()+geom_hline(yintercept=0,col="red")+ggtitle("OLS residual against X2")+xlab("X1")+ylab("residual")













