##### Problem Set 7
##### Xiner Zhou
##### 4/9/2016


# Problem 1: Estimating an ATE

setwd("E:/Files/Advanced Quantitative Method-Gary King/Week 9")
#install.packages("causalsens", lib='E:/Files/R lib')
library('causalsens', lib='E:/Files/R lib')
data(lalonde.exp)
data(lalonde.psid)

#### 1.A Use a simple difference-in-means estimator to estimate the average treatment effect in the experimental dataset.
####     Form a 95% confidence interval for the average treatment effect.

fit<-lm(re78~ factor(treat), data=lalonde.exp)
summary(fit)$coefficients 
# ATE =1794.343

#### Large-sample 95% confidnece interval

# Check Standard Deviations Equal or Unequal

y.trt<-lalonde.exp$re78[which(lalonde.exp$treat==1)]
y.control<-lalonde.exp$re78[which(lalonde.exp$treat==0)]

sd(y.trt) 
sd(y.control) # more likely unequal
#S.pooled <-sqrt(((length(y.trt)-1)*var(y.trt)+(length(y.control)-1)*var(y.control))/(length(y.trt)+length(y.control)-2))
S <-sqrt(sd(y.trt)^2/length(y.trt)+sd(y.control)^2/length(y.control))
    
# lower
1794.343-qnorm(0.975,mean=0,sd=1)*S 
# upper
1794.343+qnorm(0.975,mean=0,sd=1)*S 
 


#### 1.B Now apply the same estimator to the observational dataset.
####     Compute the point estimate and large-sample 95% confidence interval.

fit<-lm(re78~ factor(treat), data=lalonde.psid)
summary(fit)$coefficients 
# ATE = -15204.78
 
#### Large-sample 95% confidnece interval

# Check Standard Deviations Equal or Unequal

y.trt<-lalonde.psid$re78[which(lalonde.psid$treat==1)]
y.control<-lalonde.psid$re78[which(lalonde.psid$treat==0)]

sd(y.trt)
sd(y.control) # more likely unequal
#S.pooled <-sqrt(((length(y.trt)-1)*var(y.trt)+(length(y.control)-1)*var(y.control))/(length(y.trt)+length(y.control)-2))
S <-sqrt(sd(y.trt)^2/length(y.trt)+sd(y.control)^2/length(y.control))

# lower
-15204.78-qnorm(0.975,mean=0,sd=1)*S 
# upper
-15204.78+qnorm(0.975,mean=0,sd=1)*S 


#### 1.C How does your estimated treatment effect differ between 1A and 1B 
####     Why do you think this is the case?

# From experimental dataset, the average treatment effect is 1794.343 which is significantly positive;
# From observational dataset, the average treatment effect is -15204.78 which is significantly negative.
# The two estimated treatment effects by simply difference-in-means approach yield both quantitatively 
# and qualitatively opposite effect.

# I think the reason we have this discrepancy is confounding issue in observational dataset.  
# Job training program is more likely to be assigned to those who didn't have a job prior to 
# the job training program, if a person had a job at that moment why they need job training. Therefore, 
# they are more likely to be less educated and had less income in past years (1974 and 1975). And people with
# higher education and higher past income history are more likely to have higher income at measuring year(1978).
# Thus, the difference-in-means approach estimate is mostly confounded by factors like education and income history.

#### 1.D In order for the difference-in-means estimator to be unbiased for the true causal effect,
####     what is the key assumption that must be made about the relationship between treatment T and
####     the potential outcomes Y(1) and Y(0)? For which of the two datasets is this assumption more likely to be held and Why?

# Two key assumptions:
# Assumption 1: Stable Unit Treatment Value Assumption (SUTVA)
# If a subject i receives treatment Ti=ti, then the observed outcome Yi is equal to the potential outcome Yi(t).
# Assumption 2: Ignorability/Unconfoundedness
# Treatment-Control assignment is independent of potential outcomes, that is, {Yi(1),Yi(0)} is independent of Ti.

# The two key assumptions are more likely to be held for the experimental dataset.
# Because Ignorability/Unconfoundedness is clearly violated in the observational dataset, while it's most like true 
# in the experimental dataset. By design, the treatment group are more likely to have lower education and income 
# history (and other confounders) than control group, in observational dataset; while in experimental datset, individuals
# are randomly assigned to job training program, thus any background characteristics are not causally related to their
# group assignment and earnings at 1978.

#### 1.E A common way of adjusting for observed confounders when estimating causal effect in observational data is
####   to fit a regression model for outcome with treatment and your confounding covariates as regressors.
####   Run a linear regression of earnings in 1978 on treatment and 10 covariates using the observational dataset.
####   Assume that there are no interactions or polynomial terms in the model and that all of the covariates enter into
####   the model additively. What is your estimate of the average treatment effect using this regression model (the coefficient on treatment)?

fit<-lm(re78~ factor(treat)+age+education+factor(black)+
          factor(hispanic)+factor(married)+factor(nodegree)+re74+re75+factor(u74)+factor(u75), data=lalonde.psid)
summary(fit)$coefficients 
# ATE = 4.1594601

####   How does this estimated treatment effect compare to your experimental benchmark in 1A?

# It has same positive effect as the benchmark, but smaller size which is non-significant. 
# Regression-adjusted treatment effect is 4.16(p-value=0.997) compared to experimental treatment effect which is 1794.343(p-value=0.0048).




# Problem 2: Assessing Balance

#### 2.A: For each 10 covariates, calculate the absolute standardized difference in means (that is, the difference of means 
####     in two groups, divided by the sample standard deviation assuming homogeneity) between treated and control observations using the observational dataset.
####     What is the value of the largest absolute standardized difference? For which variable is it the largest?

apply(lalonde.psid[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(lalonde.psid$treat==1)])-mean(x[which(lalonde.psid$treat==0)]))/sd(x))
# u74 (unemployment status) in 1974 has the largest absolute standardized difference, which is 1.8524414.

#### 2.B Now repeat 2A, use the experimental dataset. 
####    What is the value of the largest abolute standardized difference? For which variable is it the largest?

apply(lalonde.exp[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(lalonde.exp$treat==1)])-mean(x[which(lalonde.exp$treat==0)]))/sd(x))
# nodegree (no high school degree status) has the largest absolute standardized difference, which is 0.306063477.

#### 2.C Visualize Imbalance, make a plot comparing the level of imbalance in the observational dataset to the experimental one.
obs<-apply(lalonde.psid[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(lalonde.psid$treat==1)])-mean(x[which(lalonde.psid$treat==0)]))/sd(x))
exp<-apply(lalonde.exp[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(lalonde.exp$treat==1)])-mean(x[which(lalonde.exp$treat==0)]))/sd(x))
dat<-data.frame(data=c(rep("Observational",10),rep("Experiemntal",10)),
                var=rep(c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75"),2),
                diff=c(obs,exp))

dat1<-data.frame(obs,exp)

#install.packages("ggplot2", lib='E:/Files/R lib')
library('ggplot2', lib='E:/Files/R lib')
#install.packages("labeling", lib='E:/Files/R lib')
library('labeling', lib='E:/Files/R lib')
#install.packages("digest", lib='E:/Files/R lib')
library('digest', lib='E:/Files/R lib')
#install.packages("reshape2", lib='E:/Files/R lib')
library('reshape2', lib='E:/Files/R lib')



ggplot(dat )+
  geom_point(aes(x=data, y=diff, col=factor(var)))+
  geom_segment(aes(x = "Experiemntal", y = exp, xend = "Observational", 
                   yend = obs, colour =rownames(dat1)), data = dat1)+
  theme(legend.title=element_blank(),legend.position='bottom',text = element_text(size=20))+
  ggtitle("Imbalance Experimental vs Observational")+
  ylab("Absolute Standardized Diff in Means")


# Problem 3: Propensity Score Matching

#### 3.A Estimate the propensity score for each observstion in the observational dataset using logistic regression model on all ten covariates.
####     Use the model to predict the propensity of treated for each observation.
####     Plot two histograms of the estimated propensity scores, one for the treated units and one for control units.

fit<-glm(treat~age+education+black+hispanic+married+
           nodegree+re74+re75+u74+u75, data=lalonde.psid, family=binomial)
y_hat<-predict(fit,type='response')

dat2 <-data.frame(treat=factor(lalonde.psid$treat), y_hat, age=lalonde.psid$age, education=lalonde.psid$education,
                  black=lalonde.psid$black, hispanic=lalonde.psid$hispanic, married=lalonde.psid$married, nodegree=lalonde.psid$nodegree,
                  re74=lalonde.psid$re74, re75=lalonde.psid$re75, re78=lalonde.psid$re78, u74=lalonde.psid$u74, u75=lalonde.psid$u75)

# facet plot
ggplot(data=dat2,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  facet_grid(treat~.,scales='free_y')+
  theme(legend.position='bottom',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution in Observational Data:Treated vs Control")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 

# separate plot
trt <-dat2[which(dat2$treat==1),]
control <-dat2[which(dat2$treat==0),]

ggplot(data=trt,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  theme(legend.position='none',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution in Observational Data:Treated")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 

ggplot(data=control,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  theme(legend.position='none',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution in Observational Data:Control")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 





#### 3.B Compare the two plots from 3A. How well do the propensity scores seem to predict whether the unit is 
####     in the treatment or control group? What does that tell you about the data?

# R^2=1-(Residual Deviance/Null Deviance)
# in object summary(fit), deviance is residual deviance, null.deviance is model deviance
cat("R-Sqaure= \n", 1-summary(fit)$deviance/summary(fit)$null.deviance)

# The R-square is 0.69 implies that the propensity score seems to predict whether the unit is in the treatment or control group quite well, 69% of variation is explain by the 10 covariates. 
# The propensity of being treated for control group has an extremely right skewed distribution, mostly around 0% probability of being selected to treatment;
# While the propensity of being treated for treatment group has the opposite extreme left skewed distribution, mostly around 90%-100% probability of being 
# selected to treatment. This tells us that, for each observation in the control group, there is almost none treatment observation 
# in the neighborhood in terms of 10 covariates,in other words, there is almost no similar/comparable observation in the treatment group; and vice versa.
# Therefore, the treatment-control assignment is not even close to randomization, where the likelihood of being treated for similar/comparable observation should be
# around 0.5, if we make a plot, the two histograms should have two spikes at 0,5. Instead, what the propensity score reveals is that, the treatmend and control
# groups have very different distribution in terms of the 10 covariates, and the two groups are very far apart.


#### 3.C Now we're going to use matching to try to improve balance on the covariates. 
####     Start by prunning the data so as to drop all control units with propensity socres that lies outside of the 
####     support of the propensity scores for treated units. How many units did you drop?

# find the support
lower<-min(dat2$y_hat[dat2$treat==1]) 
upper<-max(dat2$y_hat[dat2$treat==1])

dat3C <-dat2[(dat2$y_hat>=lower & dat2$y_hat<=upper & dat2$treat==0) | dat2$treat==1,]
cat("How many units did you drop? \n",table(dat2$treat)[1]-table(dat3C$treat)[1])
#1282

#### 3.D Now we're going to try to estimate the average treatment effect on treated units(ATT) by searching for appropriate counterfactuals 
####     to use for each treated observation. We'll start by doing one-to-one matching with replacement. Again, we are using observational dataset.

# Start by subsetting out all of the treated observations
trt <- dat3C[dat3C$treat==1,] # treated units with PS and all covariates
control<-dat3C[dat3C$treat==0,] # control units with PS and all covariates

# Then, for each treated observation, search through the set of all controls(that you didn't prune) to find the control observation with a 
# propensity score closest to that of the treated unit (in terms of absolute distance).Store that control observation. Combine the treated
# and matched control observations to make your "matched" dataset.

matched.PS <-data.frame() # initialize place-holder for matched pairs
 
for(i in 1:nrow(trt)){
  
  temp<-rbind(as.matrix(trt$y_hat[i]), as.matrix(control$y_hat)) # append trt PS and all control PS as a matrix to calculate distance
  dist.vec<-as.matrix(dist(temp, method="euclidean"))[1,-1] # vector of distance for each control obs
  index<-which.min(dist.vec) #matched control row number in control table
  print(index) # check
  trt$match.pair <-i
  control$match.pair <-i # create a track number for each trt/control matched to
  matched.PS <- rbind(matched.PS , trt[i,], control[index,]) # append matched pairs together as a data frame
  cat("All treat units have been matched with a control unit? \n", ifelse(nrow(matched.PS )/2==nrow(trt), 'Yes','No'),'\n')
  
}

# How many individual rows (observations) are in your matched dataset? 
cat("How many individual rows (observations) are in your matched dataset? \n", nrow(matched.PS), '\n',
    "Number of treatment units= \n ", nrow(matched.PS[matched.PS$treat==1,]),'\n',
    "Number of control units= \n ", nrow(matched.PS[matched.PS$treat==0,]),'\n') 
#370=185+185

#### 3.E Create the same histogram as in 3A, but now using the matched dataset. Compare the distribution of propensity scores. 
####     What did matching do to the distribution?

# facet plot
ggplot(data=matched.PS,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  facet_grid(treat~.,scales='free_y')+
  theme(legend.position='bottom',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution After Matching in Observational Data:Treated vs Control")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 

# separate plot
trt <-matched.PS[which(matched.PS$treat==1),]
control <-matched.PS[which(matched.PS$treat==0),]

ggplot(data=trt,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  theme(legend.position='none',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution After Matching in Observational Data:Treated")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 

ggplot(data=control,aes(y_hat))+
  geom_histogram(aes(y = ..count../sum(..count..),col=treat,fill=treat))+
  theme(legend.position='none',text = element_text(size=20))+
  ggtitle("Propensity Score Distribution After Matching in Observational Data:Control")+
  ylab("Percentage %")+
  xlab("Estimated Propensity Score") 

# Matching makes the distribution of propensity scores in treated and control groups more similar to each other, that is, it makes the distribution of propensity scores balanced between treated and control.


#### 3.F using the same standardized difference-in-means in Problem 2, calculate the imbalance in the matched dataset for each of your ten covariates.
####     What is the largest diff-in-means that you calculated? For which variate is it the largest?

apply(matched.PS[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(matched.PS$treat==1)])-mean(x[which(matched.PS$treat==0)]))/sd(x))
# u75 (unemployment status) in 1975 has the largest absolute standardized difference, which is 0.4102613.
 

#### 3.G Calculate a diff-in-means estimate of the treatment effect using your matched dataset.
####    Compare the point estimate to your original estimates on the observational and experimetnal data from Problem 1.
####    What has matching done to your estimate? Did matching alleviate the problem of unobserved confounding?

fit<-lm(re78~ factor(treat), data=matched.PS)
summary(fit)$coefficients 
# ATE = 941.7924

# The experimental dataset has ATE =1794.343, the original obervational dataset has ATE= -15204.78, and matched dataset has ATE=941.7924. 
# Both experimental and matched dataset suggest positive effect of job training program on earnings, while observational dataset suggest negative effect.
# Matching makes the observational dataset more like the experimental dataset, in the sense that the treatment and control units are more comparable 
# or similar in background characteristics. But we only observe ten covariates, matching based on observables couldn't alleviate the problem of unobserved 
# confounding. The discrepancy between experimental ATE and matched ATE also suggests that there are still unobserved confoundings, otherwise they should be very close.
 

# Problem 4: Mahalanobis matching

#### 4.A: Write a function that takes two vectors and the S variance-covariance matrix and returns the Mahalanobis distance between
####      the two vectors. Use it to calculate the Mahalanobis distance bw obs 1 and 350 in the raw observational data on the 10 covariates.

M.dist.fun <-function(vec1, vec2, S){
  X <- cbind(as.matrix(vec1), as.matrix(vec2)) # convert vector into matrix as one row
  M.dist <-as.numeric(sqrt(t(X[,1]-X[,2]) %*% solve(S) %*% (X[,1]-X[,2])))
  return(M.dist)
}

v1<-as.vector(t(lalonde.psid[1,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")]))
v2<-as.vector(t(lalonde.psid[350,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")]))

# calculate sample variance-covaraince matrix S
X<-as.matrix(lalonde.psid[,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")])
# M distance
cat("Mahalanobis distance bw obs 1 and 350 in the raw observational data on the 10 covariates= \n",M.dist.fun(vec1=v1, vec2=v2, S=cov(X)))


#### 4.B We're going to again do one-to-one matching with replacement using the observational data, but instead of matching each using
####     propensity score, we're going to match using Mahalanobis distance between the covariates of treated and control units.

# Start by subsetting out all of treated observations
trt <- lalonde.psid[lalonde.psid$treat==1,] # treated units with PS and all covariates
control<-lalonde.psid[lalonde.psid$treat==0,] # control units with PS and all covariates

# Then,for each treated observation, calculate the Mahalanobis distance bw that unit's covariates and the covariates of all of the 
# control observations. Pick the control observation with the smallest Mahalanobis distance. Store that control observation. Combine
# the treated and matched control observations to make your "matched" dataset.

matched.M <-data.frame() # initialize place-holder for matched pairs

X<-as.matrix(lalonde.psid[,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")]) # calculate sample variance-covaraince matrix S

for(i in 1:nrow(trt)){
  
  dist.vec <-rep(NA, nrow(control)) # ititialize place-holder for distance
  for(j in 1:nrow(control)){
    v1<-as.vector(t(trt[i,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")]))
    v2<-as.vector(t(control[j,c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")]))
  
    dist.vec[j]<- M.dist.fun(vec1=v1, vec2=v2, S=cov(X))
  }
  index<-which.min(dist.vec) #matched control row number in control table
  print(index) # check
  
  trt$match.pair <-i
  control$match.pair <-i # create a track number for each trt/control matched to
  
  matched.M <- rbind(matched.M , trt[i,], control[index,]) # append matched pairs together as a data frame
  cat("All treat units have been matched with a control unit? \n", ifelse(nrow(matched.M )/2==nrow(trt), 'Yes','No'),'\n')
  
}


# How many individual rows (observations) are in your matched dataset? 
cat("How many individual rows (observations) are in your matched dataset? \n", nrow(matched.M), '\n',
    "Number of treatment units= \n ", nrow(matched.M[matched.M$treat==1,]),'\n',
    "Number of control units= \n ", nrow(matched.M[matched.M$treat==0,]),'\n') 
#370=185+185

# Using your matched dataset, calculate the diff-in-means estimate of the average treatment effect on the treated.
fit<-lm(re78~ factor(treat), data=matched.M)
summary(fit)$coefficients 
# ATE = 2023.073 


#### 4.C Finally,we're going to compare the balance on covariates between the propensity score matched dataset from Problem 3 and the Mahalanobis matched distance.

Mahalanobis<-apply(matched.M[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(matched.M$treat==1)])-mean(x[which(matched.M$treat==0)]))/sd(x))
PS<-apply(matched.PS[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(matched.PS$treat==1)])-mean(x[which(matched.PS$treat==0)]))/sd(x))
dat<-data.frame(data=c(rep("Mahalanobis Matching",10),rep("Propensity Score Matching",10)),
                var=rep(c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75"),2),
                diff=c(Mahalanobis,PS))

dat1<-data.frame(Mahalanobis,PS)

library('ggplot2', lib='E:/Files/R lib')
library('labeling', lib='E:/Files/R lib')
library('digest', lib='E:/Files/R lib')
library('reshape2', lib='E:/Files/R lib')

ggplot(dat )+
  geom_point(aes(x=data, y=diff, col=factor(var)))+
  geom_segment(aes(x = "Mahalanobis Matching", y = Mahalanobis, xend = "Propensity Score Matching", 
                   yend = PS, colour =rownames(dat1)), data = dat1)+
  theme(legend.title=element_blank(),legend.position='bottom',text = element_text(size=20))+
  ggtitle("Imbalance Mahalanobis vs Propensity Score Matching")+
  ylab("Absolute Standardized Diff in Means")

####     Calculate the average of the absolute standardized diff-in-means between treated/control for each of the covariates in your propensity score matched dataet.
####     Do the same thing for the matched Mahalanobis dataset. Based on your results, which matching metirc produced better covariate balance?
####     From what we learned in class, why do you think it worked better?
cat("Mhalanobis Matching:average of the absolute standardized diff-in-means between treated/control= \n", mean(Mahalanobis))
cat("PS Matching:average of the absolute standardized diff-in-means between treated/control= \n", mean(PS))

# Mahalanobis as the metric for matching produce better covariate balance. 

# Why: First, When using propensity score matching, we don't know the "true propensity score", so we use parametric models to estimate it, by doing it, 
# we actually make a lot of unverifiable assumptions about the logistic functional form, therefore we can't know if the propensity score we estimate 
# is close enough to the truth. Plus, our intention to use nonparametric matching is to avoid parametric modeling assumptions, however, PSM doesn't 
# avoid this unescapable loop, it estimates unknown based on unknown. PSM doesn't cure the curse of dimensionality.

# Second, PSM only uses 1-dimension information, the problem of reducing from Xs to single PS is "random pruning". Random pruning happens when the PS of 
# a treated unit and several control units are close, PSM randomly delete data independent of covariates Xs, thus random pruning actually increase imbalance.
# Another case is when PSM approximates complete randomization (to begin with, or after some pruning), it will prune at random, which increase imbalance,
# increase inefficiency (by pruning observations), and thus increase model dependence (by increase imbalance), and increase bias(by increase model dependence).



# Problem 5: Coarsened Exact Matching

#### 5.A use cem() to prune the observational dataset using the default settings.
####     How many treated and control observations does your matched dataset contain after you prune.

#install.packages("cem")
library(cem)
mat <- cem(treatment = "treat", data = lalonde.psid, drop = "re78")
mat

#### 5.B Estimate the treatment effect by using a diff-in-means estimate from yoru CEM-matched data using the att() funciton.
####     Report the point estimate of the treatment effect.

# ATE= -1829.242713
att(mat, re78~treat, data=lalonde.psid)

#### 5.C Let's assess the improvement in balance from the CEM match in 5A.
####     Take your matched dataset and calcualte the weighted absolute standardized difference in means across all of your ten covariates
####     (using the observation weights provided by cem()). Hint: use the weighted.mean() function to calcualte a weighted average.

# subset matched dataset by cem
matched.cem<-lalonde.psid[mat$matched,]
matched.cem$w <-mat$w[mat$matched]
dim(matched.cem)
 
apply(matched.cem[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(weighted.mean(x[which(matched.cem$treat==1)],matched.cem$w[which(matched.cem$treat==1)])-weighted.mean(x[which(matched.cem$treat==0)],matched.cem$w[which(matched.cem$treat==0)]))/sd(x))

# make balance plot compare three matching methods

Mahalanobis<-apply(matched.M[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(matched.M$treat==1)])-mean(x[which(matched.M$treat==0)]))/sd(x))
PS<-apply(matched.PS[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(mean(x[which(matched.PS$treat==1)])-mean(x[which(matched.PS$treat==0)]))/sd(x))
cem<-apply(matched.cem[c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75")],2,function(x) abs(weighted.mean(x[which(matched.cem$treat==1)],matched.cem$w[which(matched.cem$treat==1)])-weighted.mean(x[which(matched.cem$treat==0)],matched.cem$w[which(matched.cem$treat==0)]))/sd(x))

dat<-data.frame(data=c(rep("Mahalanobis Matching",10),rep("Propensity Score Matching",10),rep("Coarsen Exact Matching",10)),
                var=rep(c("age","education","black","hispanic","married","nodegree","re74","re75","u74","u75"),3),
                diff=c(Mahalanobis,PS,cem))

dat1<-data.frame(Mahalanobis,PS,cem)
 

ggplot(dat )+
  geom_point(aes(x=data, y=diff, col=factor(var)))+
  geom_segment(aes(x = "Mahalanobis Matching", y = Mahalanobis, xend = "Propensity Score Matching", 
                   yend = PS, colour =rownames(dat1)), data = dat1)+
  geom_segment(aes(x = "Coarsen Exact Matching", y = cem, xend = "Mahalanobis Matching", 
                   yend = Mahalanobis, colour =rownames(dat1)), data = dat1)+
  theme(legend.title=element_blank(),legend.position='bottom',text = element_text(size=20))+
  ggtitle("Imbalance Mahalanobis vs Propensity Score Matching vs Coarsen Exact Matching")+
  ylab("Absolute Standardized Diff in Means")

#### What is the average weighted absolute standardized diff-in-means? 
#### How does this compare to the propensity socre and mahalanobis distance matches?

cat("Mhalanobis Matching:average of the absolute standardized diff-in-means between treated/control= \n", mean(Mahalanobis))
cat("PS Matching:average of the absolute standardized diff-in-means between treated/control= \n", mean(PS))
cat("Coarsen Exact Matching:average of the absolute standardized diff-in-means between treated/control= \n", mean(cem))

# CEM has the smallest average weighted absolute standardized difference in means between treated and control, which is 0.06002835.
# If look at individual covariate, CEM did a better balance for all covariates except two: earnings at 1975 and earnings at 1974.

