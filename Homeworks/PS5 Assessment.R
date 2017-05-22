##### Problem Set 5 Assessment
##### Xiner Zhou
##### 3/8/2016

 
#### 1.C

# log-likelihood funciton takes three arguments:
## par: 2 parameters
## outcome: the Y variable (link exists or not 1/0)
## covariates: the X matrix or design matrix (includes an intercept column and party difference indicator)
ll <-function(par,outcome,covariates){
  pi<-covariates%*%par
  sum(outcome*log(pi)+(1-outcome)*log(1-pi))
}

#### 1.D

# First set working directory
setwd("/Volumes/DISK_IMG/Files/Advanced Quantitative Method-Gary King/Week 5")
# Now read Stata file using 'foreign' packages
load("senate_network.RData")
 
# create the X matrix
design.matrix <- as.matrix(cbind(1-as.vector(party.diff[1:10]),as.vector(party.diff[1:10])))
# estimate the MLE:
MLE <- optim(par=rep(0.5,2),
             fn=ll,
             covariates=design.matrix,
             outcome=as.vector(links.vec),
             control = list(fnscale=-1),
             hessian=T,
             method = "Nelder-Mead")
 
# MLE coefficient estimates are:
coefs <- MLE$par
names(coefs)<-c("Same Party","Different Party")
# MLE for Ps and Pd are:
coefs

#### 1.E:

# MLE standard errors are:
## observed fisher information
obs_fisher_info <- -MLE$hessian
## variance-covariance matrix
vcov <- solve(obs_fisher_info)
se<-sqrt(diag(vcov))
names(se)<-c("Same Party","Different Party")
se

#### 1.F:

# restricted hypothesis correponding to no homophily effect is equivlent to the model for mean only has an intercept term
design.matrix <- as.matrix(cbind(1-as.vector(party.diff[1:10]),as.vector(party.diff[1:10])))
restricted <-optim(par=rep(0.5,1),
                   fn=ll,
                   covariates=matrix(rep(1,10),nrow=10,ncol=1),
                   outcome=as.vector(links.vec),
                   control = list(fnscale=-1),
                   hessian=T,
                   method = "Nelder-Mead")
# unrestricted model
unrestricted <-optim(par=rep(0.5,2),
                     fn=ll,
                     covariates=design.matrix,
                     outcome=as.vector(links.vec),
                     control = list(fnscale=-1),
                     hessian=T,
                     method = "Nelder-Mead")
# calculate LR stat
LR <-2*(unrestricted$value-restricted$value)
LR

# calculate p-value, that is, thebprobability of seeing likelihood ratio test stat more extreme or as extreme as this observed one
pchisq(LR,df=1,lower.tail=F)
# At 0.05 significance level, I Accept the null, and I conculde that there is no homophily effect 