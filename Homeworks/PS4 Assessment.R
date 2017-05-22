##### Problem Set 4 Assessment
##### Xiner Zhou
##### 3/1/2016

# import data
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 4")
load("presidential.Rdata")

#### 1C:
lm_ll<-function(par,y,x,z){
  # par include both beta's and gamma's
  beta <- par[1:20]
  gamma <-par[21:length(par)]
  (-1/2)*sum(z%*%gamma+(y-x%*%beta)^2/exp(z%*%gamma))
}

#### 1D:
# optimize
opt.lm<-optim(par=rep(0,23),fn=lm_ll,y=data[,1],x=cbind(1,data[,2:20]),z=cbind(1,data[,2],data[,15]),method="BFGS",control=list(fnscale=-1),hessian=TRUE)
# beta coefficient for year covariate
opt.lm$par[22]
# beta coefficient for r1 covariate
opt.lm$par[23]
 
#### 1E: standard erroe of gamma coefficient for r1
# Get hessian 
H <-opt.lm$hessian 
# calculate the observed fisher informaiton
I <- -H
# calculate the variance-covariance matrix
V <-solve(I)
# get the standard errors
se <-sqrt(diag(V))
# SD for year covariate
se[22]
# SD for r1 covariate
se[23]
