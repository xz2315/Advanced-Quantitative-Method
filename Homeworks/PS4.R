##### Problem Set 4
##### Xiner Zhou
##### 3/1/2016


# Problem 1: Maximizing a Poisson Likelihood

#### 1.H
nr <-function(lambda,y){
  new_y <- 2*lambda-2*(lambda^2)/sum(y)
  return(new_y)
}

#### 1.I
# Create observed data
y <- c(2,4)
# initialize place-holder for diff btw lambda_n and lambda_n+1
diff <-1
# initialize lambda
init.lambda <-1
while(diff>0.00001){#set to arbitrarily small gap
  # update lambda
  lambda_next <-nr(init.lambda,y)
  print(lambda_next)
  diff <-abs(lambda_next-init.lambda)
  # update lambda for next iteration if any
  init.lambda<-lambda_next
}

#### 1.J
#log-likehood function, drop constant term
poisson_ll <-function(par,y){
  sum(y)*log(par)-2*par
}
# optimiza
opt.poisson <-optim(par=1,fn=poisson_ll,y=y,method="BFGS",control=list(fnscale=-1),hessian=TRUE)
opt.poisson


# Problem 2: Maximizing a Normal Likelihood

#### 2E: 
norm_ll <-function(par,y){
  -(1/2)*sum((y-par)^2)
}

#### 2F:
# create observed data
y<-c(-1,0,1)
# optimize
opt.norm <-optim(par=1,fn=norm_ll,y=y,method="BFGS",control=list(fnscale=-1),hessian=TRUE)
opt.norm


# Problem 3: 

#### 3B:
lm_ll<-function(par,y,x){
  # par include both beta's and sigma^2 as last element
  -length(y)*log(sqrt(exp(par[length(par)])))-sum((y-x%*%par[-length(par)])^2)/(2*exp(par[length(par)]))
}

#### 3C:
# import data
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 4")
load("presidential.Rdata")

# optimize
opt.lm<-optim(par=rep(0,21),fn=lm_ll,y=data[,1],x=cbind(1,data[,2:20]),method="BFGS",control=list(fnscale=-1),hessian=TRUE)
 
# MLE for intercept:
opt.lm$par[1]
# MLE for n1:
opt.lm$par[3]
# MLE for n2:
opt.lm$par[4]
# MLE for s2:
opt.lm$par[8]
# MLE for r3:
opt.lm$par[17]

# check with lm()
check <-lm(data[,1]~data[,2:20])
check$coefficients
 
#### 3D: standard errors
# Get hessian 
H <-opt.lm$hessian 
# calculate the observed fisher informaiton
I <- -H
# calculate the variance-covariance matrix
V <-solve(I)
# get the standard errors
se <-sqrt(diag(V))
# SD for intercept:
se[1]
# SD for n1:
se[3]


# Problem 4

#### 4B: Likelihood Ratio Test
# restricted model
restricted <-optim(par=rep(0,15),fn=lm_ll,y=data[,1],x=cbind(1,data[,2:14]),method="BFGS",control=list(fnscale=-1),hessian=TRUE)
# unrestricted model
unrestricted <-optim(par=rep(0,21),fn=lm_ll,y=data[,1],x=cbind(1,data[,2:20]),method="BFGS",control=list(fnscale=-1),hessian=TRUE)
# calculate LR stat
LR <-2*(unrestricted$value-restricted$value)
# report likelihood ratio test statistics
cat("Our likelihood ratio test statistics is:",LR)

#### 4C: P-value
# calculate p-value, that is, thebprobability of seeing likelihood ratio test stat more extreme or as extreme as this observed one
pchisq(LR,df=6,lower.tail=F)
# Reject the null, I don't accept the referee's suggestion of getting rid of the regional variables