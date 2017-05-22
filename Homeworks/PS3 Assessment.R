##### Problem Set 3 Assessment
##### Xiner Zhou
##### 2/15/2016
 
# Problem 1  Likelihood Inference
 
#### 1.A What is the likelihood of theta given y and n?
####     proportional to Binomial(n,theta)

#### 1.B 
likelihood <-function(theta){
  # Binomial with n=12 y=9
  return(choose(12,9)*theta^9*(1-theta)^3)
}

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
pdf("Assess Problem 1B.pdf")
curve(likelihood,from=0,to=1,ylab="Likelihood",xlab="theta",main="Likelihood Function with theta 0-1",lwd=2)
abline(v=0.75, lty=1, lwd=2, col="pink")
dev.off()

#### 1.C MLE of theta=0.75

#### 1.D 
dbinom(x=9, size=12, prob=0.75, log = FALSE) # 0.2581036

#  Bayesian Inference

#### 1.E p(theta|y,beta,n) propto (1-theta)^(beta-1)*choose(n,y)*theta^y*(1-theta)^(n-y)

#### 1.F 
posterior <-function(theta,beta,y,n){
  pdf <- ((1-theta)^(beta-1))*choose(n,y)*(theta^y)*((1-theta)^(n-y))
  return(pdf)
}
 
# How to define another function from an existing funciton:
# leave the parameter as "x", others filling with valeus 

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
pdf("Assess Problem 1F.pdf")
curve(posterior(x, beta=0.5,n=12,y=9),from=0,to=1,ylab="Density of Posterior",xlab="theta",lwd=2, lty=2,ylim=c(0,1),col="purple",main="Posterior with Theta 0-1")
abline(v=optimize(function(x) posterior(theta=x, beta=0.5,n=12,y=9), interval=c(0, 1), maximum=TRUE)$maximum[1], lty=2, lwd=1, col="green")
dev.off()

#### 1.G
integrate(function(x) posterior(theta=x, beta=0.5,n=12,y=9),lower=0.6,upper=0.65)$value/integrate(function(x) posterior(theta=x, beta=0.5,n=12,y=9),lower=0,upper=1)$value
#0.08904508