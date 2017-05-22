##### Problem Set 3
##### Xiner Zhou
##### 2/15/2016


# Likelhood Inference

#### 1.A
dpois(x=6,lambda=5) #0.1462228

#### 1.B likelihood is proportional to the probability of observing y=6 given Poisson distribution with lambda

#### 1.C 
poi_likelihood <-function(lambda){
  # p(x) = ??^x exp(-??)/x!
  y<-6
  return((lambda^y)*exp(-lambda)/factorial(y))
}

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
pdf("Problem 1C.pdf")
curve(poi_likelihood,from=0,to=15,ylab="Likelihood",xlab="lambda",main="Likelihood Function with Lambda 0-15",lwd=3)
abline(v=6, lty=2, lwd=2, col="Blue")
dev.off()

#### 1.D When lambda=6 it roughly maximize the likelihood



# Bayesian Inference

#### 2.A
prior <-function(lambda){
  theta <-2
  k <-0.5
  pdf <-lambda^(k-1)*exp(-lambda/theta)/(gamma(k)*(theta^k))
  return(pdf)
}

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
pdf("Problem 2A.pdf")
curve(prior,from=0,to=15,ylab="Density of Prior",xlab="lambda",main="Density of Prior with Lambda 0-15",lwd=3)
dev.off()
 
#### 2.C
posterior <-function(lambda){
  theta <-2
  k <-0.5
  y<-6
  pdf <-lambda^(k+y-1)*exp(-lambda-lambda/theta)/(gamma(k)*(theta^k)*factorial(y))
  return(pdf)
}

# RE-scale posterior to be same scale as prior
post_max<-as.numeric(optimize(posterior, interval=c(0, 15), maximum=TRUE)$objective[1])
post_coplot <-function(lambda){
  theta <-2
  k <-0.5
  y<-6 
  pdf <-lambda^(k+y-1)*exp(-lambda-lambda/theta)/(gamma(k)*(theta^k)*factorial(y))/post_max 
  return(pdf)
}


setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
pdf("Problem 2C.pdf")
curve(prior,from=0,to=15,ylab="Density of Prior",xlab="lambda",lwd=2, lty=2,ylim=c(0,1),col="purple",main="Prior and Posterior with Lambda 0-15")
curve(post_coplot,from=0,to=15,ylab="Density of Posterior",xlab="lambda",main="Density of Prior & Posterior with Lambda 0-15",lwd=2, lty=1,add=T,col="pink")
abline(v=optimize(prior, interval=c(0, 15), maximum=TRUE)$maximum[1], lty=2, lwd=2, col="purple")
abline(v=optimize(posterior, interval=c(0, 15), maximum=TRUE)$maximum[1], lty=1, lwd=2, col="pink")
dev.off()

#### 2.D

#### Although the heights for prior density and poterior density are different, the shapes are invariant to re-scale by positive constants.
#### So the optimal values of lambda that achieve the max density can be obtained from the plot.
#### The lambda that achieves the maximum for prior is around 0, which might suggets that our prior belief about the average rate at which Representa-
#### tives return home, is about 0. After we observed one case that Mike Capuano made six trips to his home district in January, 
#### we have more evidence that the rate lambda should be larger, thus the posterior density shifts to right, and the lamda that achieves the 
#### maxium for posterior is around 3.5.

#### 2.E
integrate(posterior,lower=4,upper=6)$value/integrate(posterior,lower=0,upper=1000)$value
#Answer:0.3701244

# Bayes Theorem

#### 3.A What is the unconditional probability of having unusual looking election results?
#### p(unusual result)=p(unusual result|fraud)p(fraud)+p(unusual result|not fraud)p(not fraud)=0.472
0.3*0.99+0.7*0.25

#### 3.B What is the prior probability of fraud occurring at this precinct?
0.3

#### 3.C We know that the election returns from this new precinct look unusual. Given this,
####     what is the probability these unusual election returns were actually the result of fraud?
#### p(fraud|unusual result)=p(fraud,ususual result)/p(unusual result)=p(unusual result|fraud)*p(fraud)/p(unusual result)=0.6292373
0.99*0.3/0.472

#### 3.D
#### I would choose Extended beta-binomial distribution. The election results for the candidates in each precinct are the sum of a series of 
#### N Bernoulli random variables, with N indicating total number of voters and each one of them had a choice either vote for him/her or not.
#### If we assume all voters made their decisions indipendently and all have the same probability of voting for or not, then binomial 
#### distribution is adequate enough. But we can reasonably hypothesize that, the probability of each citizen voting for a candidate is not identical,
#### and people are likely to influence each other rather than totally independet, maybe people living in a region share similar 
#### political position, economic status, and religious belief. Thus, the binomial assumptions of independence and identical distributions
#### are dubious. Therefore, Extended beta-binomial distribution, with additional parameter which governs the degree to which voting probability 
#### varies across the unobserved binary variables making up each observation, is most appropriate. 



# Neyman-Pearson Hypothesis Testing

#### 4.A N(mu,9/n)

#### 4.B
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 3")
dat <-read.csv("p4_1.csv")

# estimated sample mean
mean(dat$X) #-1.418742

# z-test stat
sigma_2 <-9
n<-30
z <- (mean(dat$X)-0)/sqrt(sigma_2/n)
z #-2.590257

# p-value for two-sided z-test 
p <- 2*pnorm(z,mean=0,sd=1,lower.tail = T)
p #0.009590418

# I reject the null hypothesis at the significance level 0.05, because the probability under the null of getting a value as weird or 
# weirder than the value we got is extremely small - 0.00959.

#### 4.C
n<-30
wrongS <- pnorm(qnorm(0.025,mean=0,sd=sqrt(sigma_2/n),lower.tail = T) ,mean=0.1,sd=sqrt(sigma_2/n),lower.tail = T)
rightS <- pnorm(qnorm(0.025,mean=0,sd=sqrt(sigma_2/n),lower.tail = F) ,mean=0.1,sd=sqrt(sigma_2/n),lower.tail = F)
prob_typeS <-wrongS/(wrongS+rightS) #0.2986429

#### 4.D
n<-300
wrongS <- pnorm(qnorm(0.025,mean=0,sd=sqrt(sigma_2/n),lower.tail = T) ,mean=0.1,sd=sqrt(sigma_2/n),lower.tail = T)
rightS <- pnorm(qnorm(0.025,mean=0,sd=sqrt(sigma_2/n),lower.tail = F) ,mean=0.1,sd=sqrt(sigma_2/n),lower.tail = F)
prob_typeS <-wrongS/(wrongS+rightS) #0.06277275