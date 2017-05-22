##### Problem Set 2
##### Xiner Zhou
##### 2/7/2016

######################################################
## Problem 1 
######################################################

#### 1.A: Equation for Expectation and Variance

#### 1.B: Find Expectation and Variance using "integrate()", without simulation

# create a function that takes arguments x and lamda, then gives PDF of exponential distribution
exp_PDF <- function(x,lamda){
   lamda*exp(-lamda*x) 
}
# integrate to check if prob =1
integrate( exp_PDF,lower=0,upper=1000,lamda=2)

# evaluate Expectation when lamda=2 
exp_Exp <- function(x,lamda){
  x*exp_PDF(x,lamda)
}
mu <-integrate(exp_Exp,0,1000,lamda=2)
print(mu) # Answer:0.5 with absolute error < 2.4e-08

# evaluate Variance when lamda=2
exp_Var <- function(x,lamda){
  ((x-mu$value)^2)*exp_PDF(x,lamda)
}
integrate(exp_Var,0,1000,lamda=2) #Answer: 0.25 with absolute error < 7e-07

#### 1.C: Find Expectation and Variance via simulation

# set random seed
set.seed(02138)
# initialize number of iteration
nIter <-10000000
# initialize place-holder for random sampling
simulated_data <-  rep(NA,nIter)
for (i in 1:nIter){
  simulated_data[i]<-rexp(n=1,rate=2)
}
# print expectation and var
mean(simulated_data) # Answer: 0.5000815
var(simulated_data)  # Answer: 0.2501155

#### 1.D: Find Expectatio and Variance for Y via simulation
simulated_Y <- log(simulated_data)/(simulated_data^2-exp(simulated_data))
mean(simulated_Y) # Answer: 1.146299
var(simulated_Y)  # Answer: 1.497647


######################################################
## Problem 2 
######################################################

set.seed(02138)
gender <- c(rep(0,500),rep(1,500))
nIter <-1000
simulated_data <- rep(NA,nIter)
for(i in 1:nIter){
  error <- rnorm(n=1,mean=0,sd=3)
  simulated_data[i] <- 2+36*gender[i]+error
}
 
#### 2.A: Plot a histogram of simulated data and attach the plot as a PDF or image file
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 2")
  pdf("Problem 2A.pdf")
  hist(simulated_data, breaks=seq(floor(min(simulated_data)),ceiling(max(simulated_data)),1),
       xlab="Y", main="Simulated Data", col="red")
dev.off()

#### 2.B: Describe the distribution of your simulated data
# The simulated data has two clusters (women and men), each cluster approximately normally distributed with 
# similar dispersion but different cetral tendencies. Women's cluster centers around
# 38, while men cluster centers around 2.

#### 2.C: Does your data violate an assumption of the traditional OLS regression model?
# No, the data doesn't violate traditional OLS regression assumptions. 
# According to the data generation process, given gender, the dependent variable Y's are
# independently randomly drawn from the same or identical normal distribution with sd=3.
# In other words, the errors(stochastic component) are all independently identical distributed as normal  
# with mean 0 and standard deviation 3. 


######################################################
## Problem 3
######################################################

set.seed(02138)
employment <- c(rep(0,500),rep(1,500))
nIter <-1000 
simulated_data <- rep(NA,nIter)
for(i in 1:nIter){
  sigma  <- sqrt(2+36*employment[i])
  simulated_data[i] <- rnorm(n=1,mean=18,sd=sigma)
}

#### 3.A: Create a split histogram plot

# put employment and simulated_data into single data frame
mydata <- data.frame(employment, simulated_data)
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 2")
pdf("Problem 3A.pdf")
par(mfrow=c(1,2))
hist(mydata$simulated_data[mydata$employment==0],
     breaks=seq(floor(min(mydata$simulated_data)),ceiling(max(mydata$simulated_data)),1),
     xlab="Y", main="Simulated Data for unemployed", col="red")
hist(mydata$simulated_data[mydata$employment==1],
     breaks=seq(floor(min(mydata$simulated_data)),ceiling(max(mydata$simulated_data)),1),
     xlab="Y", main="Simulated Data for employed", col="green")
dev.off()
 
#### 3.B: What is the mean of the simulated data for employed observations?
mean(mydata$simulated_data[mydata$employment==1]) # Answer:17.34849

#### 3.C: What is the mean of the simulated data for unemployed observations?
mean(mydata$simulated_data[mydata$employment==0]) # Answer: 17.90827

#### 3.D: Explain why this model is useful and what this model tells us about the distribution of Y?

# This model is particularly useful when the data generation process violate the homosdascity 
# assumption of Ordinary Least Square regression model. Especaially when the variability of error term,
# or outcome Y, depend upon the value of a fixed variable. 

# This model tells us that, the outcome Y is independently randomely drawn from normal distributions 
# with same mean but different variances. For employed, Y is i.i.d. from normal(mean=18,var=38); while for
# unemployed, Y is i.i.d. from normal(mean=18,var=2). In other words, variability within employed group is 
# far larger than unemployed.


######################################################
## Problem 4
######################################################

#### 4.A:Write a function for PDF
PDF <- function(y,mu,sigma){
  exp(-sqrt(2)*abs(y-mu)/sigma)/(sigma*sqrt(2))
}

#### 4.B: Plot pdf
expr <- function(x){PDF(y=x,mu=0,sigma=2)}

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 2")
pdf("Problem 4B.pdf")
  curve(expr,from=-100,to=100,ylab="pdf of Y",main="pdf of Y with mu=0 and sigma=2",lwd=3)
dev.off()

#### 4.C: Integrate pdf over the support of Y 
integrate(PDF,lower=-1000,upper=1000,mu=0,sigma=2)

# 1 with absolute error < 1e-06
# The funciton integrates to 1, along with the fact that it greater than 0 over the support of Y,
# tells us that it is a valid Probability Density Funciton (PDF) of random variable Y.

#### 4.D: Find P(-2<Y<1.75) if mu=0 and sigma=2
integrate(PDF,lower=-2,upper=1.75,mu=0,sigma=2)
# 0.7333737 with absolute error < 2.4e-05



