####################################################################
### GOV 2001 Assignment 2
### Spring 2016
### SOLUTIONS
####################################################################

####################################################################
## Problem 1
####################################################################

####################################################################
## 1B
####################################################################

# First, let's write a function for the expected value of X
  ex.func <- function(x, lambda){
    x * lambda * exp(-lambda * x)
  }
  
  e.x <- integrate(ex.func, lower = 0, upper = Inf, lambda = 2)$value
  e.x #0.5 or 1/lambda


# Now, let's write a function for E(X^2) and integrate that
  ex.sq.func <- function(x, lambda){
    x^2 * lambda * exp(-lambda * x)
  }

  e.x.sq <- integrate(ex.sq.func, lower = 0, upper = Inf, lambda = 2)$value
  e.x.sq

# So if Var(X) = E(X^2) - [E(X)]^2, we can just apply the formula to our answers
# for E(X^2) - [E(X)]^2:
  var.x <- e.x.sq - e.x^2
  var.x # 0.25 or 1/lambda^2

####################################################################
## 1C
####################################################################
# Finding the resuls from 1B by simulation:

  set.seed(02138)
  exp.draws <- rexp(10000, rate = 2) 
  mean(exp.draws) #0.505
  var(exp.draws) #0.2688

# Yes - these look really close to our values from integrate!
  
####################################################################
## 1D
####################################################################
# All we need to do to generate y is to apply a transformation
# to our exponential draws
  
  y.draws <- log(exp.draws) / (exp.draws^2 - exp(exp.draws))
  mean(y.draws) #1.147
  var(y.draws) #1.507



####################################################################
## Problem 2
####################################################################

####################################################################
## 2A
####################################################################
# First, we'll generate the data using the code we have
  x <- matrix(c(rep(0,500), rep(1,500)))
  means <- 2 + 36 * x # since our covariates go into calculating mu
  
  set.seed(01238)
  y.sim <- means + rnorm(1000, mean = 0, sd = sqrt(9)) # now we'll simulate from a normal with our means and given sigma squared

  hist(y.sim) # Looks bimodal, just was we'd expect

  # Export Plot 
  setwd("~/Dropbox/Teaching/Course Folders/Gov 2001/Gov2001/Spring2016/Problem Sets/Pset2")
  pdf("pset2_2a.pdf")
  hist(y.sim[1:500],
       xlim = c(min(y.sim),max(y.sim)),
       main = "Men                                   Women",
       xlab = "y",
       col = "blue")
  hist(y.sim[501:1000],
       col = "red",
       add = T)
  graphics.off()


####################################################################
## Problem 3
####################################################################
  
####################################################################
## 3A
####################################################################

# Again, let's use the given code to create our covariate data for employment/unemployment
  x <- c(rep(0,500),rep(1,500))
  
  # Now let's calculate sigma squared
  betas <- c(2, 36) # we're given our betas in the model
  covs <- as.matrix(cbind(1,x)) # we'll bind a 1 to our data since we have an intercept
  vars <- covs %*% betas # so each sigma squared will be our covariate values times our betas
  
  set.seed(02138)
  
  # We'll draw our simulated outcome data with a mean at mu
  y.sim <- rnorm(1000, mean = 18, sd = sqrt(vars))


  # Export plot
  pdf("pset2_3a.pdf")
  par(mfrow = c(1,2))
  hist(y.sim[1:500],
       xlim = c(min(y.sim),max(y.sim)),
       main = "Unemployed People",
       xlab = "y",
       col = "blue")
  hist(y.sim[501:1000],
       xlim = c(min(y.sim),max(y.sim)),
       main = "Employed People",
       xlab = "y",
       col = "red")
  graphics.off()

####################################################################
## 3B
####################################################################
# Calculate the mean for employed (the bottom half of our matrix in this case)  
  mean(y.sim[501:1000])

####################################################################
## 3C
####################################################################
# Calculate the mean for unemployed
  mean(y.sim[1:500])


####################################################################
## Problem 4
####################################################################

####################################################################
## 4A
####################################################################

  laplace <- function(y, mu, sigma){
    exp(-sqrt(2) * abs(y - mu) / sigma) / (sqrt(2) * sigma)
  }


####################################################################
## 4B
####################################################################

pdf("pset2_4b.pdf")
par(mfrow = c(1,1))
curve(laplace(x, mu = 0, sigma = 2),
      xlim = c(-5,5),
      main = "Our PDF")
graphics.off()

## OR

y <- seq(-1000,1000,.1)
pdf_y <- laplace(y, mu = 0, sigma = 2)
plot(y = pdf_y, x = y, type = "l", xlim = c(-10, 10))


####################################################################
## 4C
####################################################################

  integrate(laplace, -1000, 1000, mu = 0, sigma = 2)
  # it integrates to 1 so it's a valid PDF
  # also, doesn't really matter what values you pick for mu and sigma, as long as
  # your upper and lower bounds are big enough


####################################################################
## 4D
####################################################################

  integrate(laplace, -2, 1.75, mu = 0, sigma = 2) #0.7334

