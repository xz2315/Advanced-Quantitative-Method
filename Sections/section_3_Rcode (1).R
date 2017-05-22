##### Section 3 - R Examples

#### Read in wall punching dataset
require(plot3D)

#### Load wall_punching dataset
wall_punching <- read.csv("wall_punching_injuries.csv")

### Raw histogram of the data
pdf("hist_wallpunch_raw.pdf")
hist(wall_punching$age, breaks=seq(6, 80, by=1), xlim=c(0, 80), freq=F, main="Ages of ER patients who punched a wall in 2014", xlab="Age", ylab="Share", axes=F)
axis(side=2, at=seq(0, 0.1, by=.01))
axis(side=1, at=seq(0, 80, by=10))
dev.off()

### Likelihood function
likelihood.func <- function(mu, sigma, Y){
  # Return the product of dnorm evaluated for all Y with fixed mu and sigma
  return(prod(dlnorm(Y, meanlog=mu, sdlog=sigma)))
}

### Log-Likelihood function
log.likelihood.func <- function(mu, sigma, Y){
  # Return the sum of the log of dnorm evaluated for all Y with fixed mu and sigma
  return(sum(dlnorm(Y, meanlog=mu, sdlog=sigma, log=T)))
}

### Let's plot the likelihood for different values of mu and sigma
mu.test <- seq(log(10), log(50), length.out=100)
sigma.test <- seq(.3, 5, length.out=100)

log.lik.mat <- matrix(NA, nrow=100, ncol=100)
for (i in 1:100){
  for(j in 1:100){
    log.lik.mat[i,j] <- log.likelihood.func(mu=mu.test[i], sigma=sigma.test[j], Y=wall_punching$age)
  }
}


pdf("contour_plot.pdf")
image2D(x=mu.test, y=sigma.test, z=log.lik.mat, xlab="Mu", ylab="Sigma", contour=T, shade=.05)
dev.off()

pdf("3dplot_plot.pdf")
persp3D(x=mu.test, y=sigma.test, z=log.lik.mat, xlab="Mu", ylab="Sigma", zlab="Log-likelihood")
dev.off()


plot.lik <- sapply(seq(log(3),log(80),length.out=1000), function(x) log.likelihood.func(mu=x, sigma=2, Y=wall_punching$age))
pdf("cond_log_lik.pdf")
plot(x= seq(log(3),log(80),length.out=1000), y=plot.lik, type="l", lwd=2, main="Conditional log-likelihood varying mu, setting sigma=2", ylab="Log-likelihood", xlab="Mu")
dev.off()


#### Compare two likelihoods

### Evaluate the log-likelihood 
log.likelihood.func(mu=4, sigma=.2, Y=wall_punching$age)

pdf("hist_wallpunch_raw_model1.pdf")
hist(wall_punching$age, breaks=seq(6, 80, by=1), xlim=c(0, 80), freq=F, main="Ages of ER patients who punched a wall in 2014", xlab="Age", ylab="Share", axes=F)
axis(side=2, at=seq(0, 0.1, by=.01))
axis(side=1, at=seq(0, 80, by=10))
curve(dlnorm(x, mean=4, sd=.2) , lwd=3, lty=2, col="darkred", add=T)

dev.off()

### Raw histogram of the data
pdf("hist_wallpunch_raw_model2.pdf")
hist(wall_punching$age, breaks=seq(6, 80, by=1), xlim=c(0, 80), freq=F, main="Ages of ER patients who punched a wall in 2014", xlab="Age", ylab="Share", axes=F)
axis(side=2, at=seq(0, 0.1, by=.01))
axis(side=1, at=seq(0, 80, by=10))
curve(dlnorm(x, mean=3.0985497, sd=0.3791292) , lwd=3, lty=2, col="darkred", add=T)

dev.off()

##### Maximize the likelihood for fun
log.likelihood.opt <- function(pars, Y){
  mu <- exp(pars[1])
  sigma <- exp(pars[2])
  # Return the sum of the log of dnorm evaluated for all Y with fixed mu and sigma
  return(sum(dlnorm(Y, meanlog=mu, sdlog=sigma, log=T)))
}

optimization <- optim(c(3, .3), log.likelihood.opt, Y = wall_punching$age, control = list(fnscale = -1))
summary(optimization)
exp(optimization$par) # Paramter Values

#######################################
##### Plotting posterior densities
pdf("posterior_plot.pdf")
curve(dgamma(x, shape=3, rate=10), from=0, to=1, xlab="Lambda", col="Red", lwd=2, lty=2, ylab="Density", ylim=c(0,5))
curve(dgamma(x, shape=3+1, rate=10+7), from=0, to=1, col="Blue", lwd=3, lty=1, add=T)
abline(v=1/7, lty=2, lwd=2, col="Blue")
dev.off()

######################################
##### Type M and S errors:


pdf("low_power_design.pdf", width=9, height=7)
curve(dnorm(x, mean=.2 , sd= 4/sqrt(50)), from=-2, to=3, lwd=2, xlab="Effect Estimate", ylab="Density", main="Example of Low Power\nEffect = .2, Population Variance = 16\n N = 50")
abline(v=.2, lty=2, lwd=3)
abline(v=0, col="grey")
abline(h=0, col="grey")
x.cord.1 <- c(seq(from=-2, to= qnorm(.025, mean=0, sd=4/sqrt(50)), length.out=100))
y.cord.1 <- c(dnorm(x.cord.1, mean=.2, sd=4/sqrt(50)), 0)
polygon(c(x.cord.1, qnorm(.025, mean=0, sd=4/sqrt(50))), y.cord.1, col='grey')

x.cord.2 <- c(seq(from=qnorm(.975, mean=0, sd=4/sqrt(50)), to=5, length.out=100))
y.cord.2 <- c(dnorm(x.cord.2, mean=.2, sd=4/sqrt(50)), 0)
polygon(c(x.cord.2, qnorm(.975, mean=0, sd=4/sqrt(50))), y.cord.2, col='grey')

text(x=-1.5, y=.2, labels=c("Type 'S':\nReject and conclude\nwrong direction"), cex=1)
text(x=2.2, y=.2, labels=c("Type 'M':\nReject and conclude\neffect > 5x larger than truth"), cex=1)
dev.off()


prob_typeS <- pnorm(qnorm(.025, mean=0, sd=4/sqrt(50)), mean=.2, sd=4/sqrt(50))
prob_typeM <- 1 - pnorm(qnorm(.975, mean=0, sd=4/sqrt(50)), mean=.2, sd=4/sqrt(50))

powerZ = prob_typeS + prob_typeM


pdf("higher_power_design.pdf", width=9, height=7)
curve(dnorm(x, mean=.2 , sd= 4/sqrt(500)), from=-1, to=1, lwd=2, xlab="Effect Estimate", ylab="Density", main="Example of Moderate Power\nEffect = .2, Population Variance = 16\n N = 500")
abline(v=.2, lty=2, lwd=3)
abline(v=0, col="grey")
abline(h=0, col="grey")
x.cord.1 <- c(seq(from=-2, to= qnorm(.025, mean=0, sd=4/sqrt(500)), length.out=100))
y.cord.1 <- c(dnorm(x.cord.1, mean=.2, sd=4/sqrt(500)), 0)
polygon(c(x.cord.1, qnorm(.025, mean=0, sd=4/sqrt(500))), y.cord.1, col='grey')

x.cord.2 <- c(seq(from=qnorm(.975, mean=0, sd=4/sqrt(500)), to=5, length.out=100))
y.cord.2 <- c(dnorm(x.cord.2, mean=.2, sd=4/sqrt(500)), 0)
polygon(c(x.cord.2, qnorm(.975, mean=0, sd=4/sqrt(500))), y.cord.2, col='grey')

text(x=-.5, y=.5, labels=c("Type 'S':\nReject and conclude\nwrong direction"), cex=1)
dev.off()


prob_typeS <- pnorm(qnorm(.025, mean=0, sd=4/sqrt(500)), mean=.2, sd=4/sqrt(500))
prob_typeM <- 1 - pnorm(qnorm(.975, mean=0, sd=4/sqrt(500)), mean=.2, sd=4/sqrt(500))

powerZ = prob_typeS + prob_typeM
