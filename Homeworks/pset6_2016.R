########################
### GOV 2001
### Problem Set 6 Code
########################

########################
###### PROBLEM 1 #######
########################

########################
# 1.C

#install.packages("foreign")
library(foreign)
data <- read.dta("fearondata.dta")
fearon <- data[c("onset","warl","gdpenl","lpopl1","lmtnest","ncontig","Oil",
                "nwstate","instab","polity2l","ethfrac","relfrac")]
fearon <- na.omit(fearon)
fearon["onset"] <- replace(fearon["onset"],fearon["onset"]==4,1)

ll.logit <- function(beta, X, Y) {
  -sum(log(1 + exp((1-2*Y)*(X%*%beta))))
}
Y <- as.matrix(fearon[,1])
X <- cbind(1,as.matrix(fearon[,-1]))
opt <- optim(par = rep(0,12),
             Y = Y,
             X = X,
             fn = ll.logit,
             method = "BFGS",
             control = list(fnscale = -1, maxit=100000),
             hessian = TRUE)
opt.coefs <- opt$par
opt.ses <- sqrt(diag(solve(-opt$hessian)))

results <- matrix(c(opt.coefs, opt.ses), ncol = 2)
colnames(results) <- c("Coefficient", "Std. Error")
rownames(results) <- colnames(fearon)
rownames(results)[1] <- "Intercept"
require(xtable)
xtable(results)


# Check that we reach the same MLE as the glm() function
init <- coef(glm(onset ~ ., data=fearon, family=binomial("logit")))
ll.logit(opt$par, X, Y)
ll.logit(init, X, Y)
#-480.4017


###########################################
#1.D
civil.wars <- Y == 1
bl <- c(apply(X[civil.wars,], 2, median))
bl.table <- matrix(bl)
rownames(bl.table) <- colnames(X)
colnames(bl.table) <- "Median"
xtable(bl.table)



##################################################
#1.E
library(MASS)
set.seed(12345)

betas <- mvrnorm(n = 50000, mu = opt$par, Sigma = solve(-opt$hessian))
nonoil <- oil <- bl
oil["Oil"] <- 1
nonoil["Oil"] <- 0

first.diffs <- 1 / (1 + exp(-oil%*%t(betas))) - 1 / (1 + exp(-nonoil%*%t(betas)))

mean(first.diffs)
quantile(first.diffs, c(.025,.975))


plot(density(first.diffs))

# an equivalent approach for the logit model, unnecessary
# because E[Y] = pi
exps <- c()
for(i in 1:10000){
    oil.prob <- 1/(1+exp(-oil%*%betas[i,]))
    nonoil.prob <-  1/(1+exp(-nonoil%*%betas[i,]))
    preds <- rbinom(10000, 1, oil.prob) - rbinom(10000, 1, nonoil.prob)
    exps[i] <- mean(preds)
}
mean(exps)
quantile(exps, c(.025,.975))
         

#############################
#1.F

# plot expected predicted probability of civil war for new states
         # and old states, comparing across different levels of GDP
         # again fixing the values at median in civil war states

civil.wars <- Y == 1
bl <- c(apply(X[civil.wars,], 2, median))
bl["gdpenl"] <- NA
bl["nwstate"] <- NA
                                        # create a GDP vector
gdp.seq <- seq(.25,8,.25)
bl <- matrix(rep(bl), nrow = length(gdp.seq),ncol = length(bl), byrow = TRUE)
bl[,3] <- gdp.seq 

# bl covariates for old and new states
bl.old <- bl
bl.old[,8] <- 0
bl.new <- bl
bl.new[,8] <- 1

set.seed(12345)
betas <- mvrnorm(n = 10000, mu = opt$par, Sigma = solve(-opt$hessian))
                                        # note: because betas is 10000*12 we have to transpose?? it
                                        # to ensure conformability with bl.old/new
exp.holder <- matrix(data = NA, ncol = 6, nrow = length(gdp.seq))
for(i in 1:length(gdp.seq)){
  old.pr.sim <- 1/(1+exp(-bl.old[i,]%*%t(betas)))
  exp.holder[i,1] <- mean(old.pr.sim)
  exp.holder[i,2:3] <- quantile(x = old.pr.sim, probs = c(.025,.975))
  new.pr.sim <- 1/(1+exp(-bl.new[i,]%*%t(betas)))
  exp.holder[i,4] <- mean(new.pr.sim)
  exp.holder[i,5:6] <- quantile(new.pr.sim, c(.025,.975))
}


exp.holder[,1]/exp.holder[,4]


#pdf(file = "pset6_1f.pdf", width = 4, height = 4, family = "Helvetica", pointsize = 10)
plot(gdp.seq, 
     exp.holder[,4], 
     type = "l", 
     ylim = c(0,.25), 
     lwd = 2,
     col = "darkblue",
     xlab = "GDP per capita (thousands of dollars)",
     ylab = "Prob of Civil War Onset",
     cex = .1)
lines(gdp.seq, exp.holder[,1], col = "darkorange3", lwd = 2)
lines(gdp.seq, exp.holder[,2], lwd = 1.2, col = "darkorange3", lty = 2)
lines(gdp.seq, exp.holder[,3], lwd = 1.2, col = "darkorange3", lty = 2)
lines(gdp.seq, exp.holder[,5], col = "darkblue", lwd = 1.2, lty = 2)
lines(gdp.seq, exp.holder[,6], col = "darkblue", lwd = 1.2, lty = 2)
legend("topleft", legend = c("New state","Old state"),
              lwd = c(2,2), col = c("darkblue","darkorange3"),
              cex = .6)
#graphics.off()


########################
###### PROBLEM 2 #######
########################

########################
#2.C

data <- read.csv("revolutions.csv")
X <- cbind(1,as.matrix(data[,2:5]))
y <- as.matrix(data[,1])

ll.normal <- function(par,y,X){
  beta <- par[1:ncol(X)]
  sigma2 <- exp(par[ncol(X)+1])
  -1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2))
}

my.opt1 <- optim(par = rep(1, ncol(X) + 1),
                 fn = ll.normal,
                 y = y,
                 X = X,
                 control = list(fnscale = -1),
                 method = "BFGS",
                 hessian = TRUE)

res <- cbind(my.opt1$par, sqrt(diag(solve(-my.opt1$hessian))))
colnames(res) <- c("Coefficient","SE")
rownames(res) <- c("Intercept", "GenStrikes","GovCrises","Riots","Polity","sigma^2")
xtable(res)

########################
#2.D
setx <- apply(X, 2, mean)
setx[5] <- 10

set.seed(02138)
simpar <- mvrnorm(10000, my.opt1$par, -solve(my.opt1$hessian))

sims <- apply(simpar, 1, function (x) rnorm(1, setx%*%x[1:5], sqrt(exp(x[6]))))

pdf("pset6_2d.pdf")
hist(sims, main="Sims from Normal", breaks=20, col="dodgerblue2", border = "white")
graphics.off()

########################
#2.E
mean(sims>=1)



########################
###### PROBLEM 3 #######
########################

########################
#3.C
ll.poisson <- function(par, y, X){
  beta <- par[1:ncol(X)]
  xbeta <- X%*%beta
  sum(xbeta*y - exp(xbeta))
}

my.opt2 <- optim(par = rep(0, ncol(X)),
                 fn = ll.poisson,
                 y = y,
                 X = X,
                 control = list(fnscale = -1),
                 method = "BFGS",
                 hessian = TRUE)
my.opt2.coefs <- my.opt2$par
my.opt2.ses <- sqrt(diag(solve(-my.opt2$hessian)))

res <- cbind(my.opt2.coefs, my.opt2.ses)
colnames(res) <- c("Coefficient","SE")
rownames(res) <- c("Intercept", "GenStrikes","GovCrises","Riots","Polity")
xtable(res)


########################
#3.D

setx <- apply(X, 2, mean)
setx[5] <- 10

set.seed(02138)
simpar <- mvrnorm(10000, my.opt2$par, -solve(my.opt2$hessian))
sims <- apply(simpar, 1, function (x) rpois(1, exp(setx%*%x)))

pdf("pset6_3d.pdf")
hist(sims, main="Sims from Poisson", breaks=15, col="dodgerblue2", border = "white")
graphics.off()


########################
#3.E
mean(sims>=1)


########################
###### PROBLEM 4 #######
########################

########################
library(foreign)
library(mvtnorm)
library(sandwich)
library(lmtest)
library(MASS)
library(maxLik)
library(car)
require(stargazer)

set.seed(01234)
X1 <- rnorm(500, 3, 2)
X2 <- rnorm(500, 5, 2)
y <- rnorm(500, 3 + .3*X1 + .1*X2 + X1^2 - X2^2, sd=2)
#save(list = c("X1","X2","y"), file = "robust.RData")

########################
#4.A Run the regression

lm.1 <- lm(y ~ X1 + X2)

stargazer(lm.1,
          no.space = T,
          dep.var.labels.include = F,
          omit.stat = c("all"))

summary(lm.1)

##########################################
#4.B Calculate the robust standard errors

#First way

est.fun <- estfun(lm.1)
robust2 <- sandwich(lm.1, meat=crossprod(est.fun)/500)
robust1 <- hccm(lm.1, type="hc0")
sqrt(diag(robust1))

#Second way

ll.normal <- function(beta,y,X, sigma){
  sigma2 <- sigma
  -1/2 * (log(sigma2) + (y -(X%*%beta))^2/sigma2)
}

X <- cbind(1, X1, X2)

sigma <- sum(lm.1$residuals^2) / (nrow(model.matrix(lm.1)) - 
                                    ncol(model.matrix(lm.1)))
out <- apply(cbind(y,X), 1, 
             function(x) numericGradient(ll.normal, 
                                         lm.1$coefficients, 
                                         y=x[1], 
                                         X=x[2:length(x)], 
                                         sigma=sigma))

meat <- out%*%t(out)
bread <- vcov(lm.1)%*%meat%*%vcov(lm.1)
sqrt(diag(bread))

#Look at residual plots to identify the problem

pdf("pset5_2c.pdf")
par(mfrow = c(1,2))
scatter.smooth(X1, lm.1$residuals, pch = 18, col = "dodgerblue2", lpars = list(col = "mediumorchid", lwd = 3))
scatter.smooth(X2, lm.1$residuals, pch = 18, col = "dodgerblue2", lpars = list(col = "mediumorchid", lwd = 3))

graphics.off()

#2.D
#Analyze the correct way
X12 <- X1^2
X22 <- X2^2
X <- cbind(1, X1, X2, X12, X22)

lm.2 <- lm(y ~ X1 + X2 + I(X1^2) + I(X2^2))
summary(lm.2)

sigma <- sum(lm.2$residuals^2)/(nrow(model.matrix(lm.2))-ncol(model.matrix(lm.2)))


ll.normal <- function(beta,y,X, sigma){
  sigma2 <- sigma
  -1/2 * (log(sigma2) + (y -(X%*%beta))^2/sigma2)
}
out <- apply(cbind(y,X),1,function(x) numericGradient(ll.normal, 
                                                      lm.2$coefficients, 
                                                      y=x[1], 
                                                      X=x[2:length(x)], 
                                                      sigma=sigma))
meat <- out%*%t(out)
bread <- vcov(lm.2)%*%meat%*%vcov(lm.2)
sqrt(diag(bread)) #robust
sqrt(diag(vcov(lm.2))) #regular

ses <- cbind(sqrt(diag(vcov(lm.2))), sqrt(diag(bread)))
colnames(ses) <- c("Regular", "Robust")
xtable(ses)


#Look at the residual plots again
plot(lm.2)
plot(X1, lm.2$residuals)
plot(X2,lm.2$residuals)



