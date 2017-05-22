##########################################################
## Gov 2001
## Problem Set 2 Assessment Problem Solutions
## Spring 2016
##########################################################

setwd("~/Dropbox/Teaching/Course Folders/Gov 2001/Gov2001/Spring2016/Problem Sets/Pset2/assessment problem")

##########################################################
# A #
##########################################################

sims <- 10000
set.seed(12345)
pdf("assessment2_a.pdf")
hist(rbinom(n = sims, size = 99, prob = .5) + 1,
     xlab = "Number at Bar",
     ylab = "Frequency",
     main = "PMF of Binomial Distribution",
     col = "dodgerblue2",
     border = "white",
     bty = "l")
graphics.off()

##########################################################
# B #
##########################################################
## You can simulate the number at the bar all at once and calculate the probability from there

  set.seed(12345)
  number.at.bar <- rbinom(n = sims, size = 99, prob = .5) + 1
  mean(number.at.bar >= 10 & number.at.bar <= 60) #0.9762


## Or you can simulate Friday nights, one at a time

  friday.fun <- function(){
    # Simulate the number of people going to the bar and add one because you're definitely going
    going.to.bar <- rbinom(n = 1, size = 99, prob = .5) + 1
    # Check if the number going is within the right bounds
    going.to.bar >= 10 & going.to.bar <= 60
  }

  set.seed(12345)
  results <- c()
  for(i in 1:sims){
    results[i] <- friday.fun()
  }
  mean(results) #0.9762



## Or:

  set.seed(12345)
  mean(replicate(sims, friday.fun())) #0.9762

##########################################################
# D #
##########################################################
  friday.fun.2 <- function(population, capacity){
    number.at.bar <- rbinom(n = sims, size = population - 1, prob = .5) + 1
    mean(number.at.bar >= 10 & number.at.bar <= capacity)
  }


## Or

  friday.fun.3 <- function(population, capacity){
    # Simulate the number of people going to the bar and add one because you're definitely going
    going.to.bar <- rbinom(n = 1, size = population - 1, prob = .5) + 1
    # Check if the number going is within the right bounds
    going.to.bar >= 10 & going.to.bar <= capacity
  }
  
##########################################################
# E #
##########################################################

## Using the vectorized version of our function:
  set.seed(54321)
  friday.fun.2(population = 435, capacity = 225) #0.7625



## Or using a loop with the non-vectorized version of the function:

  set.seed(54321)
  results <- c()
  for(i in 1:sims){
    results[i] <- friday.fun.3(population = 435, capacity = 225)
  }
  mean(results) #0.7625
  


## Or:

  set.seed(54321)
  mean(replicate(sims, friday.fun.3(population = 435, capacity = 225))) #0.7625

##########################################################
# F #
##########################################################

## Notice below how much faster the vectorized code is than the for loop!

## If we vectorized our code:

friday.fun.4 <- function(population, capacity, probability){
  number.at.bar <- rbinom(n = sims, size = population - 1, prob = probability) + 1
  mean(number.at.bar >= 10 & number.at.bar <= capacity)
}

set.seed(1776)
probabilities <- seq(.05, .95, .05)
results.by.prob <- c()

for(j in 1:length(probabilities)){
  results.by.prob[j] <- mean(friday.fun.4(population = 100, capacity = 60, probability = probabilities[j]))
  print(probabilities[j])
}


## If we don't vectorize the function and use a for-loop instead:

friday.fun.5 <- function(population, capacity, probability){
  # Simulate the number of people going to the bar and add one because you're definitely going
  going.to.bar <- rbinom(n = 1, size = (population - 1), prob = probability) + 1
  
  # Check if the number going is within the right bounds
  going.to.bar >= 10 & going.to.bar <= capacity
}

set.seed(1776)
probabilities <- seq(.05, .95, .05)
results.by.prob <- c()

for(j in 1:length(probabilities)){
  results <- c()
  for(i in 1:sims){
    results[i] <- friday.fun.5(population = 100, capacity = 60, probability = probabilities[j])
  }
  results.by.prob[j] <- mean(results)
  print(probabilities[j])
}


## or
set.seed(1776)
probabilities <- seq(.05, .95, .05)
results.by.prob <- c()

for(j in 1:length(probabilities)){
  results.by.prob[j] <- mean(replicate(sims, friday.fun.5(population = 100,
                                                          capacity = 60,
                                                          probability = probabilities[j])))
  print(probabilities[j])
}

results <- data.frame(prob.going.to.bar = probabilities, prob.fun = results.by.prob)


pdf("assessment2_f.pdf")
plot(results$prob.going.to.bar,
     results$prob.fun,
     pch = 17,
     cex = .7,
     col = "dodgerblue2",
     bty = "l",
     xlab = "Probability Others Go to Bar",
     ylab = "Probability You Have Fun",
     main = "P(Fun Friday) as a Function of P(Others Go To Bar)")
graphics.off()


##########################################################
# G #
##########################################################

# Write a function
# population: total number of House members + Senators (535)
# capacity: vector of the capacity of the two bars (60, 225)
# probability: vector of the probabilities that everyone else goes 
#    to Senate bar, House bar, stays home (.15, .35, .5)
# my.bar.prob: vector of probabilities of going to Senate or House bar (.5, .5)


friday.two.bars <- function(population, capacity, probability, my.bar.prob){
  
  others <- rmultinom(n = sims, size = population - 1, prob = probability) # row1 is number at Bar A, row2 is number at Bar B, row3 is number who stayed home
  me <- rmultinom(n = sims, size = 1, prob = my.bar.prob) # we could also use rbinom() here
  me <- apply(me, 2, function(x) which(x == 1)) # did I go to bar 1 (Senate) or bar 2 (House)?  
  
  # This for loop isn't the fastest way to write this code, but it makes it the most clear to follow
  results <- c()
  for(i in 1:length(me)){
    # Figure out which bar I went to on Friday night i
    my.bar <- me[i]
    # Figure out how many other people were at the bar
    # I went to on Friday night i (add one to include me)
    total.at.my.bar <- others[my.bar,i] + 1
    # Figure out if the bar I was at on Friday night i
    # was too crowded, too empty, or just right
    results[i] <- total.at.my.bar >= 10 & total.at.my.bar <= capacity[my.bar]
  }
  
  return(results)
}

sims <- 10000
set.seed(429)
mean(friday.two.bars(population = 535, 
                     capacity = c(60,225), 
                     probability = c(.15,.4,.45), 
                     my.bar.prob = c(.5, .5)))#0.4123
