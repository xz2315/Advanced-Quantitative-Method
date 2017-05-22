#########################
### GOV 2001 Assignment 1
### Spring 2016
### R Code
#########################

### Problem 1

### 1.1

# Create deck of cards
deck <- rep(seq(1,13),4)

# Function that checks if there's three of any
# particular card number
tok <- function(hand){
  if(any(table(hand) == 3)) 1
  else 0
}


# Use apply() to simulate it
set.seed(02138)
threeofkind.apply <- apply(as.matrix(1:10000), 1, function(x) tok(sample(deck, 5)))
mean(threeofkind.apply) #0.0226


### 1.2

# A full house has to have 1 three of a kind, and 1 two of a kind:
fh <- function(hand){
  if(all(c(3,2) %in% table(hand))) 1 
  else 0
}

set.seed(8787)
fullhouse.apply <- apply(as.matrix(1:10000), 1, function(x) fh(sample(deck, 5)))
mean(fullhouse.apply) #0.0011


#### 1.3
# Create a deck that is missing 2 jacks, 1 three, 1 two, and 1 five
deck <- c(rep(seq(1,13),3),1,4,6,7,8,9,10,12,13)
deck <- deck[-11]

probfh2 <- NA
set.seed(02138)
fullhouse2draw <- apply(as.matrix(1:10000), 1,  function(x) fh(c(11,11,sample(deck, 3))))
mean(fullhouse2draw) #0.009



#############################
#### Problem 2
#############################

### 2.1

probseat <- NA
set.seed(02138)

seatnums <- seq(1, 100, 1)
getseat <- NULL
for(j in 1:10000){
  seats <- NULL
  seats[1] <- sample(seatnums, 1)
  for(i in 2:length(seatnums)){
    seats[i] <- ifelse(i %in% seats == TRUE, sample(seatnums[!(seatnums %in% seats)], 1), i)
  }
  getseat[j] <- ifelse(seats[100] == 100, 1, 0)
}
  
mean(getseat) # 0.5067



###########################################
#### Problem 3
###########################################


# y is the dependent variable vector, X is the covariate marix
OLS <- function(y,X){ 
  # add a column of 1's for the intercept
  X <- cbind(1,X)
  
  # label the first column `intercept'
  colnames(X)[1] <- c("Intercept")
  
  # compute the betas using the formula (X'X)^-1 X'y
  betas <- solve(t(X) %*% X) %*% t(X) %*% y 
  
  # estimate the fitted values
  fitted <- X %*% betas 
  
  # calculate the residuals
  resid <- fitted - y 
  
  #calculate sigma squared
  sigma.sq <- c(t(resid) %*% resid / (nrow(X) - ncol(X))) 
  se.betas <- sqrt(diag(solve(t(X)%*%X)*sigma.sq))
  
  # return list of betas, sigma^2, standard errors
  output <- list(betas = betas, sigma.sq = sigma.sq, se.betas = se.betas)
  return(output)
}

#3.3
setwd() ## set your working directory!

covs <- read.csv("covs.csv")
set.seed(02138)
betas <- c(4, .5, -4, 1)
covs.aug <- as.matrix(cbind(1, covs))
means <- covs.aug %*% betas
y.sim <- rnorm(1000, means, sqrt(36))


OLS(y.sim, as.matrix(covs))

right <- summary(lm(y.sim ~ as.matrix(covs))) ## Check to see that it's the same!

require(stargazer) ## Install package stargazer for tables
stargazer(lm(y.sim ~ as.matrix(covs)),
          no.space = T,
          covariate.labels = c("$x_1$","$x_2$","$x_3$","$intercept$"),
          dep.var.labels.include = F,
          omit.stat = c("all"))

#######################
#3.4
covs <- as.matrix(covs)
B <- 1000

set.seed(02138)
betas.bs <- matrix(data = NA, ncol = 4, nrow = B)
for(i in 1:B){
  rows <- sample(1:nrow(covs), replace = TRUE)
  betas.bs[i,] <- OLS(y.sim[rows], covs[rows,])$betas
}

bs.ses <- apply(betas.bs, 2, sd)

errors <- matrix(c(summary(lm(y.sim ~ as.matrix(covs)))$coefficients[,2], bs.ses), ncol = 2)
colnames(errors) <- c("Regression SE", "Bootstrap SE")
rownames(errors) <- c("intercept","b1","b2","b3")
xtable(errors, digits = 4)

###############################
#### Problem 4
###############################

#### Create a helper function to roll dice
roll_dice <- function(type){
  ## If it's a green die
  if(type == "Green"){
    return(sample(c("Brain","Brain","Brain","Shotgun", "Runner","Runner"), 1, replace=F))
  # If it's a Yellow die
  }else if(type == "Yellow"){
    return(sample(c("Brain","Brain","Shotgun","Shotgun", "Runner","Runner"), 1, replace=F))
  # If it's a Red die
  }else if(type == "Red"){
    return(sample(c("Brain","Shotgun","Shotgun","Shotgun", "Runner","Runner"), 1, replace=F))
  }
  
}
### Number of iterations
niter = 10000

### Set random seed
set.seed(02138)

## Storage vector for scores
scores = rep(NA, niter)

### Start the simulation
for (iter in 1:niter){
  
  ## Boolean for whether player is still in-game
  in_game = T
  
  ## Initialize brains  counter
  num_brains <- 0
  
  ## Initialize contents of the bag of dice: 6 Green dice, 4 Yellow Dice, 3 Red dice
  bag_of_dice = rep(c("Green","Yellow","Red"), times=c(6, 4, 3))
  
  ## How many runners are left-over to roll
  runner_vec <- c()
  
  ## Which dice have resulted in a "shotgun"
  shotgun_vec <- c()
  
  ## While the player is still rolling
  while(in_game){
    
    ### Draw the needed number of dice
    n_New = 3 - length(runner_vec) # Draw 3 - # of left-over runners dice
    
    ### Reset "new dice"
    new_dice <- c()
    
    ### If n_New isn't 0 (and we need to draw dice)
    if (n_New != 0){

      # Pick indices out of the bag
      draw_index = sample(1:length(bag_of_dice), size = n_New, replace=F)
      
      ## Set those dice aside
      new_dice = bag_of_dice[draw_index]
      
      ## Remove them from the bag
      bag_of_dice = bag_of_dice[-draw_index]
    }
    
    ### Combine runners and new dice
    dice_to_roll <- c(runner_vec, new_dice)

    ##### SANITY CHECK: If you're not rolling 3 dice, something's wrong
    if (length(dice_to_roll) != 3){
      stop("Error: Rolling not 3 dice, something went wrong")
    }
    
    ## Reset runner vector 
    runner_vec <- c()
    
    ### Roll the dice
    die_results <- sapply(dice_to_roll, function(x) roll_dice(x))
  
    ### For each die you rolled
    for (q in 1:length(die_results)){
      ## If it's a brain, score it
      if (die_results[q] == "Brain"){
        num_brains <- num_brains + 1
      ## If it's a shotgun, add it to the remainder of shotguns  
      }else if (die_results[q] == "Shotgun"){
        shotgun_vec <- c(shotgun_vec, dice_to_roll[q])
      ## If it's a runner, add it to the runners
      }else if (die_results[q] == "Runner"){
        runner_vec <- c(runner_vec, dice_to_roll[q])
      }
    }
    ### If you rolled >= 3 shotguns, score zero
    if (length(shotgun_vec) >= 3){
      scores[iter] <- 0
      break
      in_game <- F
    }
    ############ Strategy in 4.1
    #### If there are fewer than 3 dice in-bag, score.
    if (length(bag_of_dice) < 3){
      scores[iter] <- num_brains
      break
      in_game <- F
    }
    ### If you have two shotguns, score.
    else if (length(shotgun_vec) == 2){
       scores[iter] <- num_brains
       break
       in_game <- F
    }
  }
}

### Calculate expected number of brains
expected_brains <- mean(scores) # 2.0857 for Strategy 4.1
expected_brains

##########################
#### 4.2

  
### Number of iterations
niter = 10000

### Set random seed
set.seed(02138)

## Storage vector for scores
scores = rep(NA, niter)

### Start the simulation
for (iter in 1:niter){
  
  ## Boolean for whether player is still in-game
  in_game = T
  
  ## Initialize brains counter
  num_brains <- 0
  
  ## Initialize contents of the bag of dice: 6 Green dice, 4 Yellow Dice, 3 Red dice
  bag_of_dice = rep(c("Green","Yellow","Red"), times=c(6, 4, 3))
  
  ## How many runners are left-over to roll
  runner_vec <- c()
  
  ## Which dice have resulted in a "shotgun"
  shotgun_vec <- c()
  
  ## While the player is still rolling
  while(in_game){
    
    ### Draw the needed number of dice
    n_New = 3 - length(runner_vec) # Draw 3 - # of left-over runners dice
    
    ### Reset "new dice"
    new_dice <- c()
    
    ### If n_New isn't 0 (and we need to draw dice)
    if (n_New != 0){
      
      # Pick indices out of the bag
      draw_index = sample(1:length(bag_of_dice), size = n_New, replace=F)
      
      ## Set those dice aside
      new_dice = bag_of_dice[draw_index]
      
      ## Remove them from the bag
      bag_of_dice = bag_of_dice[-draw_index]
    }
    
    ### Combine runners and new dice
    dice_to_roll <- c(runner_vec, new_dice)
    
    ##### SANITY CHECK: If you're not rolling 3 dice, something's wrong
    if (length(dice_to_roll) != 3){
      stop("Error: Rolling not 3 dice, something went wrong")
    }
    
    ## Reset runner vector 
    runner_vec <- c()
    
    ### Roll the dice
    die_results <- sapply(dice_to_roll, function(x) roll_dice(x))
    
    ### For each index in die_results
    for (q in 1:length(die_results)){
      ## If it's a brain, score it
      if (die_results[q] == "Brain"){
        num_brains <- num_brains + 1
        ## If it's a shotgun, add it to the remainder of shotguns  
      }else if (die_results[q] == "Shotgun"){
        shotgun_vec <- c(shotgun_vec, dice_to_roll[q])
        
        ## If it's a runner, add it to the runners
      }else if (die_results[q] == "Runner"){
        runner_vec <- c(runner_vec, dice_to_roll[q])
        
      }
    }
    ### If you rolled >= 3 shotguns, score zero
    if (length(shotgun_vec) >= 3){
      scores[iter] <- 0
      break
      in_game <- F
    }
    #### If there are fewer than 3 dice in-bag, score.
    if (length(bag_of_dice) < 3){
      scores[iter] <- num_brains
      break
      in_game <- F
    }
    #### If you have one shotgun and at least 5 brains.
    else if (length(shotgun_vec) == 1 & num_brains >= 5){
      scores[iter] <- num_brains
      break
      in_game <- F
    }
    ### If you have two shotguns and at least one brain, bank it.
    else if (length(shotgun_vec) == 2 & num_brains >= 1){
      scores[iter] <- num_brains
      break
      in_game <- F
    }
  }
}
### Calculate expected number of brains
expected_brains <- mean(scores)  ## 2.2291 for Strategy 2
expected_brains


