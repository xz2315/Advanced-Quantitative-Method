##################################
#### Section 1 - R Examples
##################################

##### Example 1 - Blackjack
## What's the probability that a dealer busts when showing a 6?

### Set seed for Random Number Generator
set.seed(02138)

### Set number of iterations
nIter = 10000

### Initialize a place-holder vector to store results
dealer_busts <- rep(NA, nIter)

### Save the card number that the dealer shows
dealer_shows <- "6"
### We're saving as a string because that's how our deck
### will be laid out

### Create a function to calculate the hand value
calculate_value <- function(hand){
  ## Convert face cards to their value
  hand[hand %in% c("J","Q","K")] <- "10"
  ## Label aces 11 for now
  hand[hand %in% c("A")] <- "11"
  ## Convert to integer and sum
  value <- sum(as.integer(hand))
  ## If value > 21
  if (value > 21){
    # How many aces are left?
    ace_count <- sum(hand == "11")
    # As long as there are some 11-valued aces
    while(ace_count > 0){
      # Find an 11-valued ace and make it a 1
      hand[match("11",hand)] <- "1" 
      # Recalculate value
      value <- sum(as.integer(hand))
      # If you brought the value down below or equal to 21, break early
      if (value <= 21){
        break
      }
      # Recalculate number of aces
      ace_count <- sum(hand == "11")
    }
  }
  return(value)
}

### Start the simulation
for (iter in 1:nIter){
  ### Initialize a deck of 52 cards (Note, all of these are strings)
  deck <- rep(c(2,3,4,5,6,7,8,9,10,"J","Q","K","A"), 4) 
  deck <- deck[-match(dealer_shows, deck)] # match() returns the position of the first matches in a vector
  ### Initialize a place-holder containing their hand
  hand <- c(dealer_shows)
  ### Choose the second card
  card2 <- sample(deck, 1)
  ### Take that card out of the deck
  deck <- deck[-match(card2, deck)]
  ### Add it to the hand
  hand <- c(dealer_shows, card2)
  ### Is the dealer still playing?
  play_on <- T
  ### While the dealer is still playing
  while(play_on){
    ### Save hand value
    hand_value <- calculate_value(hand)
    ### Did the dealer bust?
    if (hand_value > 21){
      ### Dealer busted, store a 1
      dealer_busts[iter] <- 1
      break # Break the loop
    }
    ### Should the dealer stand (dealer stands on soft 17)
    else if (hand_value >= 17 & hand_value <= 21){
      ### Dealer didn't bust, store a 1
      dealer_busts[iter] <- 0
      break # Break the loop
    ### Otherwise, the dealer hits  
    }else{
      ## Choose the new card
      new_card <- sample(deck, 1)
      ### Take that card out of the deck
      deck <- deck[-match(new_card, deck)]
      ### Add it to the hand
      hand <- c(hand, new_card)      
    }
  }
}
#### Calculate the probability
mean(dealer_busts) ### About 42%

##### Example 2 - Bootstrapping

##############################
#### What happens when we have heteroskedasticity? Can we bootstrap to fix SE estimates?

#### First, what happens when we have only 1 sample

# Set random seed
set.seed(02138)
# Number of observations in our simulated dataset
obs <- 200
# Initialize the data frame that will store our simulated data
simulated_data <- data.frame(Y=rep(NA,200), X=rep(NA, 200))
# For each observation
for (i in 1:obs){
  # Generate some simulated X value from a normal (0,1)
  x <- rnorm(1, 0, 1)
  # Generate our "regression" error but with non-constant variance
  # The variance depends on x - higher for larger absolute values of X
  u <- rnorm(1, 0, 5 + 2*(x^2))
  # Generate Y as a linear combination of X and the error
  y <- 1 + 5*x + u
  # Store in our data frame
  simulated_data[i,] <- c(y, x)
}
### Run Regression
reg <- lm(Y ~ X, data=simulated_data)
### Coefficient estimates
reg$coefficients
### Estimated standard errors
sqrt(diag(vcov(reg)))

#############
#### Second, let's get the true standard errors by Monte Carlo

# Set random seed
set.seed(02138)
## Number of iterations
nIter = 10000
## Placeholder for ``true" sampling distribution
true_sampling <- matrix(ncol=2, nrow=nIter)
for(iter in 1:nIter){
  # Number of observations in our simulated dataset
  obs <- 200
  # Initialize the data frame that will store our simulated data
  simulated_data <- data.frame(Y=rep(NA,200), X=rep(NA, 200))
  # For each observation
  for (i in 1:obs){
    # Generate some simulated X value from a normal (0,1)
    x <- rnorm(1, 0, 1)
    # Generate our "regression" error but with non-constant variance
    # The variance depends on x - higher for larger absolute values of X
    u <- rnorm(1, 0, 5 + 2*(x^2))
    # Generate Y as a linear combination of X and the error
    y <- 1 + 5*x + u
    # Store in our data frame
    simulated_data[i,] <- c(y, x)
  }
  ### Run Regression
  reg <- lm(Y ~ X, data=simulated_data)
  ### Store the point estimates
  true_sampling[iter,] <- reg$coefficients
}
### True SEs
apply(true_sampling, 2, sd)


######################
##### Now, let's see how close the bootstrap gets.

# Set random seed
set.seed(02138)
# Number of observations in our simulated dataset
obs <- 200
# Initialize the data frame that will store our simulated data
single_sample <- data.frame(Y=rep(NA,200), X=rep(NA, 200))
# For each observation
for (i in 1:obs){
  # Generate some simulated X value from a normal (0,1)
  x <- rnorm(1, 0, 1)
  # Generate our "regression" error but with non-constant variance
  # The variance depends on x - higher for larger absolute values of X
  u <- rnorm(1, 0, 5 + 2*(x^2))
  # Generate Y as a linear combination of X and the error
  y <- 1 + 5*x + u
  # Store in our data frame
  single_sample[i,] <- c(y, x)
}

## Number of iterations
nIter <- 10000
## Placeholder for bootstrapped sampling distribution
bootstrap_sampling <- matrix(ncol=2, nrow=nIter)
# For each observation
for (iter in 1:nIter){
  ### Sample n indices of ``single_sample"
  index <- sample(1:nrow(single_sample), nrow(single_sample), replace=T) # With replacement!
  ### Create bootstrapped dataset
  boot_data <- single_sample[index,]
  ### Run Regression
  reg <- lm(Y ~ X, data=boot_data)
  ### Store the point estimates
  bootstrap_sampling[iter,] <- reg$coefficients
}
### Bootstrap SEs
apply(bootstrap_sampling, 2, sd)

##############################################
######## Generating Normal Variates

normal_variates <- rnorm(n=10000, mean=5, sd=6)

pdf("monte_carlo_normal.pdf")
hist(normal_variates, freq=F, main="Monte Carlo simulation of Normal(5, 36) density", xlab="X", breaks=100)
curve(dnorm(x, mean=5, sd=6), lwd=3, col="red", add=T)
dev.off()


