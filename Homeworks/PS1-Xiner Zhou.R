##### Problem Set 1
##### Xiner Zhou
##### 2/2/2016

## set working directory
getwd()
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 1")

######################################################
## Problem 1:Probability Simulation
######################################################

#### 1.1 Use simulation to approximate the probability of getting three of a kind
####     when first dealt with 5 cards

## set seed for random number generator
set.seed(02138)

## set number of interations
nIter=1000000

## Initialize a place-holder vector to strore results
same3 <- rep(NA,nIter)

## start simulation
for (iter in 1:nIter){
  # Initialize a deck of 52 cards
  deck <- rep(c(2,3,4,5,6,7,8,9,10,"J","Q","K","A"),4)
  
  # choose 3 cards at random, without replacement
  card <- sample(deck,5,replace=F)
  
  # if 3 are of a kind, number of unique elements is 
  # 3 when the other two are different, and 2 when 
  # the other two are the same
  if (max(count(card)$freq)==3){
    # yes, store 1
    same3[iter] <- 1
  }
  else{
    # no, store 0
    same3[iter] <- 0
  }
}

## calculate the probablity
mean(same3) ## Answer: 0.022643


####1.2 Use simulation to approximate the probability of getting a full house 
install.packages("plyr")
require(plyr)

## set seed for random number generator
set.seed(02138)

## set number of interations
nIter=1000000

## Initialize a place-holder vector to strore results
full_house <- rep(NA,nIter)


## start simulation
for (iter in 1:nIter){
  
  # Initialize a deck of 52 cards
  deck <- rep(c(2,3,4,5,6,7,8,9,10,"J","Q","K","A"),4)
 
  # choose 3 cards at random, without replacement
  card <- sample(deck,5,replace=F)
  
  # full house: 3 of a kind and a pair
  if (length(count(card)$freq)==2){
    
   if ((count(card)$freq[1]==2 | count(card)$freq[1]==3) & (count(card)$freq[2]==2 | count(card)$freq[2]==3)){
     # yes, store 1
     full_house[iter] <- 1
   }else{
     # no, store 0
     full_house[iter] <- 0
   }
  
  }
  else{
    # no, store 0
    full_house[iter] <- 0
  }
}

## calculate the probablity
mean(full_house) ### Answer:  0.001438

####1.3 Use simulation to approximate the probability of getting a full house from the second draw

## set seed for random number generator
set.seed(02138)

## set number of interations
nIter=1000000

## Initialize a place-holder vector to strore results
full_house2 <- rep(NA,nIter)

## start simulation
for (iter in 1:nIter){
  
  # Initialize a deck of 52 cards
  deck_left <- c(    4,  6,7,8,9,10,    "Q","K","A",
            2,3,4,5,6,7,8,9,10,    "Q","K","A",
            2,3,4,5,6,7,8,9,10,"J","Q","K","A",
            2,3,4,5,6,7,8,9,10,"J","Q","K","A")
  
  # choose 3 cards at random, without replacement
  card2 <- sample(deck_left,3,replace=F)
  
  # put first hand and second hand together
  card <- c("J","J",card2)
  
  # full house: 3 of a kind and a pair
  if (length(count(card)$freq)==2){
    
    if ((count(card)$freq[1]==2 | count(card)$freq[1]==3) & (count(card)$freq[2]==2 | count(card)$freq[2]==3)){
      # yes, store 1
      full_house2[iter] <- 1
    }else{
      # no, store 0
      full_house2[iter] <- 0
    }
    
  }
  else{
    # no, store 0
    full_house2[iter] <- 0
  }
}

## calculate the probablity
mean(full_house2) ###Answer: 0.010147

 
######################################################
## Problem 2: More Probability by Simulation
######################################################

#### 2.1: Use simulation find the probability that when
####      100th passenger enters the plane, she finds her
####      seat unoccupied?

## set seed for random number generator
set.seed(02138)

## set number of interations
nIter=1000000

## Initialize a place-holder vector to store results
seat100 <- rep(NA,nIter)

## start simulation
for (iter in 1:nIter){
  
  
  ## assign seat number to each passenger
  right_seat <- sample(1:100,100,replace=F)
  
  ## Initialize a place-holder vector to random seats passengers finally choose
  wrong_seat <- rep(NA,100)
  
  ## Initialize a place-holder vector to seats left over
  seats <- 1:100
  
   
  for(people in 1:100){
    
    # for 1st people, choose randomly from 1 to 100
    if (people==1) {
      wrong_seat[people] <- sample(1:100,1)      
    }
    
    # for 2nd to 100th people, depend on if his seat still available
    # !(seats %in% wrong_seat): gives index of seats still available
    else if (people <=99){ 
      if (right_seat[people] %in% seats[!(seats %in% wrong_seat)]) {
        wrong_seat[people] <- right_seat[people]
      }
      else{
        wrong_seat[people] <- sample(seats[!(seats %in% wrong_seat)], 1)
      }
    }
    else {
      wrong_seat[people] <- seats[!(seats %in% wrong_seat)]
    }
    
  }
  
  # evaluate the 100th passenger
  seat100 <- ifelse(right_seat[100]==wrong_seat[100],1,0)
  
}

## calculate the probablity
mean(seat100) ###Answer: Simulated Probability= 1

#### 2.1: Discuss the intuition:
#### If the 1st passenger chose the right seat for herself (prob= 0.01), then everyone else will have the right seat
#### If the 1st passenger chose the wrong seat (assume ith person's seat) which is very likely (prob=0.99), the maximum  
#### probability of 100th person didn't get the right seat, is when ith person took 100th person's seat, which is prob=1/(100-i),
#### but the more likely situation is ith person took other seat rather then 100th person's seat, which makes the likelihood of 
#### 100th person's seat been taken by the rest of passengers even smaller, by the multiplication law of probability. 
#### Hence, the probability of 100th passenger took the right seat is very high, which is close to 1.
 

design <- as.matrix(cbind(rep(1,nrow(covs)),covs))
                
                     
                    m1 <-t(design) %*% design
                    m2 <-t(design) %*% y
                    beta <- solve(m1,m2)
                    
                    # estimate ancillary parameter: regression standard error
                    res <- y-(design %*% beta)
                    se_reg <- as.numeric(t(res) %*% (res)/(nrow(x)-ncol(x)))
                    
                    # estimate standard errors of coefficients
                    se_beta <- sqrt(diag(se_reg*solve(t(design) %*% design)))
                    
                    mylist <- list("Coefficient"=beta, "SE_Reg"=se_reg, "SE_Coefficient"=se_beta)
                    
                    return(mylist)
                    


 

######################################################
## Problem 3: Linear Regression and Bootstrapping
######################################################

#### 3.1: OLS function

OLS <- function(x,y){
  # add 1 to each observation
  design <- as.matrix(cbind(rep(1,nrow(x)),x))
  
  # estimate coefficients 
  m1 <-t(design)%*%design
  m2 <-t(design)%*%y
  beta <- solve(m1,m2)
  
  # estimate ancillary parameter: regression standard error
  res <- y-(design %*% beta)
  se_reg <- as.numeric(t(res) %*% (res)/(nrow(x)-ncol(x)))
  
  # estimate standard errors of coefficients
  se_beta <- sqrt(diag(se_reg*solve(t(design) %*% design)))
  
  mylist <- list("Coefficient"=beta, "SE_Reg"=se_reg, "SE_Coefficient"=se_beta)
  
  return(mylist)

  
}

#### 3.2: 
#### systematic component: mu=4+0.5x1-4x2+x3
#### stochastic component: normal(mu,36)



#### 3.3: Generate one set of simulated outcomes, and estimate OLS coefficients and their standard errors

## set random seed
set.seed(02138)

## Loading data
covs <- read.csv(file="covs.csv",header=T,sep=,)

## Initialize a vectorthat will store simulated data  
simulated_data <- rep(NA,nrow(covs))

## for each observation
for (i in 1:nrow(covs)){
  # generate random errors
  e <- rnorm(n=1, mean=0, sd=6)
  
  # generate Y as a linear combination of X and the error e
  simulated_data[i] <- 4+0.5*covs$x1-4*covs$x2+covs$x3+e
}

## Run regression using use-defined funciton OLS
 
# transform data frame into matrix
X <- as.matrix(covs)
reg <- OLS(x=X,y=simulated_data)

# Coefficient Estimate
reg$Coefficient 
# coefficient standard error estimate
reg$SE_Coefficient
# variance of random error
reg$SE_Reg

# Compare with lm() results
# add 1 to each observation
dataset <- data.frame(X,simulated_data)
fit <-lm(simulated_data ~ X,data=dataset)
summary(fit)

###########################################
###Answer from my function:
## Coefficient Estimate:
# intercept=34.97711677
# x1       = -0.05002802
# x2       =  0.10318981
# x3       = -0.09986311
## coefficient standard error estimate
# intercept= 0.68077919
# x1       = 0.09881244
# x2       = 0.06064973
# x3       = 0.04878684 
 
## variance of random error
# se of random error= 35.82556
#############################################



#### 3.4: Bootstrap

## Set random seed
set.seed(02138)

## Re-sample from the sample: Place-holder for Bootstrapped sampling distribution
## Number of iterations
nIter <- 1000
bootstrap_sampling <- matrix(ncol=4,nrow=nIter)

## For each observation
for (iter in 1:nIter){
  
  # sample index
  sample_index <- sample(1:length(simulated_data), length(simulated_data), replace=T)
  
  # create bootstrapped dependent variable and explanatory variables
  boot_Y <- simulated_data[sample_index]
  boot_X <- X[sample_index,]

  reg <- OLS(x=boot_X,y=boot_Y)
  bootstrap_sampling[iter,] <- reg$Coefficient
}

## Report Bootstrap SEs
apply(bootstrap_sampling,2,sd)

#################################################
##Answer from my Bootstrap
# intercept= 0.67899498
# x1       = 0.10269410
# x2       = 0.06463143
# x3       = 0.04781154

#################################################




######################################################
## Problem 4: Zombie Dice by Monte Carlo Simulation
######################################################

#### 4.1: Strategy 1

### Initialize number of iterations
nIter <- 1000000
## Initializa place-holder for simulated scores
simulated_point <- rep(NA,nIter)
### Start Simulation
for (s in 1:nIter){
  
## Make 3 color dices
Green <- c("Brain","Brain","Brain","Shotgun","Footprint","Footprint")
Yellow <- c("Brain","Brain","Shotgun","Shotgun","Footprint","Footprint")
Red <- c("Brain","Shotgun","Shotgun","Shotgun","Footprint","Footprint")

## Make the original bag containing 13 color-coded dices
bag <- c(rep("Green",6),rep("Yellow",4),rep("Red",3))

## Initialize number of dices left in bag
ndice <- 13

## Initialize number of Brains,Shotguns, and Footprints
nBrains <-0
nShotguns <-0
nFootprints <-0

## Initialize iteraction count
iter <- 1

## Begin the game 
while(ndice>=3 & nShotguns<2){
  
  
  # 1st iteraction
  if(iter==1){
    sample0 <- sample(c(1:13),3,replace=F)
    
    CurrentDice <- bag[sample0]
    bag <- bag[-sample0]
    
    NextDice <- rep(NA,3)
    # Initialize current results
    CurrentResult <- rep(NA,3)
    
    # Now, roll 3 dices
    for(i in 1:3){
      if(CurrentDice[i]=="Green"){
        CurrentResult[i] <- sample(Green,1)
      }else if(CurrentDice[i]=="Red"){
        CurrentResult[i] <- sample(Red,1)
      }else if(CurrentDice[i]=="Yellow"){
        CurrentResult[i] <- sample(Yellow,1)
      }  
      
      # if footptint, then keep in as NextDice for next iteraction 
      if(CurrentResult[i]=="Footprint"){NextDice[i] <- CurrentDice[i]}
      else{NextDice[i] <- NA}

    }
    
    # Now, count how many Brains, Gunshots, and Footprints
    nBrains <-sum(CurrentResult=="Brain")
    nShotguns <-sum(CurrentResult=="Shotgun")
    nFootprints <-sum(CurrentResult=="Footprint")
    ndice <- length(bag)
    
    print("Interaction")
    print(iter)
    print("Number of Brains")
    print(nBrains)
    print("Number of Shotguns")
    print(nShotguns)
    print("Number of Footprints")
    print(nFootprints)
    print("Cuurent Dice")
    print(CurrentDice)
    print("Current Results")
    print(CurrentResult)   
    print("Next Dice")
    print(NextDice)
    
    iter <- iter+1
    if (nShotguns >=3){
      print("Out!")
      break
    }
  }
  
  else if (iter>1){
    # Current number of dice that have came up as "footprints", before this iteration if nFootprints
    # If nFootprints=3, no need to draw again
    # If nFootprints<3, 3-nFootprints is the number of dices needed to draw from the bag
    if(nFootprints<3){ 
      n <- 3-nFootprints
      sample0 <- sample(c(1:ndice),n,replace=F)
      CurrentDice <- c(NextDice,bag[sample0]) 
      bag <- bag[-sample0]
    }
     else{CurrentDice <-  NextDice } 
         
     CurrentDice <- na.omit(CurrentDice)
    
     
    # Now, roll 3 dices
    for(i in 1:3){
      if(CurrentDice[i]=="Green"){
        CurrentResult[i] <- sample(Green,1)
      }else if(CurrentDice[i]=="Red"){
        CurrentResult[i] <- sample(Red,1)
      }else if(CurrentDice[i]=="Yellow"){
        CurrentResult[i] <- sample(Yellow,1)
      }  
      
      # if footptint, then keep in as NextDice for next iteraction 
      if(CurrentResult[i]=="Footprint"){NextDice[i] <- CurrentDice[i]}
      else{NextDice[i] <- NA}
    }
    
    # Now, count how many Brains, Gunshots, and Footprints
    nBrains <-nBrains+sum(CurrentResult=="Brain")
    nShotguns <-nShotguns+sum(CurrentResult=="Shotgun")
    nFootprints <-sum(CurrentResult=="Footprint")
    ndice <- ndice-n
    
    print("Interaction")
    print(iter)
    print("Number of Brains")
    print(nBrains)
    print("Number of Shotguns")
    print(nShotguns)
    print("Number of Footprints")
    print(nFootprints)
    print("Cuurent Dice")
    print(CurrentDice)
    print("Current Results")
    print(CurrentResult)   
    print("Next Dice")
    print(NextDice)
    
    iter <- iter+1
    if (nShotguns >=3){
      print("Out! Shotguns >=3")
      break
    }    
  }
  
}
####   store score
simulated_point[s] <- nBrains
print("Final point")
print(simulated_point[s])
}

#### report simulated expected point
mean(simulated_point)  #### Answer: 2.788332
 



#### 4.2: Strategy 2

### Initialize number of iterations
nIter <- 1000000
## Initializa place-holder for simulated scores
simulated_point <- rep(NA,nIter)
### Start Simulation
for (s in 1:nIter){
  
  ## Make 3 color dices
  Green <- c("Brain","Brain","Brain","Shotgun","Footprint","Footprint")
  Yellow <- c("Brain","Brain","Shotgun","Shotgun","Footprint","Footprint")
  Red <- c("Brain","Shotgun","Shotgun","Shotgun","Footprint","Footprint")
  
  ## Make the original bag containing 13 color-coded dices
  bag <- c(rep("Green",6),rep("Yellow",4),rep("Red",3))
  
  ## Initialize number of dices left in bag
  ndice <- 13
  
  ## Initialize number of Brains,Shotguns, and Footprints
  nBrains <-0
  nShotguns <-0
  nFootprints <-0
  
  ## Initialize iteraction count
  iter <- 1
  
  ## Begin the game 
  while(ndice>=3 & (nBrains<5 | nShotguns!=1) & (nBrains==0 | nShotguns !=2)){
    
    
    # 1st iteraction
    if(iter==1){
      sample0 <- sample(c(1:13),3,replace=F)
      
      CurrentDice <- bag[sample0]
      bag <- bag[-sample0]
      
      NextDice <- rep(NA,3)
      # Initialize current results
      CurrentResult <- rep(NA,3)
      
      # Now, roll 3 dices
      for(i in 1:3){
        if(CurrentDice[i]=="Green"){
          CurrentResult[i] <- sample(Green,1)
        }else if(CurrentDice[i]=="Red"){
          CurrentResult[i] <- sample(Red,1)
        }else if(CurrentDice[i]=="Yellow"){
          CurrentResult[i] <- sample(Yellow,1)
        }  
        
        # if footptint, then keep in as NextDice for next iteraction 
        if(CurrentResult[i]=="Footprint"){NextDice[i] <- CurrentDice[i]}
        else{NextDice[i] <- NA}
        
      }
      
      # Now, count how many Brains, Gunshots, and Footprints
      nBrains <-sum(CurrentResult=="Brain")
      nShotguns <-sum(CurrentResult=="Shotgun")
      nFootprints <-sum(CurrentResult=="Footprint")
      ndice <- length(bag)
      
      print("Interaction")
      print(iter)
      print("Number of Brains")
      print(nBrains)
      print("Number of Shotguns")
      print(nShotguns)
      print("Number of Footprints")
      print(nFootprints)
      print("Cuurent Dice")
      print(CurrentDice)
      print("Current Results")
      print(CurrentResult)   
      print("Next Dice")
      print(NextDice)
      
      iter <- iter+1
      if (nShotguns >=3){
        print("Out!")
        break
      }
    }
    
    else if (iter>1){
      # Current number of dice that have came up as "footprints", before this iteration if nFootprints
      # If nFootprints=3, no need to draw again
      # If nFootprints<3, 3-nFootprints is the number of dices needed to draw from the bag
      if(nFootprints<3){ 
        n <- 3-nFootprints
        sample0 <- sample(c(1:ndice),n,replace=F)
        CurrentDice <- c(NextDice,bag[sample0]) 
        bag <- bag[-sample0]
      }
      else{CurrentDice <-  NextDice } 
      
      CurrentDice <- na.omit(CurrentDice)
      
      
      # Now, roll 3 dices
      for(i in 1:3){
        if(CurrentDice[i]=="Green"){
          CurrentResult[i] <- sample(Green,1)
        }else if(CurrentDice[i]=="Red"){
          CurrentResult[i] <- sample(Red,1)
        }else if(CurrentDice[i]=="Yellow"){
          CurrentResult[i] <- sample(Yellow,1)
        }  
        
        # if footptint, then keep in as NextDice for next iteraction 
        if(CurrentResult[i]=="Footprint"){NextDice[i] <- CurrentDice[i]}
        else{NextDice[i] <- NA}
      }
      
      # Now, count how many Brains, Gunshots, and Footprints
      nBrains <-nBrains+sum(CurrentResult=="Brain")
      nShotguns <-nShotguns+sum(CurrentResult=="Shotgun")
      nFootprints <-sum(CurrentResult=="Footprint")
      ndice <- ndice-n
      
      print("Interaction")
      print(iter)
      print("Number of Brains")
      print(nBrains)
      print("Number of Shotguns")
      print(nShotguns)
      print("Number of Footprints")
      print(nFootprints)
      print("Cuurent Dice")
      print(CurrentDice)
      print("Current Results")
      print(CurrentResult)   
      print("Next Dice")
      print(NextDice)
      
      iter <- iter+1
      if (nShotguns >=3){
        print("Out! Shotguns >=3")
        break
      }    
    }
    
  }
  ####   store score
  simulated_point[s] <- nBrains
  print("Final point")
  print(simulated_point[s])
}

#### report simulated expected point
mean(simulated_point)  #### Answer: 2.753863

 