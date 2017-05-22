##### Problem Set 2 Assessment Problem--Happy Hour
##### Xiner Zhou
##### 2/7/2016

#### A:
# You decide that you're going to go to the bar no matter what. The other 99 Senators
# independently decide whether to go to the bar, and each has a 50% probability of going
# to the bar. First, set the seed to 12345 and simulate the total number of Senators in the
# bar on 10,000 Friday nights. Plot a histogram of the PMF.
# Hint: The rbinom() function should be helpful.

set.seed(12345)
# simulate 10,000 nights
nIter <-10000
# place-holder for number of Senators go to bar 
Senator <- rep(NA,nIter)
# start simulation
Senator  <- 1+ rbinom(n=nIter,size=99,prob=0.5)
# plot histogram
setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 2")
pdf("HappyHour_A.pdf")
hist(Senator, breaks=seq(floor(min(Senator)),ceiling(max(Senator)),1),
     xlab="Number of Senators", main="simulate the total number of Senators in the bar on 10,000 Friday nights", col="red")
dev.off()

 
#### B:
# Now, in R, simulate 10,000 possible Friday nights to determine the probability that you
# have a good time at the bar. In other words, what is the probability that between 9 and
# 59 of the other 99 Senators join you at the bar? Again set the seed to 12345.

set.seed(12345)
# simulate 10,000 nights
nIter <-10000
# place-holder for number of Senators go to bar 
Senator <- rep(NA,nIter)
# start simulation
Senator  <- 1+ rbinom(n=nIter,size=99,prob=0.5)
GoodTime <- ifelse(Senator>=10 & Senator<=60,1,0)
#  probability that you have a good time at the bar
mean(GoodTime) # Answer: 0.9762

#### C: 
# Briefly discuss and critique one of the assumptions that underlies this model. Do you
# think that assumption is a reasonable one to make?

# The model assumes that other 99 Senatoes all make their decisions independently but with 
# the same probability of going as 0.5. The first very unrealistic assumption is "independence",
# Senators may know each other, and they probably would communicate about who are going to 
# the bar, think about if Senators want to build social network, each people will be more likely
# to attend if they know there will be more people, or someone of interest will be there.
# The second unrealistic assumption is that each Senator will have the same probablity of going, 
# which is equivalent to say they have the same judgement of whether they should go. However, 
# people's opinion always defer and could be influenced by many factors, like family responsibility
# and etc, Senators with family may have lower probability of going to bar. 

#### D:
# Now let's assume that you're a member of the US House of Representatives, instead of
# the Senate, and that their bar is a little bit bigger in size. Continue to assume that each
# of your colleagues has a 50% probability of going to the bar and that the House bar also
# requires a minimum of 10 people to be fun. Write a function in R that takes, as arguments,
# the total number of Representatives (or Senators) and the maximum number of people
# who can comfortably fit into the bar. This function should return the probability that
# you would have fun on on any single Friday night.

GoodTime <- function(totSenator, max){
  Senator  <- 1+rbinom(n=10000,size=totSenator,prob=0.5)
  GoodTime <- ifelse(Senator>=10 & Senator<=max,1,0)
  return(mean(GoodTime))
}

#### E: 
# Use your function from part D to estimate the probability that you have a fun time at the
# bar, given that the House has 435 members 
# (instead of 100) and the their bar's capacity
# is 225 (instead of 60). Set the seed to 54321.
set.seed(54321)
GoodTime(totSenator=435,max=225) # Answer:0.7492

#### F:
# Assume again that you're in the Senate (with 99 colleagues and a bar that fits 60 people).
# Alter your function so that it also takes as an argument the probability that the other
# Senators go to the bar. Vary this parameter (which was originally 50%) at 5% intervals
# from 5% to 95% (i.e 5%, 10%, 15%...95%) and store the results. Set the seed to 1776.
# Create a graph showing the probability of having a fun Friday night at each level of the
# parameter that you varied.
set.seed(1776)

GoodTime <- function(chance){
  Senator  <- 1+rbinom(n=10000,size=99,prob=chance)
  GoodTime <- ifelse(Senator>=10 & Senator<=60,1,0)
  return(mean(GoodTime))
}
# create a n*1 matrix stores probability
chance_seq <- matrix(seq(0.05,0.95,0.05),nrow=length(seq(0.05,0.95,0.05)),ncol=1)
# apply function GoodTime to each row of matrix
GoodTime_seq <- apply(chance_seq, 1, GoodTime)

# make a bar plot
# assign corresponding probability as names for barplot annotate X-axis
names(GoodTime_seq)<- chance_seq

setwd("C:/Users/xiz933/Desktop/Files/Advanced Quantitative Method-Gary King/Week 2")
pdf("HappyHourF.pdf")
barplot(height=GoodTime_seq, col="red",
        main="Probability of having a fun Friday night at each level of the parameter",
        xlab="Probability that the other Senators go to the bar independently",
        ylab="Probability of having a fun Friday night",
        ylim=c(0,1.2)) 
dev.off()

#### G:
# The Constitution is changed so that members of the House of Representatives and Senators
# are allowed to go to either of the two bars inside the Capitol building. Assume that
# all the other 534 elected representatives (435 House members plus 99 Senators other than
# you) go to the Senate bar with probability 0.15, the House bar with probability 0.4, and
# stay home with probability 0.45. Also assume that you will definitely go to either the
# Senate bar or the House bar, each with equal probability. Set the seed to 429.
# What is the probability that you have a fun Friday night?
# Hint: The rmultinom() function will be useful. Also, this question is meant to be challenging
# to code, so don't worry too much if you struggle to get the right answer.

set.seed(429)
nIter <- 10000
# place-holder for you have a fun friday night for each iteration
Fun <- rep(NA,nIter)
# place-holder for others' decison about where to go (Senator bar, house bar, home)
others <- matrix(NA, 3, nIter, byrow=F)
# place-holder for your decison about where to go (Senator bar, house bar )
you <- matrix(NA, 2, nIter, byrow=F)
# place-holder for others'+your decison about where to go (Senator bar, house bar, home)
tot <- matrix(NA, 3, nIter, byrow=F)

for(i in 1:nIter){
  others[,i] <- rmultinom(n=1,size=534,prob=c(0.15,0.4,0.45))
  you[,i] <- rmultinom(n=1,size=1,prob=c(0.5,0.5))
  tot[,i] <-others[,i]+you[,i]
  if(you[,i]==c(1,0)){
    # Senator bar 10-60
    Fun[i] <- ifelse(tot[1,i]>=10 & tot[1,i]<=60,1,0)
  }
  else if(you[,i]==c(0,1)){
    # House bar 10-225
    Fun[i] <- ifelse(tot[1,i]>=10 & tot[1,i]<=225,1,0)
  }
}
# What is the probability that you have a fun Friday night?
mean(Fun) # Answer: 0.5073

