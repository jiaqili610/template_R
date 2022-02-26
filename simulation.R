## simulation  (Monte Carlo)

# Jiaqi Li
# 26/02/2022
# from https://web.stanford.edu/class/bios221/labs/simulation/lab_3_simulation.html

# set seed for the same result : )

set.seed(198911)

vecpoisson = rpois(100,5)

mean(vecpoisson)


# using the apply function : )
# recall from probability that the sum of exponentials gives a gamma distribution
# we wll confirm this by simulation

# in general we want to avoid for loops in R since that is slower than working with function such as apply()

reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)



## method 1 ####################################################################

system.time(
  x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
) # replicate


head(x1)

str(x1)

require(ggplot2)

ggplot(data.frame(x1), aes(x1)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun=function(x) dgamma(x, shape=nexps, scale=1/rate),
                color= "red", size=2)


## method 2 ####################################################################

set.seed(0)
system.time(x1 <- sapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))}))

head(x1)

## method 3 ####################################################################

set.seed(0)
system.time(x1 <- lapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))}))
head(x1)

## method 4 fastest ############################################################

# when we apply a simple function(e.g. sum), the fastest way is oftent o just 
# make a matrix of all the simulations and then appy that function to the matrix
set.seed(0)
system.time(x1 <- apply(matrix(rexp(n=nexps*reps, rate=rate), nrow = nexps), 2 , sum))
head(x1)

# speed things up by taking advantage of multiple core processor
require(parallel)
set.seed(0)
system.time(x1 <- mclapply(1:reps, function(i) {sum(rexp(n=nexps, rate=rate))}))

head(x1)


## Monte Carlo simulation - 


# we show how to compute the probability of simple events using simulation
# suppose we rolled two fair dice. What is the probability that their sum is at least 7?
# we will approach this by simulating many throws of two fair dice, and then compute the

isEvent = function(numDice, numSides, targetValue, numTrials){
  apply(matrix(sample(1:numSides, numDice*numTrials, replace=TRUE), nrow= numDice), 2, sum) >= targetValue
}

set.seed(0)
#try 5 trials
outcomes = isEvent(2,6,7,5)
mean(outcomes)


set.seed(0)

outcomes = isEvent(2,6,7,100000)

# calculate the probability (frequency)
mean(outcomes)

