####################################################
####  CHAPTER 1 exercises at end of chapter  #######
####################################################

## Exercise 1.1

# gamma distribution (continuous).
# Used to mode waiting time to events. (e.g time till death)
# definted for 0<x,alpha,beta)<inf
y<-rgamma(1e4,shape=5)
hist(y,breaks=100)

# negative bionomial distribution (discrete).
# Models the number of failures in a sequence of Bernoulli trials
# before a target number of successes is found
y<-rnbinom(1e4,size=2,p=0.2)
hist(y,breaks=100)

# beta distribution (continuous).
# Special case of the Dirichlet distribution with only two variables.
# Suitable for modelling random behaviour of proportions
# 0<= x,alpha,beta <=1
y<-rbeta(1e4,shape1=0.4,shape2=0.3)
hist(y,breaks=100)

# see a full(?) list here:
# https://www.stat.umn.edu/geyer/old/5101/rlook.html



## Exercise 1.2

# In this chapter we have concentrated on discrete random variables, where the probabilities are concentrated on a countable set of values. How would you calculate the probability mass at the value X=2 for a binomial B(10, 0.3) with dbinom? Use dbinom to compute the cumulative distribution at the value 2, corresponding to P(X<=2), and check your answer with another R function.

# B(10,0.3) tells us the parameters of the distribution we want, n and p
n=10
p=0.3
# caculating the probability mass at X=2:
dbinom(2,size=n,prob=p)
# calculating the cumulative distribution at 2 i.e P(X<=2)
cdfAt2<-pbinom(2,size=n,prob=p)
cdfAt2
# check it with another function
qbinom(cdfAt2,size=n,prob=p)
# pbinom and qbinom are inverse lookup functions


## Exercise 1.3
# Whenever we note that we keep needing a certain sequence of commands, it’s good to put them into a function. The function body contains the instructions that we want to do over and over again, the function arguments take those things that we may want to vary. Write a function to compute the probability of having a maximum as big as m when looking across n Poisson variables with rate lambda.

extremeValueProb<-function(m,n,lambda) {
  p=1-(ppois(m-1,lambda))^n
  return(p)
}

# lets test the function with random sampling
m=11 # maximum value
n=10 # number of poisson tests in which to find m
lambda=5 # paramter of poisson distribution
maxes<-replicate(10000,expr={max(rpois(n,lambda))})
mean(maxes>=m)
# and with our function:
extremeValueProb(m,n,lambda)


# Exercise 1.4
# Rewrite the function to have default values for its arguments (i.e., values that are used by it if the argument is not specified in a call to the function).
extremeValueProb<-function(m=11,n=10,lambda=5) {
  p=1-(ppois(m-1,lambda))^n
  return(p)
}
extremeValueProb()
# if you want to change one or more of the values, you have to specify them:
extremeValueProb(m=8)
extremeValueProb(m=8,lambda=7)


# Exercise 1.5
# In the epitope example, use a simulation to find the probability of having a maximum of 9 or larger in 100 trials. How many simulations do you need if you would like to prove that “the probability is smaller than 0.000001”?
lambda=0.1 # false positive rate P(declare epitope | no epitope)
n=100 # number of positions in protein that are tested (assumed independant)
m=9 #  threshold number for false positives not tolerated
# to show that the probability is smaller than 0.000001 (1e-6) we would
# need at least 1e7 simulations
maxes<-replicate(1e7,expr={max(rpois(n=n,lambda=lambda))})
mean(maxes>=m)
# lets check with our function from before
extremeValueProb(m=m,n=n,lambda=lambda)

