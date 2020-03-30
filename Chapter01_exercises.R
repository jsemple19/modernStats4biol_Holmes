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

# What is the probability of getting more than 9 false positives in this ELISA test?
# to show that the probability is smaller than 0.000001 (1e-6) we would
# need at least 1e7 simulations
maxes<-replicate(1e7,expr={max(rpois(n=n,lambda=lambda))})
mean(maxes>=m)
# lets check with our function from before
extremeValueProb(m=m,n=n,lambda=lambda)


## Exercise 1.6
?Distributions
xInteger<-c(1:100)
xCont<-seq(1,50,0.5)
xProb<-seq(0,1,0.01)

# beta (continuous)
# distribution of two probabilites
plot(xProb,dbeta(xProb,shape1=0.3,shape2=0.8),main="beta")

# binomial (discrete)
# number of successes in bernoulli trials
plot(xInteger,dbinom(xInteger,size=100,prob=0.3),main="binomial")
#plot(xCont,dbinom(xCont,size=100,prob=0.3))

# Cauchy (continuous)
# describes the ratio of two independant normal distributions with mean 0
plot(xCont,dcauchy(xCont,location=mean(xInteger)),main="Cauchy")

# Chi squared (continuous)
# special case of gamma distribution
# sum of squares of k independant standard normal random variables
plot(xCont,dchisq(xCont,df=3),main="Chi squared")

# exponential (continuous)
# time between events in a Poisson proces (i.e events occur continuously
# and independently at a constant average rate)
# this is the continous analog of geometric distribution
plot(xCont,dexp(xCont,rate=0.3),main="exponential")

# F distribution (continuous)
# arises frequently as the null distribution of a test statistic
# (esecially ANOVA). It is the ratio of the mean squares of two
# independant standard normally distributed variables
plot(xCont,df(xCont,df1=3,df2=5),main="F distribution")

# geometric (discrete)
# the probability that the first occurrence of success requires k
# independent trials, each with success probability p.
plot(xInteger,dgeom(xInteger,prob=0.02),main="geometric")

# hypergeometric (discrete)
# the probability of x sucesses in k draws WITHOUT replacement from
# a finite population size m+n that contains m objects with that feature.
# NOTE: binomial described probability o k success in n draws WITH replacement
plot(xInteger,dhyper(xInteger,m=20,n=80,k=20),main="hypergeometric")

# log-normal (continuous)
# distribution of a random variable whose logarithm is normally distributed
plot(xCont,dlnorm(xCont,meanlog=mean(log(xCont))),main="log-normal")

# multinomial (discrete)
# probability of counts for each of a k-sided die rolled n times.
# no meaningful easy plot

# negative bionomial (discrete)
# the number of failures in a sequence of bernouli trials before a
# specified number of successes occurs.
plot(xInteger,dnbinom(xInteger,size=10,prob=c(0.3)),main="negative binomial")

# normal (continuous)
# distribution of a real-valued random variable
plot(xCont,dnorm(xCont,mean=mean(xCont)))

# poisson (discrete)
# probability of a given number of events happening in a fixed
# interval of time or space
plot(xInteger,dpois(xInteger, lambda=10),main="lambda")

# t (continuous)
# distribution of estimates of the man of a normally distributed
# population with a samll sample size.
plot(xCont,dt(xCont,df=5,ncp=mean(xCont)),main="t distribution")

# uniform (continuous)
plot(xCont,dunif(xCont,min=min(xCont),max=max(xCont)),"uniform")

# Weibull (continuous)
# describes particle size distribution
plot(xCont,dweibull(xCont,shape=10,scale=mean(xCont)))
