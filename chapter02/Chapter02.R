##############################################################
####  CHAPTER 2 - statistical modelling                #######
##############################################################
# exercises in text

# In the previous chapter we had clear theoretical models of the data, so that
# given a distribution and its parameters, we could calculate probability.This
# is DEDUCTION using probability theory.
# In this chapter we work upwards from the data do find a model that could
# explain the data. This called STATISTICAL INFERENCE.


## 2.3 A simple example of statistical modeling
load("./data/e100.RData")
e99 = e100[-which.max(e100)]

# first step is always look to see what type of data its
e99
# these are clearly integers.
# Now lets visualise their distribution:
barplot(table(e99), space = 0.8, col = "chartreuse4")

# to see if this fits a theoretical poisson distribution we can use a
# rootogram:
library("vcd")
gf1 = goodfit( e99, "poisson")
rootogram(gf1, xlab = "", rect_gp = gpar(fill = "chartreuse4"))
# the bars "haning" from the red curve (the theoretical distributions)
# "miss" the x axis by a bit. The more they miss, the worse the fit.


# Question 2.1
# To calibrate what such a plot looks like with a known Poisson variable, use
# rpois with lambda = 0.05 to generate 100 Poisson distributed numbers and
# draw their rootogram.

simp = rpois(100, lambda = 0.05)
gf2 = goodfit(simp, "poisson")
rootogram(gf2, xlab = "")
# Because these samples were generated from the theoretical distribution, the
# goodness of fit is much better, the green bars sit nicely on the x axis

## Estimating the parameter of the Poisson distribution
# returning to the data with the outlier:
table(e100)

# if lambda=3:
table(rpois(100, 3))
# too many 2s and 3s.. lambda must be smaller


## Question 2.2
# Repeat this simulation with different values of lambda. Can you find one that
# gives counts close to the observed ones just by trial and error?
table(e100) # the real data
table(rpois(100,2)) # lambda=2, too few 0s
table(rpois(100,1)) # lambda=1, too ferw 0s
table(rpois(100,0.7)) # lambda=0.7, not bad.

# Lets go from visual inspection to calculating probability of seeing each
# set of numbers given the poisson parameter value we choose:
prod(dpois(c(0, 1, 2, 7), lambda = 3) ^ (c(58, 34, 7, 1)))
# remember, the dXXX gives you value of the probability density function, i.e
# the probability of observing that value from a particular distribution

## Question 2.3
# Compute the probability as above for m=0,1,2. Does m have to be integer?
# Try computing the probability for m=0.4for example.
prod(dpois(c(0, 1, 2, 7), lambda = 0) ^ (c(58, 34, 7, 1)))
prod(dpois(c(0, 1, 2, 7), lambda = 1) ^ (c(58, 34, 7, 1)))
prod(dpois(c(0, 1, 2, 7), lambda = 2) ^ (c(58, 34, 7, 1)))
prod(dpois(c(0, 1, 2, 7), lambda = 0.4) ^ (c(58, 34, 7, 1)))

# Calculating the log likelihood:
loglikelihood  =  function(lambda, data = e100) {
  sum(log(dpois(data, lambda)))
}

# Now we calculated log-likelihood to many different possible lambdas
lambdas = seq(0.05, 0.95, length = 100)
loglik = vapply(lambdas, loglikelihood, numeric(1))
plot(lambdas, loglik, type = "l", col = "red", ylab = "", lwd = 2,
     xlab = expression(lambda))
m0 = mean(e100)
abline(v = m0, col = "blue", lwd = 2)
abline(h = loglikelihood(m0), col = "purple", lwd = 2)
m0

## Question 2.4
# What does the vapply function do in the above code?
# Hint: check its manual page.
#
# vapply applies a function to each element of a vector

# this can be done with the goodfit() function:
gf  =  goodfit(e100, "poisson")
names(gf)


## Question 2.5
# What are the other components of the output from the goodfit function?
# observed: our data
gf$observed
# count: the categories of the data we observed
gf$count
# fitted: expected counts
gf$fitted
# type: type of theoretical distribution we are fitting to
gf$type
# method: fitting method, e.g. ML=maximum likelihood
gf$method
# df: degrees of freedom = number of measurement categories- number of parameters we are trying to estimate (i think, but not 100% sure)
gf$df # 4 categories (i.e. 0,1,2,7 ) - 2 parameters (lambda and goodness of fit)

