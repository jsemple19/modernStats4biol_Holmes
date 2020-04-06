##############################################################
####  CHAPTER 1 - generative models for discrete data  #######
##############################################################
# exercises in text

## 1.2 A real example - Genetic mutations

#  how often 3 mutations could occur under the Poisson(5) model
dpois(x = 3, lambda = 5)

dpois(x = 0:12, lambda = 5)

barplot(dpois(0:12, 5), names.arg = 0:12, col = "red")



## 1.2 Using discrete probability models

genotype = c("AA","AO","BB","AO","OO","AO","AA","BO","BO",
             "AO","BB","AO","BO","AB","OO","AB","BB","AO","AO")
table(genotype)

genotypeF = factor(genotype)
levels(genotypeF)
genotypeF

# Question1.1
# What if you want to create a factor that has some levels not yet in your data?
genotypeF1 = factor(genotype, levels=c("AA","AB", "AO",
                                       "BB","BO", "OO","QQ"))
genotypeF1



## 1.3.1 Bernoulli trials
# Is the ball falling into the left (0) or right (1) box?
# both boxes have equal probability
rbinom(15, prob = 0.5, size = 1)

# Question 1.2
# Repeat this function call a number of times. Why isn’t the answer always the same?
rbinom(15, prob = 0.5, size = 1)
rbinom(15, prob = 0.5, size = 1)
rbinom(15, prob = 0.5, size = 1)

# Is the ball falling into the left (0) or right (1) box?
# left box with 1/3 probability and right box with 2/3 probability
rbinom(12, prob = 2/3, size = 1)



## 1.3.2 Binomial success counts
# out of 12 throws, how often does it go into the right box?
rbinom(1, prob = 2/3, size = 12)

# important note about random simulations
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)

# reproducible random simulations
set.seed(20121)
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)

set.seed(20121)
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)
rbinom(1, prob = 2/3, size = 120)

# Question 1.3
set.seed(235569515)
rbinom(1, prob = 0.3, size = 15)
#Repeat this function call ten times. What seems to be the most common outcome?
allProbs<-c()
for (i in 1:10) {
  newProb<-rbinom(1, prob = 0.3, size = 15)
  allProbs<-c(allProbs,newProb)
}
allProbs
table(allProbs)
# most frequent is 5, not 4?!

# complete probability mass distribution (distribution of probaility mass function (PMF))
probabilities = dbinom(0:15, prob = 0.3, size = 15)
round(probabilities, 3)

barplot(probabilities, names.arg = 0:15, col = "red")
# PMF:
# each bar represents the probability of getting that number of successes
# sum of height of all bars = 1
sum(probabilities)

#### explain formula

# Question 1.4
# What is the output of the formula for k=3, p=2/3, n=4
# use function choose(n,k)
p=2/3 # probabilitiy of one success
k=3  # number of successes we want to see
n=4  # total number of trials
# The probability of seeing k success with probablitiy 2/3 in n trials:
# calculating using the PMF for a binomial distribution:
choose(n, k)*p^k*(1-p)^(n-k)

# using the dbinom function:
dbinom(3,prob=2/3,size=4)

# lets look at the whole distribution
probabilities=dbinom(0:4,prob=2/3,size=4)
barplot(probabilities, names.arg=0:4, col="red")
sum(probabilities)



# 1.3.3 Poisson distributions

# number of successes (p) is small (i.e. p<0.01)
# number of trials (n) is large (i.e n>=100)
# Poisson distribution only has one parameter: lambda
# lambda=n*p  (n*p<20 when approximating binomials)

# e.g mutation rate. Each replication of a nucleotide (i.e generation or cell cycle)
# is a trial, and a "success" is the occurance of a mutation
# When you are dealing with many generations then you can approximate a Binomial
# distribution (which has two parameters) with a Poisson distribution which only
# has one parameter (lambda)


# Question 1.5
# What is the probability mass distribution of obseving 0:12 mutations in a genome
# of n=10^4 nucleotides, when the probabilityis p=5*10^(-4) per nucleotide? Is it
# similar when modeled by the binomial B(n,p) distribution and by the Poisson
# (lambda=np) distribution?

# using the bionomial distribution
nucleotidesInGenome<-10^4 # n paramater in binomial distribution
probMutationPerNucleotide<-5*1e-4 # p parameter in bionmial distribution

binomProb<-dbinom(x=0:12, size=nucleotidesInGenome, prob=probMutationPerNucleotide)
binomProb

# using the poission distribution
probMutationsPerGeneration<-probMutationPerNucleotide*nucleotidesInGenome # lambda parameter of Poisson distribution (which is n*p)
probMutationsPerGeneration # mutation rate per generation

poisProb<-dpois(x=0:12,lambda=probMutationsPerGeneration)
poisProb

# lets plot both:
par(mfrow=c(2,1)) # change plotting parameters to plot two plots one of top of eachother
barplot(binomProb,names.arg=0:12,main="Binomial")
barplot(poisProb,names.arg=0:12,main="Poisson")
par(mfrow=c(1,1)) # go back to single plots

# the PMF for a poisson distribution ("the formula"):
lambda=5
k=3
# probability of 3 succeesses with a success rate of 5
# e.g. 3 mutations at a rate of 5 per generation
lambda^k*exp(-lambda)/factorial(k)
dpois(x=3, lambda=5)

# Task
# Simulate mutation process occuring over multiple generations

rbinom(1, prob = 5e-4, size = 10000)

simulations = rbinom(n = 300000, prob = 5e-4, size = 10000)
barplot(table(simulations), col = "lavender")


# 1.3.4 A generative model for epitope detection
# ELISE assay for epiptope recognition:
# False positive rate per position in epitope is 1%.
# Test protein at 100 positions (assume independant!).
# Look at 50 patient samples.

# one patient's data:
rbinom(n=100, prob=0.01, size=1)

# 50 patient's data
elisasBinom<- rbinom(n=100, prob=0.01, size=50)
elisasBinom

# using poisson simulation
elisasPois<-rpois(n=100,lambda=0.5)
elisasPois

par(mfrow=c(2,1))
barplot(elisasBinom, names.arg=1:100, main="Binom simulation")
barplot(elisasPois, names.arg=1:100, main="Poisson simulation")
par(mfrow=c(1,1))

# look at data generated by the author
load("../data/e100.RData")
barplot(e100, ylim = c(0, 7), width = 0.7, xlim = c(-0.5, 100.5),
        names.arg = seq(along = e100), col = "darkolivegreen")

# what are the chance of seeing a number as big as 7 (or larger) if no epitope is present?

### cumulative distribution function (CDF)
# X>=7 is the same as 1-(X<=6)
# we know how to get X<=6 by reading off the value of the CDF
1-ppois(q=6,lambda=0.5)

# there is also a paramter for this in the R function:
ppois(q=6, lambda=0.5, lower.tail=FALSE)

# Task
# Check the manual page of ppois for the meaning of the lower.tail argument.


# Extreme value analysis for the Poisson distribution
# Stop! The above calculation is not the correct computation in this case.

# Question 1.6
# Can you spot the flaw in our reasoning if we want to compute the probability
# that we observe these data if there is no epitope?
# We are doing 100 tests (100 positions) not just 1
# And we want to know what is the probability of seeing a number >=7 in 1 or more
# of these tests. This is tricky to calculate because we have ot take into account
# the different probabilities of seeing P(X>=7) in 1 test, in 2 tests, in 3 tests...
# ... in 100 tests.
# So it is simpler to just calculate the probability of NOT seeing a test with
# P(X>=7) for each test, multiply them all together and then subtract that from 1:


# The probability of NOT seeing a number at least as high as 7 in the test of
# each position can be calculated as P(X<=6)
ppois(q=6,lambda=0.5)

# The probability of NEVER seeing a number at least as high as 7 in ANY ONE of
# 100 tests (which we assume are independant and therefore we can multiply the
# probabily for each individual test - see probability rules) is:
(ppois(q=6,lambda=0.5))^100

#Therefore the probability of seeing a number >=7 in at least 1 of the 100 tests
# is:
1-(ppois(q=6,lambda=0.5))^100



# Computing probabilities by simulation
# We can approximate this number we calculated with simulations (always good
# to check becuase of the complicated logic above)
maxes = replicate(100000, {
  max(rpois(100, 0.5))
})
table(maxes)

mean( maxes >= 7 )

# Note that because the probability is so low, we need to run many simulations
# (100,000) to get enough positives to be able to calculate the frequency with
# some accuracy


## 1.4 Multinomial distributions: the case of DNA
# more than 2 outcomes (sum of all outcomes = 1)
# e.g which of 4 nucleotides is in a position

# Task
pA=1/8
pC=3/8
pG=3/8
pT=1/8

#???
runif(4)

# Question 1.7
# Suppose we have four boxes that are equally likely. Using the formula, what is
# the probability of observing 4 in the first box, 2 in the second box, and none
# in the two other boxes?
X<-c(4,2,0,0) #successes by category
n<-sum(X) # total number of successes
m<-length(X) # number of categories
# note that if probability for each category is equal, then the probability of
# each category is 1/m

# using the formula
multinomProbs<-(factorial(n)/(factorial(X[1])*factorial(X[2])*factorial(X[3])*factorial(X[4])))*(1/m)^n
multinomProbs

dmultinom(c(4, 2, 0, 0), prob = rep(1/4, 4))
# equally likely outcomes per category is often used as the null hypothesis
# in statistical testing

# suppose we have 8 characters of four different, equally likely types:
pvec = rep(1/4, 4)
t(rmultinom(1, prob = pvec, size = 8))

# Question 1.8
# what does t stand for?
rmultinom(1, prob = pvec, size = 8)
# t is "transpose" converts a column vector to a row vector (also works on matrices)
x<-matrix(c(1:9),nrow=3)
x
t(x)

# Question 1.9
# How do you interpret the difference between rmultinom(n = 8, prob = pvec,
# size = 1) and rmultinom(n = 1, prob = pvec, size = 8)?

q<-rmultinom(n = 8, prob = pvec, size = 1) # 8 trials each with 1 character put into 4 boxes
rowSums(q)

rmultinom(n = 1, prob = pvec, size = 8) # one trial with 8 characters put into 4 boxes


rmultinom(n = 8, prob = pvec, size = 8) # eight trials with 8 characters put into 4 boxes in each trial



# 1.4.1 Simulating for power
# how big a sample size do I need?
# power=true positive rate=TP/(TP+FN) i.e how many of the things that are really
# positive are you detecting and scoring as positive?
# conventionally aim for power of 0.8
# This means that 20% of experiments will fail to yield a positive result even
# if it is true
# there is a trade off between TPR and FPR

pvec = rep(1/4, 4)
pvec
obsunder0 = rmultinom(1000, prob = pvec, size = 20)
dim(obsunder0)
obsunder0[, 1:11]

# What is the expected value?
# 20 balls thrown into 4 bins:
20/4


# Statistical test: compare observed values to expected values
expected0 = pvec * 20
expected0
sum((obsunder0[, 1] - expected0)^2 / expected0)
sum((obsunder0[, 2] - expected0)^2 / expected0)
sum((obsunder0[, 3] - expected0)^2 / expected0)


#lets do this for the whole table using a function:
stat <- function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd)^2 / exptd)
}
# does the function work? test on first column:
stat(obsunder0[, 1])

#Now apply it to all columns:
S0 = apply(obsunder0, 2, stat)
S0[1:20]
# and get some summary statistics
summary(S0)

# Since these numbers were generated from the rmultinom
# function with equal probability for each bin, we can use
# this as the NULL distribution, i.e the distribution we
# expect from random sampling of relatively small sample sizes (20/4)

hist(S0, breaks = 25, col = "lavender", main = "")


# what is the 95th quantile of the data? (i.e 95% of the data will be
# be smaller than this number)
q95 = quantile(S0, probs = 0.95)
q95

# this is now our significance threshold for significance testing.
# if we do an experiment and throw 20 balls into 4 cups, we can calculate
# the weighted sum of squares of the new test and see if it is biggger than 7.6.


##### weighted sum of squares ###

# lets quickly look at that function we wrote to calculated weighted sum of
# squares
stat = function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd)^2 / exptd)
}

# we subtract expected from observed values to see how "far" they are from
# eachother.
# Why do we square that value?
# Why do we divide by the expected?

stat_notSquared = function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd) / exptd)
}


# why square?
S0_notSquared = apply(obsunder0, 2, stat_notSquared)
round(mean(S0),6)
round(mean(S0_notSquared),0)

# because this is a random sample, all the numbers will be distributed
# equally around the expected value (or mean), so if you do not square them,
# the ones that are below the expected value will give a negative value when
# you subtract them from the mean, and the ones above will give a positive
# value, so they will all cancel out and give 0

# why divide by exptd value?
stat_notWeighted = function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd)^2 )
}

S0_notWeighted = apply(obsunder0, 2, stat_notWeighted)

# lets compare this to using a larger sample size (200 balls)
obsunder1 = rmultinom(1000, prob = pvec, size = 200)
# lets calculate the weighted and non-weighted statistics (we must give the
# exptd value for this larger sample in order to overide the default of 20*pvec
# that is in the function definition)
S1 = apply(obsunder1, 2, stat, exptd=200*pvec)
S1_notWeighted = apply(obsunder1, 2, stat_notWeighted, exptd=200*pvec)

# if we compare the weighted (divided by exptd value) stats:
mean(S0)
mean(S1)

# and the unweighted (not divided by the exptd value) stats:
mean(S0_notWeighted)
mean(S1_notWeighted)

# We see that when weighted, the statistic is on a "standard" scale, so
# different sized samples can be compared.

#######

## Determining our test's power

# We have our null distribution calculated in the last section.
# Now lets generate a test distribution that is not equally distributed
# between the bins
pvecA = c(3/8, 1/4, 3/12, 1/8)
observed = rmultinom(1000, prob = pvecA, size = 20)
dim(observed)

observed[,1:7]
# calculated the average number of balls per bin (i,e, per row)
apply(observed, 1, mean)
# same as:
rowMeans(observed)

expectedA = pvecA * 20
expectedA

stat(observed[, 1])

# calculate the statistics for the new samples
S1 = apply(observed, 2, stat)

# remember our threshold from the null distribution
q95

# check how many cases in our new sample exceed taht threshold
sum(S1 > q95)
# NOTE: S1>q95 is a logical test taht return TRUE or FALSE. TRUE has the value
# of 1, so by summing, you are counting the number of TRUEs

# to know the power of the test we need to know what fraction of the cases
# are greater than the threshold. Using mean() on the vector of TRUEs and FALSEs
# from the logical test will give us the fraction of TRUEs
power = mean(S1 > q95)
power

# even though we know the two samples come from different distributions (we set it
# up that way with the rmultinom function), we see that we could only detect
# that they were different 20% of the time. As mentioned before, we would
# normally aim for a power of 0.8 (i.e if the samples are truely different we,
# would be able to detect it 80% of the time)

# Task
# Repeat the simulation experiments and suggest a new sequence length
# that will ensure that the power is acceptable.

# lets test a sample size of 90
n=90
observed = rmultinom(1000, prob = pvecA, size = n)
S2 = apply(observed, 2, stat, exptd = n * pvec)
power=mean(S2>q95)
power

# with a sample size of 30 we have more than 0.8 power,
# so this would be a useful sample size.

# we did not need to simulate data using Monte Carlo methods
# to compute the 95th quantile, because the statistic we created
# sum((obs-exptd)^2/exptd) is the well known ChiSquared statistic,
# and it has a known standard distribution and functions to do the test
# in R.

# To get the q95 from the R distribution we use the qchisq function
# We need to know how many degreees of freedom (df). This is one less
# than the number of categories we have (4-1=3)
q95<-qchisq(p=0.95, df=3)
q95

# to preform the test we use chisq.test
observed = rmultinom(1000, prob = pvecA, size = 20)

S3 = apply(observed, 2, chisq.test, correct=TRUE)

# this returns a list of test results. e.g the results of the first three tests:
S3[1:3]
# to access a sinle test you need [[]]
S3[[1]]

# Stats results in R usually don't just print results, but
# also make it possible to access key parameters the [[]]
# helps us access the contents of that item in the list
names(S3[[1]])

# extracting the first chisq statistic in the list
S3[[1]]$statistic

# We can extract only the statistic from all list items using
# sapply and '[[' and taking first item in sublist.
S3a<-sapply(S3, '[[', 1)
mean(S3a>q95)


# A better way would be to calculate the power using the
# power.chisq.test() function from DescTools libarary
library(DescTools)
n=20 # total number of observations
w=sqrt(sum((pvecA-pvec)^2/pvec))  # effect size
df=3 # degrees of freedom

power.chisq.test(n=n,df=df,w=w)
# with a sample size of 20 the power is 0.23

