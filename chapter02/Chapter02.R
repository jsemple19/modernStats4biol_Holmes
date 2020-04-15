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
# the bars "hanging" from the red curve (the theoretical distributions)
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
table(rpois(100,1)) # lambda=1, too few 0s
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


## Task
# Compare the value of m to the value that we used previously for lambda, 0.5.
# Redo the modeling that we did in Chapter 1 with m instead of 0.5.

# our m0 was:
m0
# our current m:
gf$par
# the same!

# Simulation from chapter 1 (section 1.3.4)
#50 patient's data

# using poisson simulation
elisasPois<-rpois(n=100,lambda=0.5)
elisasPois

# using our new m:
elisasPois_m<-rpois(n=100,lambda=0.55)
elisasPois_m

par(mfrow=c(3,1))
barplot(e100, ylim = c(0, 7),
        names.arg = seq(along = e100), col = "darkolivegreen")
barplot(elisasPois, names.arg=1:100, main="Poisson simulation",ylim=c(0,7))
barplot(elisasPois_m, names.arg=1:100, main="Poisson simulation, new m",
        ylim=c(0,7))
par(mfrow=c(1,1))
# looks a bit more similar to the data?



## 2.3.1 Classical statistics for classical data
# We see that the mean of the poisson distribution maximises the log-likelihood
# see detailed work through the maths in equations.pdf or Chapter02.pptx

## Question 2.6
# What is the value of modeling with a known distribution?
#  For instance, why is it interesting to know a variable has a Poisson distribution ?
#
#  Known distributions have known properties that make it easier to estimate parameters, or evaluate significance.



## 2.4 Binomial distributions and maximum likelihood
# In a binomially distributed data set we normally know one parameter,
# the number of trials (n), but we typically do not know the probability of seeing a success in a trial.
#
## 2.4.1 An example
n=120 # number of males tested for red-green colour blindedness
cb=c(rep(0,110),rep(1,10)) # results of test, 0 = not colour blind, 1= colour blind.
table(cb)

## Question 2.7
# Which value of p is the most likely given these data?
# Simply calculate it from the proportion of positives:
10/120

probs  =  seq(0, 0.3, by = 0.005)
likelihood = dbinom(sum(cb), prob = probs, size = length(cb))
plot(probs, likelihood, pch = 16, xlab = "probability of success", ylab = "likelihood", cex=0.6)
probs[which.max(likelihood)]
# almost the same... but our sampled probabilities do
# not have the same precision as our theoretical estimate.


## Likelihood for the binomial distribution
# Likelihood and probability are the same mathematical function, but with different interpretations.
# Probability tells us how probable we are to see a particular set of values of the data, given the parameters.
# Likelihood tells us how likely we are to see the parameters given the data
# See detailed workthrough of the maths in the powerpoint or in equations.pdf

loglikelihood = function(theta, n = 300, k = 40) {
 log(choose(n,k)) + k * log(theta) + (n - k) * log(1 - theta)
}

thetas = seq(0, 1, by = 0.001)
plot(thetas, loglikelihood(thetas), xlab = expression(theta),
     ylab = expression(paste("log f(", theta, " | y)")),type = "l")

# maximum lies at 40/300=0.13333
abline(v=0.13333)


## 2.5 More boxes: multinomial data
## 2.5.2 Nucleotide bias
library("Biostrings")
staph = readDNAStringSet("./data/staphsequence.ffn.txt", "fasta")

staph[1]
letterFrequency(staph[[1]], letters = "ACGT", OR = 0)


## Question 2.8
# Why did we use double square brackets in the second line?
# because it is a list-like object. single brackets would have just extracted
# the first sequence as a DNAString type object, to get the actual sequence you
# need [[]].
length(staph[1])
length(staph[[1]])


## Question 2.9
# Following a similar procedure as in Exercise 1.8, test whether the nucleotides
# are equally distributed across the four nucleotides for this first gene.
# letter frequency for first sequence
obs<-letterFrequency(staph[[1]], letters = "ACGT", OR = 0)
# expected frequency (null hypothesis)
uniformFreq<-rep(0.25,4)
uniformFreq

# simplest way:
chisq.test(obs,p=uniformFreq)

# or the simulation way:
sims<-rmultinom(10000,size=sum(obs),prob=uniformFreq)
sims[,1:6]

# function for chi squared statistic
chisqStat<-function(obs,expd){
  sum((obs-expd)^2/expd)
}

# now we apply it to each column of the simulations:
stats<-apply(sims,2,chisqStat,expd=uniformFreq*sum(obs))

# we also need to calculate the chi squared stat for our sample
sampleStat<-chisqStat(obs,uniformFreq*sum(obs))
sampleStat
# now lets test if P(mitoNucCount)<0.001
q99.9<-quantile(stats,probs=0.999)
q99.9
sampleStat>q99.9
# clearly different.

# we can also plot the result of the simulations to get an
# idea of how different it is:
hist(stats,breaks=100,xlim=c(0,sampleStat*1.1))
abline(v=sampleStat,col="red")


## Do the first ten genes come from the same multinomial? or are they subject
# to different evolutionary pressures?
letterFrq = vapply(staph, letterFrequency, FUN.VALUE = numeric(4),
                   letters = "ACGT", OR = 0)
colnames(letterFrq) = paste0("gene", seq(along = staph))
tab10 = letterFrq[, 1:10]
computeProportions = function(x) { x/sum(x) }
prop10 = apply(tab10, 2, computeProportions)
round(prop10, digits = 2)

p0 = rowMeans(prop10)
p0

# we take these summed counts to calculate an overall multinomial probability
# and use this for simulations
cs=colSums(tab10)
cs

# we use "outer" vector multiplication to multiply the probabilities for each
# category with the total number of nucleotides per gene. the result is a
# 4x10 matrix
expectedtab10 = outer(p0, cs, FUN = "*")
round(expectedtab10)

# now we cycle through the total count of nucleotides per gene (cs) using
# sapply (a version of lapply that returns the most "simple" output type, i.e.
# vector or matrix, rather than always returning a list, like lapply). We use
# rmultinom to create a random sample according to the multinomial distribution
# for that particular number of nucleotides and the null hypothesis, which
# in this case is the average probabilities from all 10 genes.
randomtab10 = sapply(cs, function(s) { rmultinom(1, s, p0) } )
all(colSums(randomtab10) == cs)
randomtab10

# but actually we want to do this 1000 times, and to avoid the data getting
# too large we immediately calculate the chi squared statistic, comparing
# the randomtab10 to the expectedtab10, and store only that.
stat = function(obsvd, exptd = 20 * pvec) { # a function to calculate the statistic
  sum((obsvd - exptd)^2 / exptd)
}
B = 1000  # number of simulations
simulstat = replicate(B, {
  randomtab10 = sapply(cs, function(s) { rmultinom(1, s, p0) }) # the random sample
  stat(randomtab10, expectedtab10) # calculating the stastic for the randoms sample
})
S1 = stat(tab10, expectedtab10) # calculating the statistic for our real counts
sum(simulstat >= S1) # checking what fraction of the null distribution statistic
# is bigger or equal to our statistic from the real counts

# plotting this results:
hist(simulstat, col = "lavender", breaks = seq(0, 75, length.out=50))
abline(v = S1, col = "red")
abline(v = quantile(simulstat, probs = c(0.95, 0.99)),
       col = c("darkgreen", "blue"), lty = 2)
# conclusion: these 10 genes do not come from the same distribution, i.e. they
# have very different nucleotide frequencies, i.e the evolutionary forces that
# affected them are very different. (some might be essential, some might have
# mutated a lot etc etc)



## The Chi Squared distrbution

# Small probabilities are hard to compute by Monte Carlo (random) simulations.
# The resolution/granularity is at best, 1/(number of simulations). This is
# why we always use theoretical distributions if possible.
S1 = stat(tab10, expectedtab10) # the statistic for the nucleotide counts vs the expected counts
1-pchisq(S1,df=10*(4-1)) #caculating the probability from the cdf of the theoretical chi squared distribution
# Note: The number of degrees of freedom is 30=10*(4-1), because we have 10
# independant genes, and for each gene, given its length, there are three
# degrees of freedom for the number of nucleotides (if you know the number
# of As Cs and Gs, and the total length, the number of Gs is no longer
# independant)



## 2.6.1 Intermezzo: quantiles and the quantile-quantile plot
## Question 2.10

# Compare the simulstat values and 1000 randomly generated chi squared (df=30)
# random numbers by displaying them in histograms with 50 bins each.
# Compute the quantiles of the simulstat values and compare them to those of
# the chi squared distribution with 30 degrees of freedom. Hint:

# ppoints generates a sequence of probability points (i.e a sequence of
# numbers between 0 and 1) so we can easily compare quantiles accross the whole
# distribution
qs = ppoints(100)
qSims<-quantile(simulstat, qs) #value of the distribution of our simulated data at the various probability values
qTheoretical<-quantile(qchisq(qs, df = 30), qs)# value of the theoretical distribution at various probability values

# We can plot the quantiles against eachother, this is a qqplot
plot(qSims,qTheoretical, main="qqplot")
abline(a=0,b=1) # line with slope 1 going through the origin
# if the distributions are exactly the same, the points should alig nalong the
# line.

## Question 2.11
# Do you know another name for the 0.5 quantile?
# The value which has exactly half the data of the distribution below it and
# half above it = Median!


## Question 2.12
# n the above definition, we were a little vague on how the quantile is defined in general, i.e., not just for 0.22. How is the quantile computed for any number between 0 and 1, including ones that are not multiples of 1/n?

# quantiles are cut points dividing the range of a probability distribution
# into continuous intervals with equal probabilities, or dividing the
# observations in a sample in the same way. (i thought the R help explanation
# was incomprehensible, so i went to wikipedia)

# Rather than drawing the qqplot manually as we did, there is a function in
# R called qqplot, as shown in the book.

qqPlot(qchisq(ppoints(B), df = 30), simulstat, main = "",
       xlab = expression(chi[nu==30]^2), asp = 1, cex = 0.5, pch = 16)
abline(a = 0, b = 1, col = "red")

# as we see the simulattions are a very well described by the theoretical
# distribution, so we can use it to calculate a more precise pvalue.
1 - pchisq(S1, df = 30)



## 2.7 Chargaff’s Rule
load("./data/ChargaffTable.RData")
ChargaffTable


## Question 2.13
# Do these data seem to come from equally likely multinomial categories?
# Can you suggest an alternative pattern?
# Can you do a quantitative analysis of the pattern, perhaps inspired by the simulations above?

# No, they do not seem to come from a multinomial with equally likely categories
# because in most organisms AT are similar to each other but different from GC
# frequencies. This suggests A-T and C-G basepairing
# To test this we could look if: (pC-pG)^2+(pA-pT)^2 is equal to 0

statChf = function(x){ # function to calculate our statistic according to the prediction of Chagraff's rule
  sum((x[, "C"] - x[, "G"])^2 + (x[, "A"] - x[, "T"])^2)
}
chfstat = statChf(ChargaffTable) # calculate for the data
permstat = replicate(100000, {  # create a null distribution where we randomise the order of the columnes in the table
  permuted = t(apply(ChargaffTable, 1, sample))
  colnames(permuted) = colnames(ChargaffTable)
  statChf(permuted)
})
pChf = mean(permstat <= chfstat)
pChf

hist(permstat, breaks = 100, main = "", col = "lavender")
abline(v = chfstat, lwd = 2, col = "red")

# it is very unlikely to get such a close match between As and Ts and
# between Cs and Gs, threfore the data support Chagraff's rule



## Question 2.14
# When computing pChf, we only looked at the values in the null distribution smaller than the observed value. Why did we do this in a one-sided way here?
# We only looked at values smaller than the observed values, becuase that is
# what interests us. and using a one-sided distribution gives us more
# statistical power.



## 2.7.1 Two categorical variables
# So far we have dealt with one variable and two (binomial) or more (multinomial)
# categories of that variable.
# if we want to look at two categorical variables we cross tabulate all
# combinations into a contingency table

HairEyeColor[,, "Female"]


## Question 2.15
# Explore the HairEyeColor object in R. What data type, shape and dimensions does it have?
str(HairEyeColor)
# This is a three-dimentional table 4(Hair)x4(Eye)x2(Sex)
?HairEyeColor


## Color blindness and sex
load("./data/Deuteranopia.RData")
Deuteranopia

# null model has two indpendant binomials, one for colour blindness and one for
# sex.

chisq.test(Deuteranopia)

# so how did that work?
# First we must calculate the row and column sums
DeuterMat<-as.data.frame(Deuteranopia)
blindSum<-rowSums(DeuterMat)
blindSum
sexSum<-colSums(DeuterMat)
sexSum

# from that we can calculate the probability of each variable independantly
# (this is called the marginal probability):
blindProb<-blindSum/sum(blindSum)
blindProb
sexProb<-sexSum/sum(sexSum)
sexProb

# our null hypothesis is that these two sums are indpendant so we can get the
# combined probability by multiplying the different category probabilites:
exptdProb<-outer(blindProb,sexProb,FUN="*")
exptdProb
# and if we multiply by the total number of people we get the count
exptdCount<-exptdProb*sum(sexSum)
exptdCount
# now we just use the chi squared statistic to compare these numbers with
# the real numbers
chisqStat<-sum((Deuteranopia-exptdCount)^2/exptdCount)
chisqStat
# Now we see the probability of getting a result as big or bigger
# than that in the chisq distribution with 1 degree of freedom (degrees
# of freedom for a contingency table are (numRows-1)*(numCols-1)
1-pchisq(chisqStat,df=1)
# this is slightly different for the answer we got above, because the default
# in chisq.test is to apply Yates' continuity correction - i.e a small
# correction that accounts for the fact that we are approximating a discrete
# probability with a continuous probability (like the 10.5 people in our
# exptdCount table). If we remove that correction we get the same answer:
chisq.test(Deuteranopia,correct=F)



## 2.7.2 A special multinomial: Hardy-Weinberg equilibrium

# see derivation of Maximum Loglikelhood estimate (MLE) of the Hardey Weinberg
# equilibrium.

library("HardyWeinberg")
data("Mourant")
Mourant[214:216,]


nMM = Mourant$MM[216]
nMN = Mourant$MN[216]
nNN = Mourant$NN[216]
loglik = function(p, q = 1 - p) {
  2 * nMM * log(p) + nMN * log(2*p*q) + 2 * nNN * log(q)
}
xv = seq(0.01, 0.99, by = 0.01)
yv = loglik(xv)
plot(x = xv, y = yv, type = "l", lwd = 2,
     xlab = "p", ylab = "log-likelihood")
imax = which.max(yv)
abline(v = xv[imax], h = yv[imax], lwd = 1.5, col = "blue")
abline(h = yv[imax], lwd = 1.5, col = "purple")

# calculate allele frequency (af) from genotype counts
phat  =  af(c(nMM, nMN, nNN))
phat

pMM   =  phat^2
qhat  =  1 - phat

# expected values under HW equilibrium
pHW = c(MM = phat^2, MN = 2*phat*qhat, NN = qhat^2)
sum(c(nMM, nMN, nNN)) * pHW


## Visual comparison to the Hardy-Weinberg equilibrium
# de Finetti plot
pops = c(1, 69, 128, 148, 192)
genotypeFrequencies = as.matrix(Mourant[, c("MM", "MN", "NN")])
HWTernaryPlot(genotypeFrequencies[pops, ],
              markerlab = Mourant$Country[pops],
              alpha = 0.0001, curvecols = c("red", rep("purple", 4)),
              mcex = 0.75, vertex.cex = 1)


## Question 2.16
# Make the ternary plot as in the code above, then add the other data points to it, what do you notice? You could back up your discussion using the HWChisq function.
HWTernaryPlot(genotypeFrequencies[-pops, ], alpha = 0.0001,
              newframe = FALSE, cex = 0.5)
# the majority of the populations follow the Harvey Weinberg equilibrium
hwPval<-HWChisqMat(genotypeFrequencies)$pvalvec
sum(hwPval<=0.0001)
# only 20 of the 216 samples do not follow the Harvey Weinberg equilibrium


## Question 2.17
# Divide all total frequencies by 50, keeping the same proportions for each of the genotypes, and recreate the ternary plot.

newgf = round(genotypeFrequencies / 50)
HWTernaryPlot(newgf[pops, ],
              markerlab = Mourant$Country[pops],
              alpha = 0.0001, curvecols = c("red", rep("purple", 4)),
              mcex = 0.75, vertex.cex = 1)

# What happens to the points ?
# What happens to the confidence regions and why?
# The points are all green. This is because with a lower number of counts the
# confidence region is much larrger



## 2.7.3 Concatenating several multinomials: sequence motifs and logos

library("seqLogo")
load("./data/kozak.RData")
kozak
pwm = makePWM(kozak)
seqLogo(pwm, ic.scale = FALSE)



## 2.8 Modeling sequential dependencies: Markov chains
# Markove chains  - probabilities of something at time t is dependent only
# upon the previous k time points. or probability of a sequence at position p
# is only dependent upon k positions



## 2.9 Bayesian Thinking
## Haplotypes
## 2.9.1 Example: haplotype frequencies
# look at haplotype consisting of variable numbers of short tandem repeats (STR)

haplo6=read.table("./data/haplotype6.txt",header = TRUE)
haplo6



## 2.9.2 Simulation study of the Bayesian paradigm for the binomial

#  if we start with a prior belief on thetathat is beta-shaped, observe a
#  dataset of n binomial trials, then update our belief, the posterior
#  distribution on theta will also have a beta distribution, albeit with
#  updated parameters.


## The distribution of Y
# We want to demonstrate by example that if the prior distribution is beta-
# shaped, then we observe a dataset of n binomial trials, and obtained an
# updated posterior distribution, this will also be beta shaped but with
# different parameters

# We generate a beta-shaped prior distribution of probabilities. We use these
# as the probability parameter for the binomial distribution.
rtheta = rbeta(100000, 50, 350) # prior distribution of thetas
y = vapply(rtheta, function(th) { # We simulate a random binomial distribution using these probabilities
  rbinom(1, prob = th, size = 300)
}, numeric(1))
hist(y, breaks = 50, col = "orange", main = "", xlab = "")


## Question 2.18
# Verify that we could have gotten the same result as in the above code chunk by using R’s vectorisation capabilities and writing rbinom(length(rtheta), rtheta, size = 300).
y=rbinom(length(rtheta), rtheta, size = 300)
hist(y, breaks = 50, col = "orange", main = "", xlab = "")


##  Histogram of all the thetas such that Y=40: the posterior distribution
# We will now choose a subset of the data where y==40 and find all the beta
# priors that gave that result. this is the posterior probability (the
# distribution of theta given a certain value of y)
thetaPostEmp = rtheta[ y == 40 ]
hist(thetaPostEmp, breaks = 40, col = "chartreuse4", main = "",
     probability = TRUE, xlab = expression("posterior"~theta))

# we compare this to the theoretical beta distribution given the updated
# parameters of the theta distribution. These were calculated as follows:
# - The parameters for the prior distribution were 50 and 350.
# - The y we chose was 40, and we ran 300 trials (size=300 in rbinom function)
# - So we add 40 successes to 50 = 90 and 300-40=260 failures to 350 to give 610.
# and use 90 and 610 as the new parameters of the beta
densPostTheory  =  dbeta(thetas, 90, 610)
lines(thetas, densPostTheory, type="l", lwd = 3)
# we see tha this theoretical posterior distribution is similar to our
# simluated one.

# Their means are also similar:
mean(thetaPostEmp)

dtheta = thetas[2]-thetas[1]
sum(thetas * densPostTheory * dtheta)
# this is an integration: summing the area under the curve and multiplying it
# by the thickness of the slice (the fraction each slice represents of the total
# x-range (0,1). This gives as an average area under the curve, i.e. the mean

# doing such an integration is difficult if there is more than one paramter
# in the distribution, so we can simply get the mean of simluated data wtih
# the new parameters
# Monte Carlo integration
thetaPostMC = rbeta(n = 1e6, 90, 610)
mean(thetaPostMC)

qqplot(thetaPostMC, thetaPostEmp, type = "l", asp = 1)
abline(a = 0, b = 1, col = "blue")


## Question 2.19
# What is the difference between the simulation that results in thetaPostEmp and
# the Monte Carlo simulation that leads to thetaPostMC?
# The difference between these simulations is that the thetaPostMC has many
# more data points, than the thetaPostEmp. that is why the qqplot agreement
# is not great at the tails. But to estimate the mean this shoul not be a
# problem


## Suppose we had a second series of data
# New series of data with n=150 observations and y=25 success (and 125 failures)
# The new posterior would now be:
# beta(90+25=115, 610+125=735)
# The mean of this distribution would be alpha/(alpha+beta)=115/850=0.135
115/(115+735)

# The Maximum A Posteriori (MAP) estimate is the mode of the posterior
# distribution.
# The mode of a beta distributions is (alpha-1)/(alpha+beta-2)
(115-1)/(115+735-2)

# lets check this numerically
# First using the cumulative density function for beta
densPost2 = dbeta(thetas, 115, 735)
sum(thetas * densPost2 * dtheta)  # mean, by numeric integration
thetas[which.max(densPost2)]      # MAP estimate (mode)

# The checking simulated data
mcPost2   = rbeta(1e6, 115, 735)
mean(mcPost2)                     # mean, by MC


## Question 2.20
# Redo all the computations replacing our original prior with a softer prior (less peaked), meaning that we use less prior information. How much does this change the final result?
# our previous prior was 90,610. A softer prior might be with a tenth of the
# date i.e 9,61
hist(rbeta(700,90,610),breaks=100,xlim=c(0,1))
hist(rbeta(700,9,61),breaks=100,xlim=c(0,1))
# you can see this is a broader distribution

# now the new posterior distribution would be with alpha=9+25=31 and
# beta=61+125=186
densPost2 = dbeta(thetas, 31, 186)
sum(thetas * densPost2 * dtheta)  # mean, by numeric integration
thetas[which.max(densPost2)]

# our estimates have changed a bit but not that much.
# So the prior doesn't change the posterior probability very much unless we
# have an extremely peaked prior because we are very sure of what to expect.
# But in general you want to have enough new data to swamp the prior.


## Confidence Statements for the proportion parameter
quantile(mcPost2, c(0.025, 0.975))


## 2.10 Example: occurrence of a nucleotide pattern in a genome
# This example deals with quasi continuous data: the distribution of
# distances between instances of  motif in the genome.


## Question 2.21
# Explore some of the useful data and functions provided in the Biostrings package by exploring the tutorial vignette.

library("Biostrings")
GENETIC_CODE
IUPAC_CODE_MAP
vignette(package = "Biostrings")
vignette("BiostringsQuickOverview", package = "Biostrings")

library("BSgenome")
ag = available.genomes()
length(ag)
ag[1:2]


library("BSgenome.Ecoli.NCBI.20080805") # E.Coli genome
Ecoli #there are 13 different genome sequences for E. coli
shineDalgarno = "AGGAGGT" # this is the motif we want to search for
ecoli = Ecoli$NC_010473 # we select the sequence of K12 strain, substrain DH10B

# how often does this sequece occur in windows 50kb wide?
window = 50000
starts = seq(1, length(ecoli) - window, by = window)
ends   = starts + window - 1
numMatches = vapply(seq_along(starts), function(i) {
  countPattern(shineDalgarno, ecoli[starts[i]:ends[i]],
               max.mismatch = 0)
}, numeric(1))
table(numMatches)


## Question 2.22
# What distribution might this table fit ?
# This could fit a Poisson distribution: the number of successes in a given
# interval

library("vcd")
gf = goodfit(numMatches, "poisson")
summary(gf)
# very high p value
distplot(numMatches, type = "poisson")
# not great fit. If the distribution fits tthe data, the plot should show a
# straight line. open points show the observed count metameters, the filled
# points show the confidence interval centers, and the dashed lines show the
# confidence intervals.

# Let's inspect the matches
sdMatches = matchPattern(shineDalgarno, ecoli, max.mismatch = 0)
sdMatches

# find the distances between the matches:
betweenmotifs = gaps(sdMatches)
betweenmotifs

# Because this is a random Bernouli occurrence along a sequence we expect
# the gap lengths to be exponentially distributed

library("Renext")
expplot(width(betweenmotifs), rate = 1/mean(width(betweenmotifs)),
        labels = "fit")


## Question 2.23
# There appears to be a slight deviation from the fitted line in Figure 2.23 at the right tail of the distribution, i.e., for the largest values. What could be the reason
# The deviation is probably due to the small number of counts of motifs with
# such large gaps between them. this introduces a large sampling error.



## 2.10.1 Modeling in the case of dependencies
library("BSgenome.Hsapiens.UCSC.hg19")
chr8  =  Hsapiens$chr8
CpGtab = read.table("./data/model-based-cpg-islands-hg19.txt",
                    header = TRUE)
nrow(CpGtab)
head(CpGtab)

# lets select ranges coming from chr8 and then create an IRanges object
irCpG = with(dplyr::filter(CpGtab, chr == "chr8"),
             IRanges(start = start, end = end))
irCpG

# then we creat a GRanges object. This is a useful data structure for
# the position of genomic elements. It is used all the time in Bioconductor.
grCpG = GRanges(ranges = irCpG, seqnames = "chr8", strand = "+")
genome(grCpG) = "hg19"
grCpG

library("Gviz")
ideo = IdeogramTrack(genome = "hg19", chromosome = "chr8")
plotTracks(
  list(GenomeAxisTrack(),
       AnnotationTrack(grCpG, name = "CpG"), ideo),
  from = 2200000, to = 5800000,
  shape = "box", fill = "#006400", stacking = "dense")

CGIview    = Views(unmasked(Hsapiens$chr8), irCpG) # views of CpG chr seq
NonCGIview = Views(unmasked(Hsapiens$chr8), gaps(irCpG)) # views of chr seq of gaps

# Views are a "lookup" method for the actual sequence rather than containing
# the sequence itself, this makes them consume less computer memory
# We use the unmasked sequence because we want to get the real underlying
# nucleotide sequence. masked sequences will have NNNN instead of the sequence
# in regions were there is repetative sequences.


seqCGI      = as(CGIview, "DNAStringSet")
seqNonCGI   = as(NonCGIview, "DNAStringSet")
dinucCpG    = sapply(seqCGI, dinucleotideFrequency)
dinucNonCpG = sapply(seqNonCGI, dinucleotideFrequency)
dinucNonCpG[, 1]

# by calculating the dinucleotide frequency we are actually getting what is
# called in Monte Carlo Chains, the "transition" frequency, i.e. how often,
# as you walk along the sequence do you transition from the frist nucleotide
# in the pair to the second. For example, here an A is followed by another A 389
# times, and by a C 351 times etc.


NonICounts = rowSums(dinucNonCpG)
IslCounts  = rowSums(dinucCpG)

# To convert these to transition probabilities we have to divide by the total
# counts of each type:
# rearrange the counts as a matrix with each cell containing the transition
# frequency from the nucleotide in its rowname to the nucleotide in its column
# name
TI  = matrix( IslCounts, ncol = 4, byrow = TRUE)
TnI = matrix(NonICounts, ncol = 4, byrow = TRUE)
dimnames(TI) = dimnames(TnI) =
  list(c("A", "C", "G", "T"), c("A", "C", "G", "T"))

MI = TI /rowSums(TI)
MI

MN = TnI / rowSums(TnI)
MN

## Question 2.24
# Are the transitions different in the different rows? This would mean that, for instance, P(A|C) != P(A|T)
# Yes, this is indeed the case. For examplein CpG islands (MI): P(A|C)=0.201 and
# P(A|T)=0.098


## Question 2.25
# Are the relative frequencies of the different nucleotides different in CpG islands compared to elsewhere ?
# Yes

freqIsl = alphabetFrequency(seqCGI, baseOnly = TRUE, collapse = TRUE)[1:4]
freqIsl / sum(freqIsl)

freqNon = alphabetFrequency(seqNonCGI, baseOnly = TRUE, collapse = TRUE)[1:4]
freqNon / sum(freqNon)


## Question 2.26
# How can we use these differences to decide whether a given sequence comes from a CpG island?
# Compare the frequencies in our given sequence with those in the CpG and nonCpG
# datasets using the Chi squared statistic

# For very short sequences this might not work because the counts we get for
# each nucleotide will be very small

# Using a Markov Chain to calculate the probability of a short sequence given
# particular transition probabilities is more sensitive.
# We will use a "first order" Markov Chain, this just means that the position
# of a given nucleotide only depends on the ID of the nucleotide directly
# before it. (a second order Markov Chain would depend on the two nucleotides
# before it... etc.)

# therefore the probability of a sequence would be the probability of
# the first nucleotide multiplied by probablities of all the transitions you
# make as you go along the sequence:

#P(x=AGTCC) = P(A)*P(AG)*P(GT)*P(TC)*P(CC)

# you simply use the transition probabilities from the CpG table or the NonCpG
# table to get the probability of it coming from either of these regions.

# We divide these probabilities by eachother to get the ODDS RATIO

# In practice we use log of the odds ratio to avoid dealing with tiny numbers
# This is called LOG-LIKELIHOOD RATIO score

# we calculate the logs once and then sum the relevant ones for our sequence
alpha = log((freqIsl/sum(freqIsl)) / (freqNon/sum(freqNon))) # Log liklihood ratio of single nucleotides
beta  = log(MI / MN)  # log likelihood of trarnsitions

# Now we use alpha and beta to calculate the loglikelihood ratio for our sequence
x = "ACGTTATACTACG"
scorefun = function(x) {
  s = unlist(strsplit(x, ""))
  score = alpha[s[1]] # takes the first nucleotide and uses single nucleotide probability
  if (length(s) >= 2)
    for (j in 2:length(s)) # all remaining nucleotides use transition probilities
      score = score + beta[s[j-1], s[j]]
  score
}
scorefun(x)
# The log likelihood ratio is <1 therefore it is probably not a CpG island sequence


# To see how well our score separate sequences originating from these two
# data sets we will subsample 100bp sequences out of the sequences in our
# two data sets. This will be done by sampling start sites for the subsequences
# uniformally, in proportion  to the length of the sequence.
# (so if the first sequence in CpG dataset is 2000 bases long and the second
# 20,000 bases long, we will take 10x more sequences from the second than from
# the first, up to a maximum of 1000 subsequences).
# Then for each subsequence we will calculate the log likelihood ratio.

generateRandomScores = function(s, len = 100, B = 1000) {
  alphFreq = alphabetFrequency(s)
  isGoodSeq = rowSums(alphFreq[, 5:ncol(alphFreq)]) == 0
  s = s[isGoodSeq]
  slen = sapply(s, length)
  prob = pmax(slen - len, 0)
  prob = prob / sum(prob)
  idx  = sample(length(s), B, replace = TRUE, prob = prob)
  ssmp = s[idx]
  start = sapply(ssmp, function(x) sample(length(x) - len, 1))
  scores = sapply(seq_len(B), function(i)
    scorefun(as.character(ssmp[[i]][start[i]+(1:len)]))
  )
  scores / len
}
scoresCGI    = generateRandomScores(seqCGI)
scoresNonCGI = generateRandomScores(seqNonCGI)

# now we plot the two distributions
br = seq(-0.6, 0.8, length.out = 50)
h1 = hist(scoresCGI,    breaks = br, plot = FALSE)
h2 = hist(scoresNonCGI, breaks = br, plot = FALSE)
plot(h1, col = rgb(0, 0, 1, 1/4), xlim = c(-0.5, 0.5), ylim=c(0,120))
plot(h2, col = rgb(1, 0, 0, 1/4), add = TRUE)
# This Monte Carlo Chain scoring method works quite well to distinguish
# the origin of sequences as short as 100bp
