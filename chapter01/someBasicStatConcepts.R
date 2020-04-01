### some illustrations of basic statistical concepts

# I also recommend this website for an interactive display of the
# interactions between power, significance, effect size and population size:
# https://rpsychologist.com/d3/NHST/

## effect size
library(ggplot2)
s1<-rnorm(1000, mean=160,sd=5)

s2<-rnorm(1000, mean=161,sd=5)

dataf<-data.frame(x=c(s1,s2),sample=c(rep("s1",length(s1)),rep("s2",length(s2))))

ggplot(dataf,aes(x=x,fill=sample)) +
  geom_histogram(alpha=0.3,position="identity") +
  geom_vline(aes(xintercept=c(mean(s1))),linetype="dashed",size=1,
             color="salmon") +
  geom_vline(aes(xintercept=c(mean(s2))),linetype="dashed",size=1,
           color="cyan")
t.test(s1,s2)
mean(s1)
mean(s2)


## degrees of freedom
# suppose you have a random variable with the following values:
myCounts<-c(7,6,3,9,4)
# and you estimate a parameter, like the mean:
myMean<-mean(myCounts)
myMean

# imagine you know only the first 4 numbers of myCounts
myCountsPartial<-c(7,6,3,9,NA)
# since you know the mean, it is possible to calculate the last number:
myMean*5-sum(myCountsPartial, na.rm=T)
# so the last (nth) number is completely dependant.

# if you were missing two numbers
myCountsPartial<-c(7,6,3,NA,NA)
# then there would be many combinations that would give the same mean.e.g:
mean(c(7,6,3,8,5))
mean(c(7,6,3,2,11))
#etc
# so that means that n-1 of the numbers are independant.
# that is why you say the mean of a normally distributed sample has
# n-1 degrees of freedom.
# once that number of measurements has been fixed for a given population, # the nth measurement is no longer independant.


## power
# power or sensitivty is the true positive rate TPR=TP/(TP+FN)
# i.e what fraction of all the truely positive results are we detecting?

set.seed(21032020)
# power depends on sample size:
s1<-rnorm(1000, mean=160,sd=5)
s2<-rnorm(1000, mean=161,sd=5)
t.test(s1,s2)
# .. is significant

s1<-rnorm(25, mean=160,sd=5)
s2<-rnorm(25, mean=161,sd=5)
t.test(s1,s2)
# .. is not significant


## power depends on effect size
s1<-rnorm(30, mean=160,sd=5)
s2<-rnorm(30, mean=161,sd=5)
t.test(s1,s2)
# .. is not significant

s1<-rnorm(30, mean=160,sd=5)
s2<-rnorm(30, mean=180,sd=5)
t.test(s1,s2)
# .. is significant

## power depends on significance threshold
s1<-rnorm(35, mean=160,sd=5)
s2<-rnorm(35, mean=161,sd=5)
t.test(s1,s2,conf.level=0.9999)
# .. is not significant

t.test(s1,s2,conf.level=0.95)
# .. is significant

