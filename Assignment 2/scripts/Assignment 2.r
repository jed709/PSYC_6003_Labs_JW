# load libraries

library(tidyverse)
library(BayesFactor)
library(ez)
library(bayestestR)

# Question 1 --------------------------------------------------------------

# calculate posteriors using my prior beliefs

# probability of detecting a difference given that a difference exists

p_TrueDiff <- 0.70

# probability of detecting a difference given that no difference exists

p_DiffFalse <- 0.05

# my prior knowledge that a true difference has only a 1% chance of
# existing

p_True <- 0.01

# the average probability of observing a difference 

p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

# using bayes theorem to calculate the probability that a difference exists
# given that a difference was detected

p_diffTrue <- (p_TrueDiff*p_True)/p_Diff

# Recalculating posteriors a second time for a second experiment

# updating priors using the calculated posterior

p_True <- p_diffTrue

# recalculate probability of a difference and posterior

p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue_e2 <- (p_TrueDiff*p_True)/p_Diff

# calculating posterior for E1 using my colleague's priors

# change prior

p_True <- 0.95

# calculate average probability of a difference existing

p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

# calculate posterior

p_diffTrue <- (p_TrueDiff*p_True)/p_Diff

# recalculate posterior using new prior calculated above

# updating priors using the calculated posterior

p_True <- p_diffTrue

# recalculate probability of a difference and posterior

p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue_e2 <- (p_TrueDiff*p_True)/p_Diff

# Question 2 --------------------------------------------------------------

# load data

sleepDat = sleep

# paired two-tailed t.test for the effect of group on extra hours of sleep

t.test(extra ~ group, data = sleepDat, 
       paired = TRUE)

# equivalent bayesian paired t.test

ttestBF(sleepDat[sleepDat$group == 1,]$extra, 
        sleepDat[sleepDat$group == 2,]$extra, 
        paired = TRUE)

# Question 3 --------------------------------------------------------------

# load data

toothDat = ToothGrowth

# independent two-tailed t.test for the effect of supplement type on tooth length

t.test(len ~ supp, data = toothDat, paired = F)

# equivalent bayesian t.test

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = F)

# Question 4 --------------------------------------------------------------

# one-tailed t-test for the effect of supplement type on tooth length
# assuming MDiff for OJ - VC is greater than 0

t.test(len ~ supp, data = toothDat, paired = F, 
       alternative = 'l')

# equivalent one-tailed bayesian t.test

ttestBF(toothDat[toothDat$supp == 'OJ',]$len, 
        toothDat[toothDat$supp == 'VC',]$len, 
        paired = F, nullInterval = c(-Inf, 0))

# Question 5 --------------------------------------------------------------

# create ID column and convert dose to factor

toothDat %>%
  mutate(subnum = 1:nrow(toothDat),
         dose = as.factor(dose)) -> toothDat

# calculate ANOVA on tooth length using dose and supplement as between-subject
# fixed factors

ezANOVA(toothDat,
        len,
        wid = subnum,
        between = .(supp,dose),
        detailed = T) -> toothAOV

# calculate comparable bayes factor ANOVA 

set.seed(999)

anovaBF(len ~ supp * dose,
        whichRandom = 'subnum',
        data = toothDat) -> toothAOV_BF

# interaction vs both effects

7.645356e+14/2.758168e+14

# supp vs both effects

1.198757/2.758168e+14

# dose vs both effects

4.983636e+12/2.758168e+14

# Question 6 --------------------------------------------------------------

# calculate inclusion bayes factors using BF ANOVA on tooth dat

bayesfactor_inclusion(toothAOV_BF)

# Question 7 --------------------------------------------------------------

# recalculate bayesian t-test for the effect of supplement on tooth length
# using prior presets

# medium

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = F,
        rscale = 'medium')

#wide

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = F,
        rscale = 'wide')

# ultra wide

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = F,
        rscale = 'ultrawide')

# Question 8 --------------------------------------------------------------

# define function to calculate probability of observing a given number
# of heads results in a given number of flips dependent on how likely
# a heads result is for the coin being used

coinFlips = function(flips, heads, prob) {
  
  p = (length(combn(flips, heads)) / heads) * 
    (prob^heads) * (1 - prob)^(flips - heads)
  
  return(p)
    
}

# probability of heads for each coin type

h_hb <- 0.80
h_tb <- 0.20
h_f <- 0.50

# probability of picking each coin from the pile

p_hb <- 25/60
p_tb <- 25/60
p_f <- 10/60

# number of coin flips

flips <- 10

# number of heads results

heads <- 7

# avaerage probabilitity of the observation

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb

# probability that the coin is fair given seven heads in ten flips

# coin 1

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# working through Q8 using slides method

# define putcome of the coin flips

f <- c(rep(0,3), rep(1,7))

# define the coins' biases

theta_vals <- c(0.8,0.2,0.5)

# define the likelihood of picking each coin from the pile

priors <- c(25/60, 25/60, 10/60)

# calculate likelihood, evidence to standardize the posterior, and posterior
# probabilities

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg
  
# coin 2

flips <- 7
heads <- 3

# p(observed)

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# slides method

f <- c(rep(0,4), rep(1,3))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# coin 3

flips <- 5
heads <- 3

# observed

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# slides method

f <- c(rep(0,2), rep(1,3))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# coin 4 
  
flips <- 2
heads <- 2

# observed

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# slides method

f <- c(rep(0,0), rep(1,2))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# Question 9 --------------------------------------------------------------

# probability of heads for each coin type

h_hb <- 0.80
h_tb <- 0.20
h_f <- 0.50
h_ah <- 1.00
h_sh <- 0.60

# probability of picking each coin from the pile

p_hb <- 25/110
p_tb <- 25/110
p_f <- 10/110
p_ah <- 40/110
p_sh <- 10/110

# number of coin flips

flips <- 10

# number of heads results

heads <- 7

# 

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb +
  coinFlips(flips,heads,h_ah) * p_ah +
  coinFlips(flips,heads,h_sh) * p_sh

# probability that the coin is fair given seven heads in ten flips

# coin 1

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# all heads

(coinFlips(flips,heads,h_ah) * p_ah)/p_o

# 60% heads

(coinFlips(flips,heads,h_sh) * p_sh)/p_o

# working through Q9 using slides method

f <- c(rep(0,3), rep(1,7))

theta_vals <- c(0.8,0.2,0.5,1.0,0.6)

priors <- c(25/110, 25/110, 10/110, 40/110, 10/110)

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# coin 2

# number of coin flips

flips <- 7

# number of heads results

heads <- 3

# 

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb +
  coinFlips(flips,heads,h_ah) * p_ah +
  coinFlips(flips,heads,h_sh) * p_sh

# probability of coins 

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# all heads

(coinFlips(flips,heads,h_ah) * p_ah)/p_o

# 60% heads

(coinFlips(flips,heads,h_sh) * p_sh)/p_o

# slides method

f <- c(rep(0,4), rep(1,3))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# coin 3

# number of coin flips

flips <- 5

# number of heads results

heads <- 3

# 

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb +
  coinFlips(flips,heads,h_ah) * p_ah +
  coinFlips(flips,heads,h_sh) * p_sh

# probability of coins 

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# all heads

(coinFlips(flips,heads,h_ah) * p_ah)/p_o

# 60% heads

(coinFlips(flips,heads,h_sh) * p_sh)/p_o

# slides method

f <- c(rep(0,2), rep(1,3))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

# coin 4

# number of coin flips

flips <- 2

# number of heads results

heads <- 2

# 

p_o <- coinFlips(flips,heads,h_f) * p_f +
  coinFlips(flips,heads,h_hb) * p_hb +
  coinFlips(flips,heads,h_tb) * p_tb +
  coinFlips(flips,heads,h_ah) * p_ah +
  coinFlips(flips,heads,h_sh) * p_sh

# probability of coins 

# fair

(coinFlips(flips,heads,h_f) * p_f)/p_o

# heads biased

(coinFlips(flips,heads,h_hb) * p_hb)/p_o

#tails biased

(coinFlips(flips,heads,h_tb) * p_tb)/p_o

# all heads

(coinFlips(flips,heads,h_ah) * p_ah)/p_o

# 60% heads

(coinFlips(flips,heads,h_sh) * p_sh)/p_o

# slides method

f <- c(rep(0,0), rep(1,2))

lik = dbinom(sum(f), length(f), theta_vals)

marg = sum(priors * lik)

post = (priors*lik)/marg

#