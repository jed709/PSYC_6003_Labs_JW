library(tidyverse)
library(BayesFactor)
library(ez)
library(bayestestR)

# Q1

#me

p_TrueDiff <- 0.70
p_DiffFalse <- 0.05
p_True <- 0.01
p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue <- (p_TrueDiff*p_True)/p_Diff

p_TrueDiff <- 0.70
p_DiffFalse <- 0.05
p_True <- 0.01
p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue <- (p_TrueDiff*p_True)/p_Diff

#E2

p_True <- p_diffTrue
p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue_e2 <- (p_TrueDiff*p_True)/p_Diff

# colleague

p_True <- 0.95
p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue <- (p_TrueDiff*p_True)/p_Diff

# colleague e2

p_True <- p_diffTrue
p_Diff <- (p_TrueDiff*p_True) + p_DiffFalse*(1-p_True)

p_diffTrue_e2 <- (p_TrueDiff*p_True)/p_Diff

# Q2

sleepDat = sleep

#

t.test(extra ~ group, data = sleepDat, 
       paired = TRUE)

# 

ttestBF(sleepDat[sleepDat$group == 1,]$extra, 
        sleepDat[sleepDat$group == 2,]$extra, 
        paired = TRUE)

# Q3 and Q4

toothDat = ToothGrowth

# 

t.test(len ~ supp, data = toothDat, paired = TRUE)

#

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = TRUE)

#

t.test(len ~ supp, data = toothDat, paired = TRUE, 
       alternative = 'g')

#

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = TRUE, nullInterval = c(-Inf, 0))

# Q5 and Q6

toothDat %>%
  mutate(subnum = 1:nrow(toothDat),
         dose = as.factor(toothDat$dose)) -> toothDat

ezANOVA(toothDat,
        len,
        wid = subnum,
        between = .(supp,dose),
        detailed = T) -> toothAOV

anovaBF(len ~ supp * dose,
        data = toothDat) -> toothAOV_BF

bayesfactor_inclusion(toothAOV_BF)

# Q7

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = TRUE,
        rscale = 'medium')

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = TRUE,
        rscale = 'wide')

ttestBF(toothDat[toothDat$supp == 'VC',]$len, 
        toothDat[toothDat$supp == 'OJ',]$len, 
        paired = TRUE,
        rscale = 'ultrawide')


# Q8

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

# 

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

### Q9

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

#