# libraries and functions

library(tidyverse)
library(BayesFactor)
library(bayestestR)
library(brms)
library(ez)
library(tidybayes)
library(emmeans)

# Question 1 --------------------------------------------------------------
 
# sleep data

sleepdat <- sleep

# paired two-tailed t.test for the effect of group on extra hours of sleep

t.test(extra ~ group, data = sleepdat, 
       paired = TRUE)

# bayesian paired t test with 10000 posterior samples

sleep.post <- ttestBF(sleepdat[sleepdat$group == 1,]$extra, 
                      sleepdat[sleepdat$group == 2,]$extra, 
                      paired = T, posterior = T, iterations = 1e5)

# calculate estimates (median HDI) for mu and sigma^2

sleep.post %>%
  as.data.frame() %>%
  median_hdi(mu)

sleep.post %>%
  as.data.frame() %>%
  median_hdi(sig2)

# Question 2 --------------------------------------------------------------

# bayesian paired t test with 10000 posterior samples using ultrawide priors

sleep.post.2 <- ttestBF(sleepdat[sleepdat$group == 1,]$extra, 
                      sleepdat[sleepdat$group == 2,]$extra, 
                      paired = T, posterior = T, 
                      iterations = 1e5, rscale = 'ultrawide')

# calculate estimates for mu and sigma^2

sleep.post.2 %>%
  as.data.frame() %>%
  median_hdi(mu)

sleep.post.2 %>%
  as.data.frame() %>%
  median_hdi(sig2)

# Question 3 --------------------------------------------------------------

# 3 bayesian paired t tests with 3 posterior samples

sleep.post.a <- ttestBF(sleepdat[sleepdat$group == 1,]$extra, 
                        sleepdat[sleepdat$group == 2,]$extra, 
                        paired = T, posterior = T, 
                        iterations = 3)

sleep.post.b <- ttestBF(sleepdat[sleepdat$group == 1,]$extra, 
                        sleepdat[sleepdat$group == 2,]$extra, 
                        paired = T, posterior = T, 
                        iterations = 3)

sleep.post.c <- ttestBF(sleepdat[sleepdat$group == 1,]$extra, 
                        sleepdat[sleepdat$group == 2,]$extra, 
                        paired = T, posterior = T, 
                        iterations = 3)

# estimates for mu for each

# switching intervals because HDI doesn't work for such small posteriors

sleep.post.a %>%
  as.data.frame() %>%
  mean_qi(mu)

sleep.post.b %>%
  as.data.frame() %>%
  mean_qi(mu)

sleep.post.c %>%
  as.data.frame() %>%
  mean_qi(mu)

# comparable interval for our first model

sleep.post %>%
  as.data.frame() %>%
  mean_qi(mu)

# Question 4 --------------------------------------------------------------

# separate histograms

sleep.post %>%
  as.data.frame() %>%
  ggplot(aes(x = mu)) +
  geom_histogram(color = 'black') +
  theme_classic()

sleep.post %>%
  as.data.frame() %>%
  ggplot(aes(x = delta)) +
  geom_histogram(color = 'black') +
  theme_classic()

# combined histogram, for fun

sleep.post %>%
  as.data.frame() %>%
  pivot_longer(cols = c(mu, delta)) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(color = 'black')

# Question 5 --------------------------------------------------------------

# Group 1 should demonstrate greater
# sleep gain than Group 2

mean(sleep.post[,'mu'] > 0)
  
# Group 2 should demonstrate 
# greater sleep gain than Group 1

mean(sleep.post[,'mu'] < 0)

# Group 2 should gain 
# at least 30 minutes of extra sleep each night

mean(sleep.post[,'mu'] < -0.5)

# Group 2 would receive between 1 and 2 hours of additional sleep each night

mean(sleep.post[,'mu'] <= -1 & sleep.post[,'mu'] >= -2)

# negligible difference of no more than 15 minutes in either direction

mean(sleep.post[,'mu'] >= -0.25 & sleep.post[,'mu'] <= 0.25)

# evid ratio

(mean(sleep.post[,'mu'] <= -1 & sleep.post[,'mu'] >= -2))/
  (mean(sleep.post[,'mu'] >= -0.25 & sleep.post[,'mu'] <= 0.25))

# Question 6 --------------------------------------------------------------

TG_noMid = ToothGrowth %>%
  filter(dose != 1) 

TG_noMid = TG_noMid %>%
  mutate(subnum = 1:nrow(TG_noMid),
         dose = as.factor(dose)) 

# calculate ANOVA on tooth length using dose and supplement as between-subject
# fixed factors

ezANOVA(TG_noMid,
        len,
        wid = subnum,
        between = .(supp,dose),
        detailed = T) -> toothAOV

# Question 7 --------------------------------------------------------------

mt.1 <- brm(len~supp*dose,
            data = TG_noMid,
            chains = 4,
            cores = 4,
            file = 'toothmod_1')

prior_summary(mt.1)


# Question 8 --------------------------------------------------------------

mt.2 <- brm(len~supp*dose,
            data = TG_noMid,
            chains = 4,
            cores = 4,
            prior = c(prior(normal(10, 10), class = 'Intercept'),
                      prior(normal(0,20), class = 'b'),
                      prior(normal(0,10), class = 'sigma')),
            sample_prior = 'yes',
            file = 'toothmod_2')


# Question 9 --------------------------------------------------------------

emmeans(mt.2, ~supp*dose)

# dose within supp

pairs(emmeans(mt.2, ~ dose|supp))

# supp within dose

pairs(emmeans(mt.2, ~ supp|dose))

conditional_effects(mt.2)

# Question 10 -------------------------------------------------------------

hypothesis(mt.2, 'Intercept = 0')
hypothesis(mt.2, 'suppVC = 0')
hypothesis(mt.2, 'dose2 = 0')
hypothesis(mt.2, 'suppVC:dose2 = 0')

# Question 11 -------------------------------------------------------------

hypothesis(mt.2, 'dose2 + Intercept = 0')

# Question 12 -------------------------------------------------------------

hypothesis(mt.2, 'suppVC > 0')
hypothesis(mt.2, 'dose2 > 0')

# Question 13 -------------------------------------------------------------

TG_noHi = ToothGrowth %>%
  filter(dose != 2)

TG_noHi = TG_noHi %>%
  mutate(subnum = 1:nrow(TG_noHi),
         dose = as.factor(dose))

priors_noHi <- c(prior(normal(15,5), class = 'Intercept'),
                 prior(normal(0,50), class = 'b'),
                 prior(normal(0,2), class = 'sigma'))

mt.3 <- brm(len~supp*dose,
            data = TG_noHi,
            chains = 4,
            cores = 4,
            prior = priors_noHi,
            sample_prior = 'yes',
            file = 'toothmod_nohi_a')

# emmmeans

emmeans(mt.3, ~supp*dose)

# dose within supp

pairs(emmeans(mt.3, ~ dose|supp))

# supp within dose

pairs(emmeans(mt.3, ~ supp|dose))

conditional_effects(mt.3)


