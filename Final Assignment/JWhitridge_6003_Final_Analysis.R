# load libraries --------------------------------------------------------------


library(metafor)
library(brms)
library(tidyverse)
library(ggpubr)
library(openxlsx)
library(tidybayes)


# load data for e3 ------------------------------------------------------------


# Reading both phases at once

e3_test_dat = NULL
for(l in list.files('data/E3', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/E3', l, sep='/'), sep=',', 
                   header=TRUE, fill=TRUE)
  dat = dat %>% 
    select(participant, testN, words = testWord, condition=testType, 
           participant, conf=keyConf.keys, conf_rt=keyConf.rt, 
           rkn=keyRKN.keys, rkn_rt=keyRKN.rt)
  dat$group = substr(l, 1,1)
  
  
  e3_test_dat = rbind(e3_test_dat, dat)
}

# Create matched foils

e3_test_dat = e3_test_dat %>%
  filter(!is.na(as.numeric(as.character(testN)))) %>%
  mutate(instruction = str_replace(condition, 'new_', ''), 
         is_old = as.numeric(condition %in% c('silent', 'aloud', 'sing')), 
         is_old_cent = is_old - .5) %>%
  mutate(conf=as.numeric(as.character(conf)), 
         condition=recode(condition, 
                          new_a='new_aloud', 
                          new_b='new_silent', 
                          new_c='new_sing')) %>%
  mutate(said_old = as.numeric(conf > 3), 
         instruction = recode(instruction, 
                              a = 'aloud', 
                              b = 'silent', 
                              c = 'sing'))

# fix coding error in "group" variable

e3_test_dat = e3_test_dat %>%
  mutate(group = tolower(group))


# Analysis of Confidence Judgements ---------------------------------------


# specify non-linear formula for analyses of confidence

e3m4 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

# check model structure before specifying priors

get_prior(e3m4,
          family = bernoulli(link="identity"),
          data = e3_test_dat)

# specify priors on d', c, and correlation terms; all e3 models use the
# same structure, so these priors will be recycled

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(normal(0, 1), nlpar = "dprime", class = "sd"),
  prior(normal(0, 1), nlpar = "c"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),
  prior(lkj(4), class = "cor")
)

# fit model

e3_conf_large <- brm(
  e3m4,
  family = bernoulli(link = "identity"),
  data = e3_test_dat,
  prior = Priors,
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  backend = 'cmdstanr',
  sample_prior = 'yes'
)

# calculate SSE difference by group for d'

e3_conf_large %>%
  as_draws_df() %>%
  median_hdi((`b_dprime_instructionsing:groupc`-`b_dprime_instructionaloud:groupc`) - 
               (`b_dprime_instructionsing:groupn`-`b_dprime_instructionaloud:groupn`))

# calculate SSE difference by group for C

e3_conf_large %>%
  as_draws_df() %>%
  median_hdi((`b_c_instructionsing:groupc`-`b_c_instructionaloud:groupc`) - 
               (`b_c_instructionsing:groupn`-`b_c_instructionaloud:groupn`))

# halfeye plot for estimates of d' by condition/group for conf ratings

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_conf_plot

# halfeye plot for contrasts by group (PEs and SSE) for d' for conf ratings

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_conf_con_plot

# halfeye plot for estimates of C by condition/group for conf ratings

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Confidence', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_conf_plot_c

# halfeye plot for contrasts by group (PEs and SSE) for d' for conf ratings

e3_conf_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_conf_con_plot_c


# Analysis of Recollection ------------------------------------------------


# create binarized rec and fam variables

e3_test_dat <- e3_test_dat %>%
  mutate(said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn == 'f'))

# specify non-linear formula for analyses of recollection

e3rm1 <- bf(
  said_rec ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

# check model structure before specifying priors

get_prior(e3rm1,
          family = bernoulli(link="identity"),
          data = e3_test_dat)

# fit model

e3_rec_large <- brm(
  e3rm1,
  family = bernoulli(link = "identity"),
  data = e3_test_dat,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes',
  file = 'models/e3_rec_large'
)

# calculate SSE difference by group for d'

e3_rec_large %>%
  as_draws_df() %>%
  median_hdi((`b_dprime_instructionsing:groupc`-`b_dprime_instructionaloud:groupc`) - 
               (`b_dprime_instructionsing:groupn`-`b_dprime_instructionaloud:groupn`))

# calculate SSE difference by group for C

e3_rec_large %>%
  as_draws_df() %>%
  median_hdi((`b_c_instructionsing:groupc`-`b_c_instructionaloud:groupc`) - 
               (`b_c_instructionsing:groupn`-`b_c_instructionaloud:groupn`))

# halfeye plot for estimates of d' by condition/group for recollection

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_rec_plot

# halfeye plot for contrasts by group (PEs and SSE) for d' for recollection

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_rec_con_plot

# halfeye plot for estimates of C by condition/group for recollection

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Recollection', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_rec_plot_c

# halfeye plot for contrasts by group (PEs and SSE) for C for recollection

e3_rec_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_rec_con_plot_c


# Analysis of Familiarity  ----------------------------------------------------


# remove 'rec' response trials from data for familiarity model; save as
# new dataframe

e3_fd <- e3_test_dat %>%
  filter(said_rec != 1)

# specify non-linear formula for analyses of familiarity

e3fm1 <- bf(
  said_fam ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words), 
  c ~ instruction:group-1 + (instruction-1|participant) + (instruction:group-1|words),
  nl = TRUE
)

# check model structure before specifying priors

get_prior(e3fm1,
          family = bernoulli(link="identity"),
          data = e3_fd)

# fit model

e3_fam_large <- brm(
  e3fm1,
  family = bernoulli(link = "identity"),
  data = e3_fd,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes'
)

# calculate SSE difference by group for d'

e3_fam_large %>%
  as_draws_df() %>%
  median_hdi((`b_dprime_instructionsing:groupc`-`b_dprime_instructionaloud:groupc`) - 
               (`b_dprime_instructionsing:groupn`-`b_dprime_instructionaloud:groupn`))

# calculate SSE difference by group for C

e3_fam_large %>%
  as_draws_df() %>%
  median_hdi((`b_c_instructionsing:groupc`-`b_c_instructionaloud:groupc`) - 
               (`b_c_instructionsing:groupn`-`b_c_instructionaloud:groupn`))

# halfeye plot for estimates of d' by condition/group for familiarity

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e3_fam_plot

# halfeye plot for contrasts by group (PEs and SSE) for d' for familiarity

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_dprime_instructionaloud:groupc',
         Aloud_Unmatched = 'b_dprime_instructionaloud:groupn',
         Sing_Matched = 'b_dprime_instructionsing:groupc',
         Sing_Unmatched = 'b_dprime_instructionsing:groupn',
         Silent_Matched = 'b_dprime_instructionsilent:groupc',
         Silent_Unmatched = 'b_dprime_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_con_fam_plot

# halfeye plot for estimates of C by condition/group for familiarity

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  select(Aloud_Matched:Sing_Unmatched) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Group = if_else(grepl('Un', Condition), 'Unmatched', 'Matched')) %>%
  mutate(Condition = case_when(grepl('Aloud', Condition) ~ 'Aloud',
                               grepl('Sing', Condition) ~ 'Sing',
                               grepl('Silent', Condition) ~ 'Silent')) %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.7, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = 'E2 Familiarity', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5, 2, 0.5)) +
  theme_classic() -> e3_fam_plot_c

# halfeye plot for contrasts by group (PEs and SSE) for C for familiarity

e3_fam_large %>%
  as_draws_df() %>%
  rename(Aloud_Matched = 'b_c_instructionaloud:groupc',
         Aloud_Unmatched = 'b_c_instructionaloud:groupn',
         Sing_Matched = 'b_c_instructionsing:groupc',
         Sing_Unmatched = 'b_c_instructionsing:groupn',
         Silent_Matched = 'b_c_instructionsilent:groupc',
         Silent_Unmatched = 'b_c_instructionsilent:groupn') %>%
  mutate('MatchedAS' = Aloud_Matched - Silent_Matched,
         'MatchedSS' = Sing_Matched - Silent_Matched,
         'MatchedSA' = Sing_Matched - Aloud_Matched,
         'UnmatchedAS' = Aloud_Unmatched - Silent_Unmatched,
         'UnmatchedSS' = Sing_Unmatched - Silent_Unmatched,
         'UnmatchedSA' = Sing_Unmatched - Aloud_Unmatched) %>%
  select(MatchedAS:UnmatchedSA) %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'PE') %>%
  mutate(Group = if_else(grepl('Un', Contrast), 'Unmatched', 'Matched')) %>%
  mutate(Contrast = case_when(grepl('AS', Contrast) ~ 'Aloud-Silent',
                              grepl('SS', Contrast) ~ 'Sing-Silent',
                              grepl('SA', Contrast) ~ 'Sing-Aloud')) %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = PE, fill = Group)) +
  stat_halfeye(point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.5, scale = 0.8, 
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'))+
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,0.5), breaks = seq(-1, 0.5, 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() -> e3_con_fam_plot_c


# Arrange Plots for E3 --------------------------------------------------------


# arrange plots for d'

ggarrange(e3_conf_plot, e3_conf_con_plot, 
          e3_rec_plot, e3_rec_con_plot,
          e3_fam_plot, e3_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1),
          common.legend = T, legend = "bottom")

# arrange plots for C

ggarrange(e3_conf_plot_c, e3_conf_con_plot_c, 
          e3_rec_plot_c, e3_rec_con_plot_c,
          e3_fam_plot_c, e3_con_fam_plot_c,
          nrow = 3, ncol = 2, widths = c(0.75,1),
          common.legend = T, legend = "bottom")


# Load Data for Meta-Analysis ---------------------------------------------


# load data

meta_dat = read.xlsx('data/meta/singing_meta_v4.xlsx', sheet=1) %>%
  filter(include==1) %>%
  mutate(ri = d_cor, 
         sing_sens = ifelse(!is.na(sing_dm), sing_dm, 
                            ifelse(!is.na(sing_am), sing_am, 
                                   ifelse(!is.na(sing_corhm), sing_corhm, NA))),
         sing_sens_sd = ifelse(!is.na(sing_dsd), sing_dsd, 
                               ifelse(!is.na(sing_asd), sing_asd, 
                                      ifelse(!is.na(sing_corhsd), sing_corhsd, NA))),
         aloud_sens = ifelse(!is.na(aloud_dm), aloud_dm, 
                             ifelse(!is.na(aloud_am), aloud_am, 
                                    ifelse(!is.na(aloud_corhm), 
                                           aloud_corhm, NA))),
         aloud_sens_sd = ifelse(!is.na(aloud_dsd), aloud_dsd, 
                                ifelse(!is.na(aloud_asd), aloud_asd, 
                                       ifelse(!is.na(aloud_corhsd), aloud_corhsd, NA))),
         silent_sens = ifelse(!is.na(silent_dm), silent_dm, 
                              ifelse(!is.na(silent_am), silent_am, 
                                     ifelse(!is.na(silent_corhm), silent_corhm, NA))),
         silent_sens_sd = ifelse(!is.na(silent_dsd), silent_dsd, 
                                 ifelse(!is.na(silent_asd), silent_asd, 
                                        ifelse(!is.na(silent_corhsd), silent_corhsd, NA))))


# Meta-Analysis of the SSE ------------------------------------------------


# impute missing correlation for Quinlan and Taylor (2013; E3)

meta_dat[is.na(meta_dat$ri),]$ri = mean(meta_dat$ri, na.rm = T)

# adjust erroneous coding in spreadsheet (unmatched study coded as matched)

meta_dat$sep_fa[13] = 'n'

# calculate observed effects and sampling variance reflecting the raw
# mean difference between the sing and aloud conditions in each study

meta_dat = escalc('MD', m1i=sing_sens, m2i=aloud_sens, 
                  sd1i=sing_sens_sd, sd2i=aloud_sens_sd, 
                  n1i=n_par, n2i = n_par, ri=ri, data=meta_dat)

# add unique identifier for each independent effect size

meta_dat$effid = 1:nrow(meta_dat)

# calculate SEI for meta-analytic models

meta_dat$sei = meta_dat$vi**.5

# create study identifier that is properly leveled to aid in creation of figure
# i wish there was a better way to do this, but there doesn't seem to be;
# could've been easier if we didn't need to slightly modify the plot labels 
# from the excel sheet. All this stuff could also theoretically be less messy
# if ordering worked properly for the excel data, but I couldn't get it to; I 
# had the same issues when conducting the frequentist cumulative meta

meta_dat$study = factor(c('Quinlan & Taylor (2013; Pilot)',
                          'Quinlan & Taylor (2013; E2)',
                          'Quinlan & Taylor (2013; E3)',
                          'Hassall et al. (2016)',
                          'Quinlan & Taylor (2019; E1)',
                          'Quinlan & Taylor (2019; E2)',
                          'Quinlan & Taylor (2019; E3)',
                          'Current Study (E1a)',
                          'Current Study (E1b)',
                          'Current Study (E2 Matched)',
                          'Current Study (E2 Unmatched)',
                          'Current Study (E3 Matched)',
                          'Current Study (E3 Unmatched)'),
levels = c('Quinlan & Taylor (2013; Pilot)',
           'Quinlan & Taylor (2013; E2)',
           'Quinlan & Taylor (2013; E3)',
           'Hassall et al. (2016)',
           'Quinlan & Taylor (2019; E1)',
           'Quinlan & Taylor (2019; E2)',
           'Quinlan & Taylor (2019; E3)',
           'Current Study (E1a)',
           'Current Study (E1b)',
           'Current Study (E2 Matched)',
           'Current Study (E2 Unmatched)',
           'Current Study (E3 Matched)',
           'Current Study (E3 Unmatched)'))

# fit model

m.sse = brm(yi|se(sei) ~ 1 + (1|study), data=meta_dat, 
           chains = 4, iter=80000, cores=4,
           control=list(adapt_delta=.999),
           prior = c(prior(normal(0, .3), class='Intercept'),
                     prior(normal(0, .3), class='sd')))

# calculate 50% and 95% PIs for meta-analysis

median_hdi(posterior_epred(m2.c, 
                           newdata=data.frame(sei=0, study = 'new'), 
                           re_formula=~(1|study), 
                           allow_new_levels=TRUE), .width = c(0.5, 0.95))


# Meta Plot ---------------------------------------------------------------


# extract estimates for each study; save as df

m2_r <- spread_draws(m.sse, r_study[study,term], b_Intercept) %>% 
  mutate(b_Intercept = r_study + b_Intercept)

# extract average estimate; save as df

m2_f <- spread_draws(m.sse, b_Intercept) %>% 
  mutate(study = "Average") 

# bind average and by-study estimates together

m2_all <- bind_rows(m2_r, m2_f) %>% 
  ungroup() %>%
  mutate(study = fct_relevel(study, "Average")) %>%
  mutate(study = str_replace_all(study, "\\.", " "))

# relevel labels again because this is a dataframe; we need the studies
# appear in order (oldest to newest), so this must be consistent with how we
# leveled the studies in meta_dat

m2_all$study = factor(m2_all$study, levels = c('Average', 
                                               'Current Study (E3 Unmatched)',
                                               'Current Study (E3 Matched)',
                                               'Current Study (E2 Unmatched)',
                                               'Current Study (E2 Matched)',
                                               'Current Study (E1b)',
                                               'Current Study (E1a)',
                                               'Quinlan & Taylor (2019; E3)',
                                               'Quinlan & Taylor (2019; E2)',
                                               'Quinlan & Taylor (2019; E1)',
                                               'Hassall et al  (2016)',
                                               'Quinlan & Taylor (2013; E3)',
                                               'Quinlan & Taylor (2013; E2)',
                                               'Quinlan & Taylor (2013; Pilot)'))

# create summary dataframe to be used for labels and observed effects

m2_all_sum <- group_by(m2_all, study) %>% 
  median_hdi(b_Intercept)

# plot the meta analysis

m2_all %>%   
  ggplot(aes(b_Intercept, study)) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_halfeye(.width = c(.5, .95), fill = "#b5b5b5", 
               point_interval = 'median_hdi',
               slab_alpha = 0.5, scale = 0.8) +
  geom_text(data = m2_all_sum %>% 
              mutate_if(is.numeric, round, digits=2) %>%
      mutate_if(is.numeric, formatC, format='f', digits=2),
    aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 1),
    hjust = "inward") +
  geom_point(
    data = meta_dat %>% mutate(study = str_replace_all(study, "\\.", " ")), 
    aes(x=yi), shape = 4, size = 3) + 
  labs(x = 'Raw Mean Difference', y = NULL) +
  # manually add in PI; there is no better way to do this
  annotate("segment", x=-0.07897, xend = 0.4050, y=0.7, yend = 0.7, linetype = 3) +
  theme_classic()

# exploratory moderator analysis including color matching as a fixed effect 

m.sse.mods = brm(yi|se(sei) ~ sep_fa-1 + (1|effid), data=meta_dat,
               chains = 4, iter=80000, cores=4,
               control=list(adapt_delta=.999),
               prior = c(prior(normal(0, .3), class='b'),
                         prior(normal(0, .3), class='sd')))


# Regression Tests -----------------------------------------


# create variables corresponding to scaled SE and scaled sample size

meta_dat = meta_dat %>%
  mutate(se_std = (sei - mean(sei))/sd(sei),
         size_std = (n_par - mean(n_par))/sd(n_par))

# regression test using scaled standard error

regtest_se = brm(yi|se(vi**.5) ~ se_std + (1|plot_label),
                 data=meta_dat, cores=4, chains = 4, 
                 sample_prior = 'yes', 
                 control = list(adapt_delta=.9),
                 prior = c(prior(normal(0, .3), class='Intercept'),
                           prior(normal(0, 1), class = 'b'),
                           prior(normal(0, .3), class='sd')),
                 iter = 80000)

# regression test using scaled sample size

regtest_n = brm(yi|se(vi**.5) ~ size_std + (1|plot_label),
                data=meta_dat, cores=4, chains = 4, 
                sample_prior = 'yes', 
                control = list(adapt_delta=.9999),
                prior = c(prior(normal(0, .3), class='Intercept'),
                          prior(normal(0, 1), class = 'b'),
                          prior(normal(0, .3), class='sd')),
                iter = 80000)


# Cumulative Meta-Analysis ------------------------------------------------


# reversing order by # of participants doesn't work; tried to find a solution,
# couldn't. So, create order in which studies will be added to the model:
# smallest to largest

meta_dat$order = c(13,11,10,8,12,7,6,9,5,4,3,2,1)

# create list to store all the models

metalist <- vector(mode="list")

# for loop to iteratively fit and refit the model for each study

for (i in 1:nrow(meta_dat)) {
  
  # subset main dataframe using only as many studies we want in
  # a given iteration
  
  nd = meta_dat[meta_dat$order <= i,]
  
  # fit model using the subset
  
  temp = brm(yi|se(sei) ~ 1 + (1|plot_label), data=nd, 
             chains = 4, iter=80000, cores=4,
             backend = 'cmdstanr',
             control=list(adapt_delta=.999),
             prior = c(prior(normal(0, .3), class='Intercept'),
                       prior(normal(0, .3), class='sd')))
  
  # add the model to the list
  
  metalist[[i]] <- temp
}


# Cumulative Meta-Analysis Plot -------------------------------------------


# extract posterior for each model and create plot labels; i wish there was
# a better way to do this, but we're working with 13 separate models here,
# so there doesn't seem to be 

metalist[[13]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; Pilot)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; Pilot)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.a

metalist[[12]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E1)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E1)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.b

metalist[[11]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; E2)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; E2)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.c

metalist[[10]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; E3)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; E3)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.d

metalist[[9]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E1a)' = b_Intercept) %>% 
  select('+ Current study (E1a)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.e

metalist[[8]] %>% 
  as_draws_df() %>% 
  mutate('+ Hassall et al. (2016)' = b_Intercept) %>% 
  select('+ Hassall et al. (2016)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.f

metalist[[7]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E2)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E2)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.g

metalist[[6]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E3)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E3)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.h

metalist[[5]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E1b)' = b_Intercept) %>% 
  select('+ Current study (E1b)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.i

metalist[[4]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E2 Unmatched)' = b_Intercept) %>% 
  select('+ Current study (E2 Unmatched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.j

metalist[[3]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E2 Matched)' = b_Intercept) %>% 
  select('+ Current study (E2 Matched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.k

metalist[[2]] %>% 
  as_draws_df() %>% 
  mutate('+ Current Study (E5 Matched)' = b_Intercept) %>% 
  select('+ Current Study (E5 Matched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.l

metalist[[1]] %>% 
  as_draws_df() %>% 
  mutate('Current Study (E5 Unmatched)' = b_Intercept) %>% 
  select('Current Study (E5 Unmatched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', 
               values_to = 'intercept') -> d.m

# lol (bind the above posteriors into a single dataframe)

v <- rbind(d.a,d.b,d.c,d.d,d.e,d.f,d.g,d.h,d.i,d.j,d.k,d.l,d.m)

# level the plot label variable so studies show up in the order we want

v$plot_label= factor(v$plot_label, 
                     levels = c("+ Quinlan & Taylor (2013; Pilot)",
                                "+ Quinlan & Taylor (2019; E1)",
                                "+ Quinlan & Taylor (2013; E2)",
                                "+ Quinlan & Taylor (2013; E3)",
                                "+ Current study (E1a)",
                                "+ Hassall et al. (2016)",
                                "+ Quinlan & Taylor (2019; E2)",
                                "+ Quinlan & Taylor (2019; E3)",
                                "+ Current study (E1b)",
                                "+ Current study (E2 Unmatched)",                  
                                "+ Current study (E2 Matched)",
                                "+ Current Study (E5 Matched)",                   
                                "Current Study (E5 Unmatched)"))

# create summary df from which we will get the labels for the estimates 

cumul_sum <- group_by(v, plot_label) %>%
  median_hdi(intercept)

# plot the models

v %>%
  ggplot(aes(x = intercept, y = plot_label)) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_halfeye(.width = c(.5, .95), fill = "#b5b5b5", 
               point_interval = 'median_hdi',
               slab_alpha = 0.5, scale = 0.8) +
  geom_text(data = cumul_sum%>% 
              mutate_if(is.numeric, round, digits=2) %>%
      mutate_if(is.numeric, formatC, format='f', digits=2),
    aes(label = str_glue("{intercept} [{.lower}, {.upper}]"), x = 1.5),
    hjust = "inward") +
  labs(x = 'Raw Mean Difference', y = NULL) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  theme_classic()