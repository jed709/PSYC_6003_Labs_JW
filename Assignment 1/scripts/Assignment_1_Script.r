### Load libraries

library(tidyverse)
library(stats)
library(ggplot2)
library(ez)
library(psych)

# Question 4 --------------------------------------------------------------


# define function that accepts 3 numbers as arguments and returns 
# their product if all numbers are even and returns their sum otherwise
 
evenProd = function(x, y, z) {
  
  # check if any arguments are odd numbers by 
  # dividing arguments by 2 and comparing remainders to zero
  
  if(x %% 2 != 0 | 
     y %% 2 != 0 | 
     z %% 2 != 0) {
    
    # if there is an odd number, return the sum of the arguments
    
    return(sum(x, y, z))
    
  }
  
  # if no arguments are odd, return their product
  
  else {
    
    return(x * y * z)
    
  }
  
}

# confirm that the function works

evenProd(2, 6, 8)
evenProd(2, 6, 7)


# Question 6 --------------------------------------------------------------

# read in stroop data file as dataframe

dat = read.csv('data/stroop_dat.csv')

# Question 7 --------------------------------------------------------------

# clean up stroop data

dat %>%
  
  # select only relevant columns
  
  select(c(SubjectIdNumber:c, 
           ResponseTimesButtonP1)) %>%
  
  # rename the poorly named variables
  
  rename(subject = SubjectIdNumber,
         trial = trial.number,
         rt = ResponseTimesButtonP1) %>%
  
  # recode condition column as factor, specify levels
  
  mutate(condition = recode_factor(c, '1' = 'Congruent', 
                            '2' = 'Incongruent')) %>%
  
  # remove old condition column and save as new dataframe
  
  select(-c) -> dat2

# Question 8 --------------------------------------------------------------


dat2 %>%
  
  # create new column with log transformed RT for each row
  
  mutate(log_rt = log(rt),
         
         # create new column wherein responses within time
         # window are coded with 1 and response outside
         # are coded with 0
         
         acc = as.numeric(!is.na(rt))) %>%
  
  # filter rows where acc is 0 and save as new dataframe
  
  filter(acc != 0) -> dat3

# Question 9 --------------------------------------------------------------

# write dat3 dataframe to rds file in data folder

write_rds(dat3, 'data/dat3.rds')

# Question 10 -------------------------------------------------------------

dat3 %>%
  
  # group data by subject ID and condition
  
  group_by(subject, condition) %>%
  
  # produce columns containing the means for rt and log_rt,
  # save as new dataframe
  
  summarise(rt_mean = mean(rt), log_mean = mean(log_rt)) -> sum_dat

# Question 11 -------------------------------------------------------------

# create histograms of RT and log RT means using base graphics

# write to file to report

png('figures/base_hist_rt.png',
    width = 1024,
    height = 768)

# create histogram

hist(sum_dat$rt_mean,
     main = 'Histogram of Response Times',
     ylim = range(0,25),
     xlab = 'Mean RT',
     col = '#b86fd9')

# close plot window

dev.off()

# repeat for log transformed rts

png('figures/base_hist_log_rt.png',
    width = 1024,
    height = 768)

hist(sum_dat$log_mean,
     main = 'Histogram of Log-Transformed Response Times',
     ylim = range(0,25),
     xlab = 'Mean Log RT',
     col = '#b86fd9')

dev.off()

# do the same as above, but with ggplot

# first, specify data and what to put on X axis

ggplot(sum_dat, aes(x = rt_mean)) +
  
  # call ggplot's histogram function
  
  geom_histogram()

# do this again for log RTs

ggplot(sum_dat, aes(x = log_mean)) + 
  geom_histogram()

# those defaults are disgusting; i'll make the above look passable

# raw RTs

png('figures/gg_hist_rt.png',
    width = 1024,
    height = 768)

ggplot(sum_dat, aes(x = rt_mean)) +
  
  # specify number of bins, line width, and color palette
  
  geom_histogram(bins = 15,
                 linewidth = 0.5,
                 color = 'black', 
                 fill = '#b86fd9') +
  
  # use classic theme to remove the ugly background
  
  theme_classic() +
  
  # stop the bins from floating
  
  scale_y_continuous(expand = c(0,0)) +
  
  # add more informative plot labels
  
  labs(title = 'Histogram of Response Times', 
       x = "Mean RT", 
       y = "Frequency") +
  
  # center the title
  
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# same as above but for log transformed RTs

png('figures/gg_hist_log_rt.png',
    width = 1024,
    height = 768)

ggplot(sum_dat, aes(x = log_mean)) + 
  geom_histogram(bins = 15,
                 linewidth = 0.5,
                 color = 'black', 
                 fill = '#b86fd9') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = 'Histogram of Log-Transformed Response Times', 
       x = "Mean Log RT", 
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# Question 12 -------------------------------------------------------------

# filter only rows for congruent trials, save as new
# data frame

sum_dat %>%
  filter(condition == 'Congruent') -> con_dat

# filter only rows for incongruent trials, save as new
# data frame

sum_dat %>%
  filter(condition == 'Incongruent') -> inc_dat

# summarize the separate congruent and incongruent data frames
# to look at trends in data

summary(con_dat)
summary(inc_dat)

# Question 13 -------------------------------------------------------------

# conduct ANOVA on mean raw RTs with condition as a within-subject
# factor

# specify data

ezANOVA(sum_dat,
        
        # specify dependent variable
        
        dv = rt_mean,
        
        # specify subject ID column
        
        wid = subject,
        
        # specify within-subject factor
        
        within = condition,
        
        # specify detailed, just in case
        
        detailed = TRUE) -> rt_anova

# mean, SD, N, and FLSD by condition for mean raw RTs

ezStats(sum_dat, 
        dv = rt_mean, 
        wid = subject, 
        within = condition) -> rt_stats

# conduct ANOVA on mean log-transformed RTs with condition 
# as a within-subject factor

ezANOVA(sum_dat, 
        dv = log_mean, 
        wid = subject, 
        within = condition, 
        detailed = TRUE) -> log_rt_anova

# mean, SD, N, and FLSD by condition for mean log-transformed rts

ezStats(sum_dat, 
        dv = log_mean, 
        wid = subject, 
        within = condition) -> log_rt_stats

# Question 14 -------------------------------------------------------------


# create plots for raw and log-transformed RTs as a
# function of condition

# raw mean RT plot

# specify data and axes

png('figures/rt_anova_plot.png',
    width = 1024,
    height = 768)

ggplot(rt_stats, aes(x = condition,
                     y = Mean)) + 
  
  # call geom_col function because we don't want counts; this is more
  # efficient than calling geom_bar
  # also specify bin width and palette
  
  geom_col(width = 0.5,
           color = 'black',
           fill = '#b86fd9') +
  
  # add error bars using the SD calculated by ezStats
  
  geom_errorbar(aes(x = condition,
                    
                    # specify min (mean - sd) and max (mean + sd) of error
                    # bars
                    
                    ymin = Mean - SD, 
                    ymax = Mean + SD),
                
                # make the error bars a reasonable size
                
                width = 0.06) +
  
  # use classic theme to remove background
  
  theme_classic() +
  
  # stop columns from floating above X axis
  
  scale_y_continuous(expand = c(0,0),
                     
                     # set a very high limit on the Y axis 
                     # in order to accommodate large SD error bars
                     
                     limits = c(0, 1200),
                     
                     # specify breaks to go with this limit
                     
                     breaks = seq(0, 1200, 200)) +
  
  # add title and axis labels
  
  labs(title = 'Mean Raw Response Times by Condition', 
       x = 'Condition', 
       y = 'Response Time') +
  
  # center the title
  
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# log-transformed rt plot

png('figures/log_rt_anova_plot.png',
    width = 1024,
    height = 768)

ggplot(log_rt_stats, aes(x = condition,
                     y = Mean)) + 
  geom_col(width = 0.5,
           color = 'black',
           fill = '#b86fd9') +
  geom_errorbar(aes(x = condition, 
                    ymin = Mean - SD, 
                    ymax = Mean + SD), 
                width = 0.06) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 10),
                     breaks = seq(0, 10, 2)) +
  labs(title = 'Mean Log-Transformed Response Times by Condition', 
       x = 'Condition', 
       y = 'Response Time') +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

###