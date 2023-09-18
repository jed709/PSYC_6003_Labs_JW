### Load libraries

library(tidyverse)
library(stats)
library(ggplot2)
library(ez)
library(psych)

### Question 4

# define function that accepts 3 numbers as arguments and returns 
# their product if all numbers are even and returns their sum otherwise
 
evenProd = function(x, y, z) {
  
  # check if any arguments are odd numbers using remainders
  
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

### Question 6

# read in stroop data file as dataframe

dat = read.csv('data/stroop_dat.csv')

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
  
  mutate(condition = recode_factor(c, '1' = 'level_1', 
                            '2' = 'level_2')) %>%
  
  # remove old condition column and save as new dataframe
  
  select(-c) -> dat2

### Question 7

dat2 %>%
  
  # create new column with log transformed RT for each row
  
  mutate(log_rt = log(rt),
         
         # create new column wherein responses within time
         # window are coded with 1 and response outside
         # are coded with 0; save as new dataframe
         
         acc = as.numeric(!is.na(rt))) -> dat3


