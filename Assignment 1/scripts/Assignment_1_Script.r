### Load libraries

library(tidyverse)
library(stats)
library(ggplot2)
library(ez)
library(psych)

### Question 4

# define function that accepts 3 numbers as arguments and returns their product 
# if all numbers are even and returns their sum otherwise
 
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

dat = read.csv('data/stroop_dat.csv')
