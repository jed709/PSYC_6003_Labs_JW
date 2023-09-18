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
  
  if(x %% 2 != 0 | y %% 2 != 0 | z %% 2 != 0) {
    
    return(sum(x, y, z))
    
  }
  else {
    
    return(x * y * z)
    
  }
  
}
