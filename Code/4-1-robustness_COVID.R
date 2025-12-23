### Load packages ##############################################################

  library(dtms)
  library(tidyverse)


### Load & editing data ########################################################

  # File with code for data preparation for main analysis
  dtms_source(file="Code/2-main_analysis.R",
              start=1,end=58)


### Drop two most recent waves #################################################

  estdata <- estdata |> filter(wave<=13)


### Analysis ###################################################################

  # Source analysis code
  dtms_source(file="Code/2-main_analysis.R",
              start=64,end=795) 
  

### Compare ####################################################################
  
  # Copy
  newtable <- restable
  
  # Load reference
  load("Results/main.Rda")
  
  # Difference
  difference <- newtable-restable
  
  
### Save #######################################################################
  
  save(difference,file="Results/robustness_COVID.Rda")
  