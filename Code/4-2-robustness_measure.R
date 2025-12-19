### Load packages ##############################################################

  library(dtms)
  library(tidyverse)


### Load data ##################################################################

  load("Data/long_hrs50.Rdata")


### Editing data ###############################################################

  # Rename some of the variables
  hrs <- hrs %>% rename('state' = 'state_adl_sim',
                        'smoke' = 'smok3',
                        'id'    = 'hhidpn')

  # Otherwise similar as for main analysis
  dtms_source(file="Code/2-main_analysis.R",
              start=20,end=58)


### Analysis ###################################################################

  # Source analysis code
  dtms_source(file="Code/2-main_analysis.R",
              start=64,end=795) 


### Compare ####################################################################

  # Copy
  adltable <- restable
  
  # Load reference
  load("Results/tab1.Rds")
  
  # Difference
  difference <- adltable-restable
  
  
### Save #######################################################################
  
  save(list=c("difference","adltable"),
       file="Results/robustness_COVID.Rda")
