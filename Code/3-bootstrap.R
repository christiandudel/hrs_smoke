### Load packages ##############################################################

  library(dtms)
  library(tidyverse)


### Load & editing data ########################################################

  # File with code for data preparation for main analysis
  dtms_source(file="Code/2-main_analysis.R",
              start=1,end=58)


### Bootstrap function #########################################################

  bootfun <- function(estdata,dtms) {
    
    # Source analysis code
    dtms_source(file="Code/2-main_analysis.R",
                start=64,end=795,
                local=environment()) 
    
    # Return main table
    return(restable)
    
  }
  
  
### Run bootstrap ##############################################################  
  
  bootres <- dtms_boot(data=estdata,
                       dtms=hrsdtms,
                       fun=bootfun,
                       method="block",
                       parallel=TRUE,
                       rep=1000,
                       cores=5,
                       .packages=c("VGAM","dtms","tidyverse"))
  
  
### Save #######################################################################  
  
  save(bootres,file="Results/bootstrap.Rda")
  