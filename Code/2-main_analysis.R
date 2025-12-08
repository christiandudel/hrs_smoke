### Load packages ##############################################################

  library(dtms)
  library(tidyverse)
  #library(parallel)
  #library(haven)
  #library(foreach)
  #library(doParallel)
  #library(data.table)
  #'%ni%'<-Negate('%in%')

  
### Load data ##################################################################

  load("Data/long_hrs50.Rdata")

  
### Edit data ##################################################################    

  # Rename some of the variables
  hrs <- hrs %>% rename('state' = 'state_smkd_sim',
                        'smoke' = 'smok3',
                        'id'    = 'hhidpn')

  # Select key variables 
  hrs <- hrs |> select(id,age,gender,smoke,state,race,wave,education)
  
  # Add dummies for threshold age 
  hrs$dum65<-ifelse(hrs$age==65,1,0)
  hrs$dum66<-ifelse(hrs$age==66,1,0)
  hrs$dum67<-ifelse(hrs$age>=67,1,0)
  
  
### Define dtms ################################################################  

  # Transient states
  transientstates <- c("working/healthy","not working/healthy",
                       "working/unhealthy","not working/unhealthy")
  
  # dtms
  hrsdtms <- dtms(transient=transientstates,
                  absorbing="dead",
                  timescale=seq(50,98,1),
                  timestep=1:3)
  

### Reshape ####################################################################
  
  estdata <- dtms_format(data=hrs,
                         dtms=hrsdtms,
                         idvar="id",
                         timevar="age",
                         statevar="state",
                         steplength=TRUE)
  
  
### Cleaning ###################################################################  
  
  estdata <- dtms_clean(data=estdata,
                        dtms=hrsdtms)
  
  
### Edit variables #############################################################
  
  estdata$time2 <- estdata$time^2
  estdata$smoke <- as.factor(estdata$smoke)
  estdata$education <- as.factor(estdata$education)
  
  
### Sample splits ##############################################################
  
  # Gender
  estdata_m <- estdata |> filter(gender==0)
  estdata_w <- estdata |> filter(gender==1)
  
  # Gender & race/ethnicity
  estdata_w_m <- estdata |> filter(race=="White" & gender==0)
  estdata_w_w <- estdata |> filter(race=="White" & gender==1)
  estdata_b_m <- estdata |> filter(race=="Black" & gender==0)
  estdata_b_w <- estdata |> filter(race=="Black" & gender==1)
  estdata_h_m <- estdata |> filter(race=="Hispan" & gender==0)
  estdata_h_w <- estdata |> filter(race=="Hispan" & gender==1)
  
  
### Fit models #################################################################  
  
  # Controls w/o education
  convar <- c("smoke","time","time2",
              "dum65","dum66","dum67",
              "steplength")
  
  # Controls with education
  convaredu <- c("smoke","education","time","time2",
                 "dum65","dum66","dum67",
                 "steplength")
  
  # Basic models without education and race/ethnicity
  fit <- dtms_fit(data=estdata,controls=convar)
  fit_m <- dtms_fit(data=estdata_m,controls=convar)
  fit_w<- dtms_fit(data=estdata_w,controls=convar)
  
  # Models + education + race/ethnicity 
  fit_w_m <- dtms_fit(data=estdata_w_m,controls=convaredu)
  fit_w_w <- dtms_fit(data=estdata_w_w,controls=convaredu)
  fit_b_m <- dtms_fit(data=estdata_b_m,controls=convaredu)
  fit_b_w <- dtms_fit(data=estdata_b_w,controls=convaredu)
  fit_h_m <- dtms_fit(data=estdata_h_m,controls=convaredu)
  fit_h_w <- dtms_fit(data=estdata_h_w,controls=convaredu)
  
  
### Values for prediction ######################################################
  
  # Timescale
  timescale <- seq(50,98,2)
  
  # dtms
  hrspredict <- dtms(transient=transientstates,
                     absorbing="dead",
                     timescale=timescale)
  
  # Timescale, steplength
  reused <- list(time=timescale,
                 time2=timescale^2,
                 dum65=ifelse(timescale==65,1,0),
                 dum66=ifelse(timescale==66,1,0),
                 dum67=ifelse(timescale>=67,1,0),
                 steplength=2)
  
  # Plus smoking status
  smoke0 <- list(smoke=factor("0",levels=c("0","1","2")))
  smoke1 <- list(smoke=factor("1",levels=c("0","1","2")))
  smoke2 <- list(smoke=factor("2",levels=c("0","1","2")))
  
  reused_ns <- c(smoke0,reused) # Never
  reused_ex <- c(smoke1,reused) # Former
  reused_sm <- c(smoke2,reused) # Current
  
  # Plus education
  edu0 <- list(education=factor("0",levels=c("0","1","2")))
  edu1 <- list(education=factor("1",levels=c("0","1","2")))
  edu2 <- list(education=factor("2",levels=c("0","1","2")))
  
  reused_ns_0 <- c(edu0,reused_ns)
  reused_ns_1 <- c(edu1,reused_ns)
  reused_ns_2 <- c(edu2,reused_ns)
  
  reused_ex_0 <- c(edu0,reused_ex)
  reused_ex_1 <- c(edu1,reused_ex)
  reused_ex_2 <- c(edu2,reused_ex)

  reused_sm_0 <- c(edu0,reused_sm)
  reused_sm_1 <- c(edu1,reused_sm)
  reused_sm_2 <- c(edu2,reused_sm)
  
  
### Predict probabilities ######################################################
  
  # Probabilities: full sample 
  probs_ns <- dtms_transitions(model=fit,dtms=hrspredict,controls=reused_ns)
  probs_ex <- dtms_transitions(model=fit,dtms=hrspredict,controls=reused_ex)
  probs_sm <- dtms_transitions(model=fit,dtms=hrspredict,controls=reused_sm)
  
  # Probabilities: gender
  probs_m_ns <- dtms_transitions(model=fit_m,dtms=hrspredict,controls=reused_ns)
  probs_m_ex <- dtms_transitions(model=fit_m,dtms=hrspredict,controls=reused_ex)
  probs_m_sm <- dtms_transitions(model=fit_m,dtms=hrspredict,controls=reused_sm)
  probs_w_ns <- dtms_transitions(model=fit_w,dtms=hrspredict,controls=reused_ns)
  probs_w_ex <- dtms_transitions(model=fit_w,dtms=hrspredict,controls=reused_ex)
  probs_w_sm <- dtms_transitions(model=fit_w,dtms=hrspredict,controls=reused_sm)
  
  # Probabilities: gender + race/ethnicity + education
  probs_w_m_0_ns <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ns_0)
  probs_w_m_0_ex <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ex_0)
  probs_w_m_0_sm <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_sm_0)
  probs_w_m_1_ns <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ns_1)
  probs_w_m_1_ex <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ex_1)
  probs_w_m_1_sm <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_sm_1)
  probs_w_m_2_ns <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ns_2)
  probs_w_m_2_ex <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_ex_2)
  probs_w_m_2_sm <- dtms_transitions(model=fit_w_m,dtms=hrspredict,controls=reused_sm_2)
  
  probs_w_w_0_ns <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ns_0)
  probs_w_w_0_ex <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ex_0)
  probs_w_w_0_sm <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_sm_0)
  probs_w_w_1_ns <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ns_1)
  probs_w_w_1_ex <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ex_1)
  probs_w_w_1_sm <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_sm_1)
  probs_w_w_2_ns <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ns_2)
  probs_w_w_2_ex <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_ex_2)
  probs_w_w_2_sm <- dtms_transitions(model=fit_w_w,dtms=hrspredict,controls=reused_sm_2)
  
  probs_b_m_0_ns <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ns_0)
  probs_b_m_0_ex <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ex_0)
  probs_b_m_0_sm <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_sm_0)
  probs_b_m_1_ns <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ns_1)
  probs_b_m_1_ex <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ex_1)
  probs_b_m_1_sm <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_sm_1)
  probs_b_m_2_ns <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ns_2)
  probs_b_m_2_ex <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_ex_2)
  probs_b_m_2_sm <- dtms_transitions(model=fit_b_m,dtms=hrspredict,controls=reused_sm_2)
  
  probs_b_w_0_ns <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ns_0)
  probs_b_w_0_ex <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ex_0)
  probs_b_w_0_sm <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_sm_0)
  probs_b_w_1_ns <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ns_1)
  probs_b_w_1_ex <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ex_1)
  probs_b_w_1_sm <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_sm_1)
  probs_b_w_2_ns <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ns_2)
  probs_b_w_2_ex <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_ex_2)
  probs_b_w_2_sm <- dtms_transitions(model=fit_b_w,dtms=hrspredict,controls=reused_sm_2)
  
  probs_h_m_0_ns <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ns_0)
  probs_h_m_0_ex <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ex_0)
  probs_h_m_0_sm <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_sm_0)
  probs_h_m_1_ns <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ns_1)
  probs_h_m_1_ex <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ex_1)
  probs_h_m_1_sm <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_sm_1)
  probs_h_m_2_ns <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ns_2)
  probs_h_m_2_ex <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_ex_2)
  probs_h_m_2_sm <- dtms_transitions(model=fit_h_m,dtms=hrspredict,controls=reused_sm_2)
  
  probs_h_w_0_ns <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ns_0)
  probs_h_w_0_ex <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ex_0)
  probs_h_w_0_sm <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_sm_0)
  probs_h_w_1_ns <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ns_1)
  probs_h_w_1_ex <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ex_1)
  probs_h_w_1_sm <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_sm_1)
  probs_h_w_2_ns <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ns_2)
  probs_h_w_2_ex <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_ex_2)
  probs_h_w_2_sm <- dtms_transitions(model=fit_h_w,dtms=hrspredict,controls=reused_sm_2)
  
  # Transition matrix: total population
  tmat_ns <- dtms_matrix(probs=probs_ns,dtms=hrspredict)
  tmat_ex <- dtms_matrix(probs=probs_ex,dtms=hrspredict)
  tmat_sm <- dtms_matrix(probs=probs_sm,dtms=hrspredict)
  
  # Transition matrix: gender
  tmat_m_ns <- dtms_matrix(probs=probs_m_ns,dtms=hrspredict)
  tmat_m_ex <- dtms_matrix(probs=probs_m_ex,dtms=hrspredict)
  tmat_m_sm <- dtms_matrix(probs=probs_m_sm,dtms=hrspredict)
  
  tmat_w_ns <- dtms_matrix(probs=probs_w_ns,dtms=hrspredict)
  tmat_w_ex <- dtms_matrix(probs=probs_w_ex,dtms=hrspredict)
  tmat_w_sm <- dtms_matrix(probs=probs_w_sm,dtms=hrspredict)
  
  # Transition matrix: gender + education + race/ethnicity
  tmat_w_m_0_ns <- dtms_matrix(probs=probs_w_m_0_ns,dtms=hrspredict)
  tmat_w_m_0_ex <- dtms_matrix(probs=probs_w_m_0_ex,dtms=hrspredict)
  tmat_w_m_0_sm <- dtms_matrix(probs=probs_w_m_0_sm,dtms=hrspredict)
  tmat_w_m_1_ns <- dtms_matrix(probs=probs_w_m_1_ns,dtms=hrspredict)
  tmat_w_m_1_ex <- dtms_matrix(probs=probs_w_m_1_ex,dtms=hrspredict)
  tmat_w_m_1_sm <- dtms_matrix(probs=probs_w_m_1_sm,dtms=hrspredict)
  tmat_w_m_2_ns <- dtms_matrix(probs=probs_w_m_2_ns,dtms=hrspredict)
  tmat_w_m_2_ex <- dtms_matrix(probs=probs_w_m_2_ex,dtms=hrspredict)
  tmat_w_m_2_sm <- dtms_matrix(probs=probs_w_m_2_sm,dtms=hrspredict)
  
  tmat_w_w_0_ns <- dtms_matrix(probs=probs_w_w_0_ns,dtms=hrspredict)
  tmat_w_w_0_ex <- dtms_matrix(probs=probs_w_w_0_ex,dtms=hrspredict)
  tmat_w_w_0_sm <- dtms_matrix(probs=probs_w_w_0_sm,dtms=hrspredict)
  tmat_w_w_1_ns <- dtms_matrix(probs=probs_w_w_1_ns,dtms=hrspredict)
  tmat_w_w_1_ex <- dtms_matrix(probs=probs_w_w_1_ex,dtms=hrspredict)
  tmat_w_w_1_sm <- dtms_matrix(probs=probs_w_w_1_sm,dtms=hrspredict)
  tmat_w_w_2_ns <- dtms_matrix(probs=probs_w_w_2_ns,dtms=hrspredict)
  tmat_w_w_2_ex <- dtms_matrix(probs=probs_w_w_2_ex,dtms=hrspredict)
  tmat_w_w_2_sm <- dtms_matrix(probs=probs_w_w_2_sm,dtms=hrspredict)
  
  tmat_b_m_0_ns <- dtms_matrix(probs=probs_b_m_0_ns,dtms=hrspredict)
  tmat_b_m_0_ex <- dtms_matrix(probs=probs_b_m_0_ex,dtms=hrspredict)
  tmat_b_m_0_sm <- dtms_matrix(probs=probs_b_m_0_sm,dtms=hrspredict)
  tmat_b_m_1_ns <- dtms_matrix(probs=probs_b_m_1_ns,dtms=hrspredict)
  tmat_b_m_1_ex <- dtms_matrix(probs=probs_b_m_1_ex,dtms=hrspredict)
  tmat_b_m_1_sm <- dtms_matrix(probs=probs_b_m_1_sm,dtms=hrspredict)
  tmat_b_m_2_ns <- dtms_matrix(probs=probs_b_m_2_ns,dtms=hrspredict)
  tmat_b_m_2_ex <- dtms_matrix(probs=probs_b_m_2_ex,dtms=hrspredict)
  tmat_b_m_2_sm <- dtms_matrix(probs=probs_b_m_2_sm,dtms=hrspredict)
  
  tmat_b_w_0_ns <- dtms_matrix(probs=probs_b_w_0_ns,dtms=hrspredict)
  tmat_b_w_0_ex <- dtms_matrix(probs=probs_b_w_0_ex,dtms=hrspredict)
  tmat_b_w_0_sm <- dtms_matrix(probs=probs_b_w_0_sm,dtms=hrspredict)
  tmat_b_w_1_ns <- dtms_matrix(probs=probs_b_w_1_ns,dtms=hrspredict)
  tmat_b_w_1_ex <- dtms_matrix(probs=probs_b_w_1_ex,dtms=hrspredict)
  tmat_b_w_1_sm <- dtms_matrix(probs=probs_b_w_1_sm,dtms=hrspredict)
  tmat_b_w_2_ns <- dtms_matrix(probs=probs_b_w_2_ns,dtms=hrspredict)
  tmat_b_w_2_ex <- dtms_matrix(probs=probs_b_w_2_ex,dtms=hrspredict)
  tmat_b_w_2_sm <- dtms_matrix(probs=probs_b_w_2_sm,dtms=hrspredict)

  tmat_h_m_0_ns <- dtms_matrix(probs=probs_h_m_0_ns,dtms=hrspredict)
  tmat_h_m_0_ex <- dtms_matrix(probs=probs_h_m_0_ex,dtms=hrspredict)
  tmat_h_m_0_sm <- dtms_matrix(probs=probs_h_m_0_sm,dtms=hrspredict)
  tmat_h_m_1_ns <- dtms_matrix(probs=probs_h_m_1_ns,dtms=hrspredict)
  tmat_h_m_1_ex <- dtms_matrix(probs=probs_h_m_1_ex,dtms=hrspredict)
  tmat_h_m_1_sm <- dtms_matrix(probs=probs_h_m_1_sm,dtms=hrspredict)
  tmat_h_m_2_ns <- dtms_matrix(probs=probs_h_m_2_ns,dtms=hrspredict)
  tmat_h_m_2_ex <- dtms_matrix(probs=probs_h_m_2_ex,dtms=hrspredict)
  tmat_h_m_2_sm <- dtms_matrix(probs=probs_h_m_2_sm,dtms=hrspredict)
  
  tmat_h_w_0_ns <- dtms_matrix(probs=probs_h_w_0_ns,dtms=hrspredict)
  tmat_h_w_0_ex <- dtms_matrix(probs=probs_h_w_0_ex,dtms=hrspredict)
  tmat_h_w_0_sm <- dtms_matrix(probs=probs_h_w_0_sm,dtms=hrspredict)
  tmat_h_w_1_ns <- dtms_matrix(probs=probs_h_w_1_ns,dtms=hrspredict)
  tmat_h_w_1_ex <- dtms_matrix(probs=probs_h_w_1_ex,dtms=hrspredict)
  tmat_h_w_1_sm <- dtms_matrix(probs=probs_h_w_1_sm,dtms=hrspredict)
  tmat_h_w_2_ns <- dtms_matrix(probs=probs_h_w_2_ns,dtms=hrspredict)
  tmat_h_w_2_ex <- dtms_matrix(probs=probs_h_w_2_ex,dtms=hrspredict)
  tmat_h_w_2_sm <- dtms_matrix(probs=probs_h_w_2_sm,dtms=hrspredict)
  
  
### Starting distribution ######################################################
  
  # General sample
  start_ns <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=0))
  start_ex <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=1))
  start_sm <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=2))
  
  # Gender
  start_m_ns <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=0))
  start_m_ex <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=1))
  start_m_sm <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=2))

  start_w_ns <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=0))
  start_w_ex <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=1))
  start_w_sm <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=2))
  
  # Gender + education + race/ethnicity 
  start_w_m_0_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_w_m_0_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_w_m_0_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_w_m_1_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_w_m_1_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_w_m_1_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_w_m_2_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_w_m_2_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_w_m_2_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  start_w_w_0_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_w_w_0_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_w_w_0_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_w_w_1_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_w_w_1_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_w_w_1_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_w_w_2_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_w_w_2_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_w_w_2_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  start_b_m_0_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_b_m_0_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_b_m_0_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_b_m_1_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_b_m_1_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_b_m_1_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_b_m_2_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_b_m_2_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_b_m_2_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  start_b_w_0_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_b_w_0_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_b_w_0_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_b_w_1_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_b_w_1_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_b_w_1_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_b_w_2_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_b_w_2_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_b_w_2_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  start_h_m_0_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_h_m_0_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_h_m_0_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_h_m_1_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_h_m_1_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_h_m_1_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_h_m_2_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_h_m_2_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_h_m_2_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  start_h_w_0_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=0))
  start_h_w_0_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=0))
  start_h_w_0_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=0))
  start_h_w_1_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=1))
  start_h_w_1_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=1))
  start_h_w_1_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=1))
  start_h_w_2_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=2))
  start_h_w_2_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=2))
  start_h_w_2_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=2))
  
  
### Expectancies ###############################################################  
  
  # General population
  hwle_ns <- dtms_expectancy(matrix=tmat_ns,start_distr=start_ns,dtms=hrspredict)
  hwle_ex <- dtms_expectancy(matrix=tmat_ex,start_distr=start_ex,dtms=hrspredict)
  hwle_sm <- dtms_expectancy(matrix=tmat_sm,start_distr=start_sm,dtms=hrspredict)
  
  # Gender
  hwle_m_ns <- dtms_expectancy(matrix=tmat_m_ns,start_distr=start_m_ns,dtms=hrspredict)
  hwle_m_ex <- dtms_expectancy(matrix=tmat_m_ex,start_distr=start_m_ex,dtms=hrspredict)
  hwle_m_sm <- dtms_expectancy(matrix=tmat_m_sm,start_distr=start_m_sm,dtms=hrspredict)
  hwle_w_ns <- dtms_expectancy(matrix=tmat_w_ns,start_distr=start_w_ns,dtms=hrspredict)
  hwle_w_ex <- dtms_expectancy(matrix=tmat_w_ex,start_distr=start_w_ex,dtms=hrspredict)
  hwle_w_sm <- dtms_expectancy(matrix=tmat_w_sm,start_distr=start_w_sm,dtms=hrspredict)
  
  # Gender + education + race/ethnicity 
  hwle_w_m_0_ns <- dtms_expectancy(matrix=tmat_w_m_0_ns,start_distr=start_w_m_0_ns,dtms=hrspredict)
  hwle_w_m_0_ex <- dtms_expectancy(matrix=tmat_w_m_0_ex,start_distr=start_w_m_0_ex,dtms=hrspredict)
  hwle_w_m_0_sm <- dtms_expectancy(matrix=tmat_w_m_0_sm,start_distr=start_w_m_0_sm,dtms=hrspredict)
  hwle_w_m_1_ns <- dtms_expectancy(matrix=tmat_w_m_1_ns,start_distr=start_w_m_1_ns,dtms=hrspredict)
  hwle_w_m_1_ex <- dtms_expectancy(matrix=tmat_w_m_1_ex,start_distr=start_w_m_1_ex,dtms=hrspredict)
  hwle_w_m_1_sm <- dtms_expectancy(matrix=tmat_w_m_1_sm,start_distr=start_w_m_1_sm,dtms=hrspredict)
  hwle_w_m_2_ns <- dtms_expectancy(matrix=tmat_w_m_2_ns,start_distr=start_w_m_2_ns,dtms=hrspredict)
  hwle_w_m_2_ex <- dtms_expectancy(matrix=tmat_w_m_2_ex,start_distr=start_w_m_2_ex,dtms=hrspredict)
  hwle_w_m_2_sm <- dtms_expectancy(matrix=tmat_w_m_2_sm,start_distr=start_w_m_2_sm,dtms=hrspredict)
  
  hwle_w_w_0_ns <- dtms_expectancy(matrix=tmat_w_w_0_ns,start_distr=start_w_w_0_ns,dtms=hrspredict)
  hwle_w_w_0_ex <- dtms_expectancy(matrix=tmat_w_w_0_ex,start_distr=start_w_w_0_ex,dtms=hrspredict)
  hwle_w_w_0_sm <- dtms_expectancy(matrix=tmat_w_w_0_sm,start_distr=start_w_w_0_sm,dtms=hrspredict)
  hwle_w_w_1_ns <- dtms_expectancy(matrix=tmat_w_w_1_ns,start_distr=start_w_w_1_ns,dtms=hrspredict)
  hwle_w_w_1_ex <- dtms_expectancy(matrix=tmat_w_w_1_ex,start_distr=start_w_w_1_ex,dtms=hrspredict)
  hwle_w_w_1_sm <- dtms_expectancy(matrix=tmat_w_w_1_sm,start_distr=start_w_w_1_sm,dtms=hrspredict)
  hwle_w_w_2_ns <- dtms_expectancy(matrix=tmat_w_w_2_ns,start_distr=start_w_w_2_ns,dtms=hrspredict)
  hwle_w_w_2_ex <- dtms_expectancy(matrix=tmat_w_w_2_ex,start_distr=start_w_w_2_ex,dtms=hrspredict)
  hwle_w_w_2_sm <- dtms_expectancy(matrix=tmat_w_w_2_sm,start_distr=start_w_w_2_sm,dtms=hrspredict)
  
  hwle_b_m_0_ns <- dtms_expectancy(matrix=tmat_b_m_0_ns,start_distr=start_b_m_0_ns,dtms=hrspredict)
  hwle_b_m_0_ex <- dtms_expectancy(matrix=tmat_b_m_0_ex,start_distr=start_b_m_0_ex,dtms=hrspredict)
  hwle_b_m_0_sm <- dtms_expectancy(matrix=tmat_b_m_0_sm,start_distr=start_b_m_0_sm,dtms=hrspredict)
  hwle_b_m_1_ns <- dtms_expectancy(matrix=tmat_b_m_1_ns,start_distr=start_b_m_1_ns,dtms=hrspredict)
  hwle_b_m_1_ex <- dtms_expectancy(matrix=tmat_b_m_1_ex,start_distr=start_b_m_1_ex,dtms=hrspredict)
  hwle_b_m_1_sm <- dtms_expectancy(matrix=tmat_b_m_1_sm,start_distr=start_b_m_1_sm,dtms=hrspredict)
  hwle_b_m_2_ns <- dtms_expectancy(matrix=tmat_b_m_2_ns,start_distr=start_b_m_2_ns,dtms=hrspredict)
  hwle_b_m_2_ex <- dtms_expectancy(matrix=tmat_b_m_2_ex,start_distr=start_b_m_2_ex,dtms=hrspredict)
  hwle_b_m_2_sm <- dtms_expectancy(matrix=tmat_b_m_2_sm,start_distr=start_b_m_2_sm,dtms=hrspredict)
  
  hwle_b_w_0_ns <- dtms_expectancy(matrix=tmat_b_w_0_ns,start_distr=start_b_w_0_ns,dtms=hrspredict)
  hwle_b_w_0_ex <- dtms_expectancy(matrix=tmat_b_w_0_ex,start_distr=start_b_w_0_ex,dtms=hrspredict)
  hwle_b_w_0_sm <- dtms_expectancy(matrix=tmat_b_w_0_sm,start_distr=start_b_w_0_sm,dtms=hrspredict)
  hwle_b_w_1_ns <- dtms_expectancy(matrix=tmat_b_w_1_ns,start_distr=start_b_w_1_ns,dtms=hrspredict)
  hwle_b_w_1_ex <- dtms_expectancy(matrix=tmat_b_w_1_ex,start_distr=start_b_w_1_ex,dtms=hrspredict)
  hwle_b_w_1_sm <- dtms_expectancy(matrix=tmat_b_w_1_sm,start_distr=start_b_w_1_sm,dtms=hrspredict)
  hwle_b_w_2_ns <- dtms_expectancy(matrix=tmat_b_w_2_ns,start_distr=start_b_w_2_ns,dtms=hrspredict)
  hwle_b_w_2_ex <- dtms_expectancy(matrix=tmat_b_w_2_ex,start_distr=start_b_w_2_ex,dtms=hrspredict)
  hwle_b_w_2_sm <- dtms_expectancy(matrix=tmat_b_w_2_sm,start_distr=start_b_w_2_sm,dtms=hrspredict)
  
  hwle_h_m_0_ns <- dtms_expectancy(matrix=tmat_h_m_0_ns,start_distr=start_h_m_0_ns,dtms=hrspredict)
  hwle_h_m_0_ex <- dtms_expectancy(matrix=tmat_h_m_0_ex,start_distr=start_h_m_0_ex,dtms=hrspredict)
  hwle_h_m_0_sm <- dtms_expectancy(matrix=tmat_h_m_0_sm,start_distr=start_h_m_0_sm,dtms=hrspredict)
  hwle_h_m_1_ns <- dtms_expectancy(matrix=tmat_h_m_1_ns,start_distr=start_h_m_1_ns,dtms=hrspredict)
  hwle_h_m_1_ex <- dtms_expectancy(matrix=tmat_h_m_1_ex,start_distr=start_h_m_1_ex,dtms=hrspredict)
  hwle_h_m_1_sm <- dtms_expectancy(matrix=tmat_h_m_1_sm,start_distr=start_h_m_1_sm,dtms=hrspredict)
  hwle_h_m_2_ns <- dtms_expectancy(matrix=tmat_h_m_2_ns,start_distr=start_h_m_2_ns,dtms=hrspredict)
  hwle_h_m_2_ex <- dtms_expectancy(matrix=tmat_h_m_2_ex,start_distr=start_h_m_2_ex,dtms=hrspredict)
  hwle_h_m_2_sm <- dtms_expectancy(matrix=tmat_h_m_2_sm,start_distr=start_h_m_2_sm,dtms=hrspredict)
  
  hwle_h_w_0_ns <- dtms_expectancy(matrix=tmat_h_w_0_ns,start_distr=start_h_w_0_ns,dtms=hrspredict)
  hwle_h_w_0_ex <- dtms_expectancy(matrix=tmat_h_w_0_ex,start_distr=start_h_w_0_ex,dtms=hrspredict)
  hwle_h_w_0_sm <- dtms_expectancy(matrix=tmat_h_w_0_sm,start_distr=start_h_w_0_sm,dtms=hrspredict)
  hwle_h_w_1_ns <- dtms_expectancy(matrix=tmat_h_w_1_ns,start_distr=start_h_w_1_ns,dtms=hrspredict)
  hwle_h_w_1_ex <- dtms_expectancy(matrix=tmat_h_w_1_ex,start_distr=start_h_w_1_ex,dtms=hrspredict)
  hwle_h_w_1_sm <- dtms_expectancy(matrix=tmat_h_w_1_sm,start_distr=start_h_w_1_sm,dtms=hrspredict)
  hwle_h_w_2_ns <- dtms_expectancy(matrix=tmat_h_w_2_ns,start_distr=start_h_w_2_ns,dtms=hrspredict)
  hwle_h_w_2_ex <- dtms_expectancy(matrix=tmat_h_w_2_ex,start_distr=start_h_w_2_ex,dtms=hrspredict)
  hwle_h_w_2_sm <- dtms_expectancy(matrix=tmat_h_w_2_sm,start_distr=start_h_w_2_sm,dtms=hrspredict)
  
### Aggregate ##################################################################
  
  table(estdata_m$education,estdata_m$race)
  
### Group differences ##########################################################  
  
################################################################################  
################################################################################
################################################################################





###### BOOTFUN #######################################################################################################
hrsdtms<-dtms(transient=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy"),
              absorbing="dead",
              timescale=seq(50,98,2))

bootfun <- function(data,dtms) {
  
  controls<-c("time","time2","smoke","dum65","dum66","dum67","steplength")
  fit <- dtms_fit(data=data,controls=controls)
  
  reused <- list(time=seq(min(dtms$timescale),98,2),time2=seq(min(dtms$timescale),98,2)^2,
                 dum65=ifelse(seq(50,98,2)==65,1,0),
                 dum66=ifelse(seq(50,98,2)==66,1,0),
                 dum67=ifelse(seq(50,98,2)>=67,1,0),
                 steplength=2)
  
  probs_ns    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(smoke=factor("0",levels=c("0","1","2")))))
  probs_ex    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(smoke=factor("1",levels=c("0","1","2")))))
  probs_sm    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(smoke=factor("2",levels=c("0","1","2")))))
  Tp_ns <- dtms_matrix(dtms=dtms, probs=probs_ns)
  Tp_ex <- dtms_matrix(dtms=dtms, probs=probs_ex)
  Tp_sm <- dtms_matrix(dtms=dtms, probs=probs_sm)
  S_ns <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(smoke=factor("0",levels=c("0","1","2"))))
  S_ex <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(smoke=factor("1",levels=c("0","1","2"))))
  S_sm <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(smoke=factor("2",levels=c("0","1","2"))))
  
  rbind(
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ns,start_distr=S_ns),smoking=rep(0,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ex,start_distr=S_ex),smoking=rep(1,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_sm,start_distr=S_sm),smoking=rep(2,5)))
}

bootfun_educ <- function(data,dtms) {
  
  controls<-c("time","time2","smoke","education","dum65","dum66","dum67","steplength")
  fit <- dtms_fit(data=data,controls=controls)
  
  reused <- list(time=seq(min(dtms$timescale),98,2),time2=seq(min(dtms$timescale),98,2)^2,
                 dum65=ifelse(seq(50,98,2)==65,1,0),
                 dum66=ifelse(seq(50,98,2)==66,1,0),
                 dum67=ifelse(seq(50,98,2)>=67,1,0),
                 steplength=2)
  
  probs_ns_h    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("0",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2")))))
  probs_ex_h    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("0",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2")))))
  probs_sm_h    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("0",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2")))))
  probs_ns_m    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("1",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2")))))
  probs_ex_m    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("1",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2")))))
  probs_sm_m    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("1",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2")))))
  probs_ns_l    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("2",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2")))))
  probs_ex_l    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("2",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2")))))
  probs_sm_l    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused,list(education=factor("2",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2")))))
  Tp_ns_h <- dtms_matrix(dtms=dtms, probs=probs_ns_h)
  Tp_ex_h <- dtms_matrix(dtms=dtms, probs=probs_ex_h)
  Tp_sm_h <- dtms_matrix(dtms=dtms, probs=probs_sm_h)
  Tp_ns_m <- dtms_matrix(dtms=dtms, probs=probs_ns_m)
  Tp_ex_m <- dtms_matrix(dtms=dtms, probs=probs_ex_m)
  Tp_sm_m <- dtms_matrix(dtms=dtms, probs=probs_sm_m)
  Tp_ns_l <- dtms_matrix(dtms=dtms, probs=probs_ns_l)
  Tp_ex_l <- dtms_matrix(dtms=dtms, probs=probs_ex_l)
  Tp_sm_l <- dtms_matrix(dtms=dtms, probs=probs_sm_l)
  S_ns_h <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("0",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2"))))
  S_ex_h <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("0",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2"))))
  S_sm_h <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("0",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2"))))
  S_ns_m <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("1",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2"))))
  S_ex_m <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("1",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2"))))
  S_sm_m <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("1",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2"))))
  S_ns_l <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("2",levels=c("0","1","2")),smoke=factor("0",levels=c("0","1","2"))))
  S_ex_l <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("2",levels=c("0","1","2")),smoke=factor("1",levels=c("0","1","2"))))
  S_sm_l <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(education=factor("2",levels=c("0","1","2")),smoke=factor("2",levels=c("0","1","2"))))
  
  rbind(
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ns_h,start_distr=S_ns_h),smoking=rep(0,5),education=rep(0,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ex_h,start_distr=S_ex_h),smoking=rep(1,5),education=rep(0,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_sm_h,start_distr=S_sm_h),smoking=rep(2,5),education=rep(0,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ns_m,start_distr=S_ns_m),smoking=rep(0,5),education=rep(1,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ex_m,start_distr=S_ex_m),smoking=rep(1,5),education=rep(1,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_sm_m,start_distr=S_sm_m),smoking=rep(2,5),education=rep(1,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ns_l,start_distr=S_ns_l),smoking=rep(0,5),education=rep(2,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ex_l,start_distr=S_ex_l),smoking=rep(1,5),education=rep(2,5)),
    data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_sm_l,start_distr=S_sm_l),smoking=rep(2,5),education=rep(2,5)))
}

bootfun_nosm <- function(data,dtms) {
  
  controls<-c("time","time2","dum65","dum66","dum67","steplength")
  fit <- dtms_fit(data=data,controls=controls)
  
  reused <- list(time=seq(min(dtms$timescale),98,2),time2=seq(min(dtms$timescale),98,2)^2,
                 dum65=ifelse(seq(50,98,2)==65,1,0),
                 dum66=ifelse(seq(50,98,2)==66,1,0),
                 dum67=ifelse(seq(50,98,2)>=67,1,0),
                 steplength=2)
  
  probs_ov    <- dtms_transitions(dtms=dtms,model = fit,controls=c(reused))
  Tp_ov <- dtms_matrix(dtms=dtms, probs=probs_ov)
  S_ov <- dtms_start(dtms=dtms,data=data,start_time=c(min(dtms$timescale):(min(dtms$timescale)+6)),variables = list(smoke=factor("0",levels=c("0","1","2"))))

  data.frame(dtms_expectancy(dtms=dtms,matrix=Tp_ov,start_distr=S_ov))
}


###### Expectancies Males #######
res_forward_m_smoking<-data.frame(bootfun(estdata_forward_m,dtms=hrsdtms))
res_forward_m.white_smoking<-data.frame(bootfun(estdata_forward_m.white,dtms=hrsdtms))
res_forward_m.black_smoking<-data.frame(bootfun(estdata_forward_m.black,dtms=hrsdtms))
res_forward_m.hispan_smoking<-data.frame(bootfun(estdata_forward_m.hispan,dtms=hrsdtms))

res_forward_m_smoking_educ<-data.frame(bootfun_educ(estdata_forward_m,dtms=hrsdtms))
res_forward_m.white_smoking_educ<-data.frame(bootfun_educ(estdata_forward_m.white,dtms=hrsdtms))
res_forward_m.black_smoking_educ<-data.frame(bootfun_educ(estdata_forward_m.black,dtms=hrsdtms))
res_forward_m.hispan_smoking_educ<-data.frame(bootfun_educ(estdata_forward_m.hispan,dtms=hrsdtms))

###### Expectancies Females #######
res_forward_f_smoking<-data.frame(bootfun(estdata_forward_f,dtms=hrsdtms))
res_forward_f.white_smoking<-data.frame(bootfun(estdata_forward_f.white,dtms=hrsdtms))
res_forward_f.black_smoking<-data.frame(bootfun(estdata_forward_f.black,dtms=hrsdtms))
res_forward_f.hispan_smoking<-data.frame(bootfun(estdata_forward_f.hispan,dtms=hrsdtms))

res_forward_f_smoking_educ<-data.frame(bootfun_educ(estdata_forward_f,dtms=hrsdtms))
res_forward_f.white_smoking_educ<-data.frame(bootfun_educ(estdata_forward_f.white,dtms=hrsdtms))
res_forward_f.black_smoking_educ<-data.frame(bootfun_educ(estdata_forward_f.black,dtms=hrsdtms))
res_forward_f.hispan_smoking_educ<-data.frame(bootfun_educ(estdata_forward_f.hispan,dtms=hrsdtms))

###### CIs Males #######
N.iter<-1000
ci_m_smoking<-summary(dtms_boot(data = estdata_forward_m, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_m.white_smoking<-summary(dtms_boot(data = estdata_forward_m.white, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_m.black_smoking<-summary(dtms_boot(data = estdata_forward_m.black, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_m.hispan_smoking<-summary(dtms_boot(data = estdata_forward_m.hispan, dtms = hrsdtms, fun = bootfun, rep = N.iter))

ci_m_smoking_educ<-summary(dtms_boot(data = estdata_forward_m, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_m.white_smoking_educ<-summary(dtms_boot(data = estdata_forward_m.white, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_m.black_smoking_educ<-summary(dtms_boot(data = estdata_forward_m.black, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_m.hispan_smoking_educ<-summary(dtms_boot(data = estdata_forward_m.hispan, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))

res.ci_m_smoking_race<-rbind(
  cbind(res_forward_m_smoking,c1=ci_m_smoking$`2.5%`,c2=ci_m_smoking$`97.5%`,Race=rep("Total",nrow(res_forward_m_smoking))),
  cbind(res_forward_m.white_smoking,c1=ci_m.white_smoking$`2.5%`,c2=ci_m.white_smoking$`97.5%`,Race=rep("White",nrow(res_forward_m.white_smoking))),
  cbind(res_forward_m.black_smoking,c1=ci_m.black_smoking$`2.5%`,c2=ci_m.black_smoking$`97.5%`,Race=rep("Black",nrow(res_forward_m.black_smoking))),
  cbind(res_forward_m.hispan_smoking,c1=ci_m.hispan_smoking$`2.5%`,c2=ci_m.hispan_smoking$`97.5%`,Race=rep("Hispan",nrow(res_forward_m.hispan_smoking)))
)

res.ci_m_smoking_educ_race<-rbind(
  cbind(res_forward_m_smoking_educ,c1=ci_m_smoking_educ$`2.5%`,c2=ci_m_smoking_educ$`97.5%`,Race=rep("Total",nrow(res_forward_m_smoking_educ))),
  cbind(res_forward_m.white_smoking_educ,c1=ci_m.white_smoking_educ$`2.5%`,c2=ci_m.white_smoking_educ$`97.5%`,Race=rep("White",nrow(res_forward_m.white_smoking_educ))),
  cbind(res_forward_m.black_smoking_educ,c1=ci_m.black_smoking_educ$`2.5%`,c2=ci_m.black_smoking_educ$`97.5%`,Race=rep("Black",nrow(res_forward_m.black_smoking_educ))),
  cbind(res_forward_m.hispan_smoking_educ,c1=ci_m.hispan_smoking_educ$`2.5%`,c2=ci_m.hispan_smoking_educ$`97.5%`,Race=rep("Hispan",nrow(res_forward_m.hispan_smoking_educ)))
)

res.ci_m_smoking_race$start<-rep(c("working/healthy","not working/healthy",
                                   "working/unhealthy","not working/unhealthy","AVERAGE"),nrow(res.ci_m_smoking_race)/5)
res.ci_m_smoking_educ_race$start<-rep(c("working/healthy","not working/healthy",
                                        "working/unhealthy","not working/unhealthy","AVERAGE"),nrow(res.ci_m_smoking_educ_race)/5)

# save(res.ci_m_smoking_race,file="/Users/alessandroferaldi/Desktop/res.ci_m_smoking_race_diab.Rdata")
# save(res.ci_m_smoking_educ_race,file="/Users/alessandroferaldi/Desktop/res.ci_m_smoking_educ_race_diab.Rdata")

# load(file="/Users/alessandroferaldi/Desktop/res.ci_m_smoking_race_diab.Rdata") # res.ci_m_smoking_race
# load(file="/Users/alessandroferaldi/Desktop/res.ci_m_smoking_educ_race_diab.Rdata") # res.ci_m_smoking_educ_race

###### CIs Females #######
N.iter<-1000
ci_f_smoking<-summary(dtms_boot(data = estdata_forward_f, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_f.white_smoking<-summary(dtms_boot(data = estdata_forward_f.white, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_f.black_smoking<-summary(dtms_boot(data = estdata_forward_f.black, dtms = hrsdtms, fun = bootfun, rep = N.iter))
ci_f.hispan_smoking<-summary(dtms_boot(data = estdata_forward_f.hispan, dtms = hrsdtms, fun = bootfun, rep = N.iter))

ci_f_smoking_educ<-summary(dtms_boot(data = estdata_forward_f, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_f.white_smoking_educ<-summary(dtms_boot(data = estdata_forward_f.white, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_f.black_smoking_educ<-summary(dtms_boot(data = estdata_forward_f.black, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))
ci_f.hispan_smoking_educ<-summary(dtms_boot(data = estdata_forward_f.hispan, dtms = hrsdtms, fun = bootfun_educ, rep = N.iter))

res.ci_f_smoking_race<-rbind(
  cbind(res_forward_f_smoking,c1=ci_f_smoking$`2.5%`,c2=ci_f_smoking$`97.5%`,Race=rep("Total",nrow(res_forward_f_smoking))),
  cbind(res_forward_f.white_smoking,c1=ci_f.white_smoking$`2.5%`,c2=ci_f.white_smoking$`97.5%`,Race=rep("White",nrow(res_forward_f.white_smoking))),
  cbind(res_forward_f.black_smoking,c1=ci_f.black_smoking$`2.5%`,c2=ci_f.black_smoking$`97.5%`,Race=rep("Black",nrow(res_forward_f.black_smoking))),
  cbind(res_forward_f.hispan_smoking,c1=ci_f.hispan_smoking$`2.5%`,c2=ci_f.hispan_smoking$`97.5%`,Race=rep("Hispan",nrow(res_forward_f.hispan_smoking)))
)

res.ci_f_smoking_educ_race<-rbind(
  cbind(res_forward_f_smoking_educ,c1=ci_f_smoking_educ$`2.5%`,c2=ci_f_smoking_educ$`97.5%`,Race=rep("Total",nrow(res_forward_f_smoking_educ))),
  cbind(res_forward_f.white_smoking_educ,c1=ci_f.white_smoking_educ$`2.5%`,c2=ci_f.white_smoking_educ$`97.5%`,Race=rep("White",nrow(res_forward_f.white_smoking_educ))),
  cbind(res_forward_f.black_smoking_educ,c1=ci_f.black_smoking_educ$`2.5%`,c2=ci_f.black_smoking_educ$`97.5%`,Race=rep("Black",nrow(res_forward_f.black_smoking_educ))),
  cbind(res_forward_f.hispan_smoking_educ,c1=ci_f.hispan_smoking_educ$`2.5%`,c2=ci_f.hispan_smoking_educ$`97.5%`,Race=rep("Hispan",nrow(res_forward_f.hispan_smoking_educ)))
)

res.ci_f_smoking_race$start<-rep(c("working/healthy","not working/healthy",
                                   "working/unhealthy","not working/unhealthy","AVERAGE"),nrow(res.ci_f_smoking_race)/5)
res.ci_f_smoking_educ_race$start<-rep(c("working/healthy","not working/healthy",
                                        "working/unhealthy","not working/unhealthy","AVERAGE"),nrow(res.ci_f_smoking_educ_race)/5)


# save(res.ci_f_smoking_race,file="/Users/alessandroferaldi/Desktop/res.ci_f_smoking_race_diab.Rdata")
# save(res.ci_f_smoking_educ_race,file="/Users/alessandroferaldi/Desktop/res.ci_f_smoking_educ_race_diab.Rdata")

# load(file="/Users/alessandroferaldi/Desktop/res.ci_f_smoking_race_diab.Rdata") # res.ci_f_smoking_race
# load(file="/Users/alessandroferaldi/Desktop/res.ci_f_smoking_educ_race_diab.Rdata") # res.ci_f_smoking_educ_race




###### Table 2 ######
res.ci_m_smoking_race |> 
  filter(start == "AVERAGE" & Race=="Total") %>% select(smoking,Race,working.healthy,c1.working.healthy,c2.working.healthy,
                                                        not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
                                                        working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
                                                        not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
                                                        TOTAL,c1.TOTAL,c2.TOTAL)
res.ci_f_smoking_race |> 
  filter(start == "AVERAGE" & Race=="Total") %>% select(smoking,Race,working.healthy,c1.working.healthy,c2.working.healthy,
                                                        not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
                                                        working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
                                                        not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
                                                        TOTAL,c1.TOTAL,c2.TOTAL)

###### Figure 3 and Tables Males Appendix ############
le_male<-res.ci_m_smoking_educ_race |> 
  filter(start == "AVERAGE" & Race!="Total") |> 
  mutate(
    Race = factor(Race, levels = c("White", "Black", "Hispan"), 
                  labels = c("White", "Black", "Hispanic")),
    EduLevel = factor(education, levels = c(2, 1, 0), 
                      labels = c("Low education", "Medium education", "High education")),
    SmokingStatus = factor(smoking, levels = c(0, 1, 2), 
                           labels = c("Never", "Ex", "Current")))

ggplot(le_male, aes(x = Race, y = working.healthy, color = SmokingStatus)) +
  geom_hline(yintercept = seq(0, 14, 2), linetype = "dashed", color = "grey85") +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(aes(ymin = c1.working.healthy, ymax = c2.working.healthy), 
                width = 0.4, position = position_dodge(width = 0.6)) +
  facet_grid(. ~ EduLevel, labeller = labeller(EduLevel = label_value)) +
  geom_vline(xintercept = c(1.5, 2.5), color = "grey90") +
  scale_color_manual(values = c("Never" = "black", "Ex" = "steelblue3", "Current" = "indianred2")) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(
    # title = "Male Healthy Working Life Expectancy at age 50 and 95% confidence intervals",
    y = "Years",
    x = NULL,
    color = "Smoking"
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),      
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),   
    axis.text.y = element_text(size = 12),                           
    strip.text = element_text(face = "bold", size = 12),             
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE") %>% select(
  start,Race,education,smoking,
  working.healthy,c1.working.healthy,c2.working.healthy,
  not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
  working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
  not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
  TOTAL,c1.TOTAL,c2.TOTAL)

res.ci_m_smoking_race %>% filter(start=="AVERAGE") %>% select(
  start,Race,smoking,
  working.healthy,c1.working.healthy,c2.working.healthy,
  not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
  working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
  not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
  TOTAL,c1.TOTAL,c2.TOTAL)

res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE") %>% select(
  start,Race,education,smoking,
  working.healthy,c1.working.healthy,c2.working.healthy,
  not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
  working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
  not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
  TOTAL,c1.TOTAL,c2.TOTAL)

res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total") %>% select(
  start,Race,education,smoking,
  working.healthy,
  not.working.healthy,
  working.unhealthy,
  not.working.unhealthy,
  TOTAL)


###### Figure 3 and Tables Females Appendix ############
le_female<-res.ci_f_smoking_educ_race |> 
  filter(start == "AVERAGE" & Race!="Total") |> 
  mutate(
    Race = factor(Race, levels = c("White", "Black", "Hispan"), 
                  labels = c("White", "Black", "Hispanic")),
    EduLevel = factor(education, levels = c(2, 1, 0), 
                      labels = c("Low education", "Medium education", "High education")),
    SmokingStatus = factor(smoking, levels = c(0, 1, 2), 
                           labels = c("Never", "Ex", "Current")))

ggplot(le_female, aes(x = Race, y = working.healthy, color = SmokingStatus)) +
  geom_hline(yintercept = seq(0, 14, 2), linetype = "dashed", color = "grey85") +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(aes(ymin = c1.working.healthy, ymax = c2.working.healthy), 
                width = 0.4, position = position_dodge(width = 0.6)) +
  facet_grid(. ~ EduLevel, labeller = labeller(EduLevel = label_value)) +
  geom_vline(xintercept = c(1.5, 2.5), color = "grey90") +
  scale_color_manual(values = c("Never" = "black", "Ex" = "steelblue3", "Current" = "indianred2")) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(
    # title = "Female Healthy Working Life Expectancy at age 50 and 95% confidence intervals",
    y = "Years",
    x = NULL,
    color = "Smoking"
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),      
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),   
    axis.text.y = element_text(size = 12),                           
    strip.text = element_text(face = "bold", size = 12),             
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )


res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE") %>% select(
  start,Race,education,smoking,
  working.healthy,c1.working.healthy,c2.working.healthy,
  not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
  working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
  not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
  TOTAL,c1.TOTAL,c2.TOTAL)

res.ci_f_smoking_race %>% filter(start=="AVERAGE") %>% select(
  start,Race,smoking,
  working.healthy,c1.working.healthy,c2.working.healthy,
  not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
  working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
  not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy,
  TOTAL,c1.TOTAL,c2.TOTAL)

###### Kitagawa decomposition Function #####

kitagawa_decomp<-function(prevalence1,prevalence2,expectancy1,expectancy2){
  
  avg_LE_1<-sum(c(t(expectancy1))*prevalence1)
  avg_LE_2<-sum(c(t(expectancy2))*prevalence2)
  
  avg_LE_diff<-avg_LE_1-avg_LE_2
  
  LE_diff<-c(c(t(expectancy1))-c(t(expectancy2)))
  LE_avg<-c((c(t(expectancy1))+c(t(expectancy2)))/2)
  Comp_diff<-prevalence1-prevalence2
  Comp_avg<-(prevalence1+prevalence2)/2
  Comp_eff<-sum(Comp_diff*LE_avg)
  Rate_eff<-sum(LE_diff*Comp_avg)
  dec_diff<-Comp_eff+Rate_eff
  
  data.frame(Comp_eff,Rate_eff,avg_LE_diff,dec_diff)
  
}
###### Males Decomposition and Figure #######
##### Males Education
kd_m_hig_low<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_m %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & education==2 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==2) %>% select(working.healthy)
),grp="High–Low")

kd_m_hig_med<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_m %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & education==1 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==1) %>% select(working.healthy)
),grp="High–Medium")

##### Males Race
kd_m_white_black<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)
),grp="White–Black")

kd_m_white_hispan<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy)
),grp="White–Hispan")

kd_m_hispan_black<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)
),grp="Hispan–Black")


dec_m_out<-rbind(kd_m_hig_low,
                 kd_m_hig_med,
                 kd_m_white_black,
                 kd_m_white_hispan,
                 kd_m_hispan_black)

dec_m_out_long <- dec_m_out %>%
  pivot_longer(cols = c(Rate_eff, Comp_eff), names_to = "metric", values_to = "value") %>%
  mutate(grp = factor(grp, levels = unique(grp)))

ggplot(dec_m_out_long, aes(x = grp, y = value, fill = metric)) +
  geom_col(width = 0.6, position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "HWLE gap decomposition (Males aged 50)",
    x = " ",
    y = "Contribution (years)",
    fill = "Smoking"
  ) +
  scale_fill_manual(
    values = c("Comp_eff" = "steelblue", "Rate_eff" = "grey70"),
    labels = c("Comp_eff" = "Compositional effect", "Rate_eff" = "Rate effect")
  ) +
  coord_cartesian(ylim = c(-0.5, 3.5)) +   # <-- y-axis fixed here
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),      
    axis.text.x = element_text(angle = 1, hjust = 0.5),   
    axis.text.y = element_text(size = 12),                           
    strip.text = element_text(face = "bold", size = 12),             
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )



###### Females Decomposition and Figure #######
##### Females Education
kd_f_hig_low<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_f %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & education==2 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==2) %>% select(working.healthy)
),grp="High–Low")

kd_f_hig_med<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_f %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & education==1 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==1) %>% select(working.healthy)
),grp="High–Medium")

##### Females Race
kd_f_white_black<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)
),grp="White–Black")

kd_f_white_hispan<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy)
),grp="White–Hispan")

kd_f_hispan_black<-data.frame(kitagawa_decomp(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)
),grp="Hispan–Black")

dec_f_out<-rbind(kd_f_hig_low,
                 kd_f_hig_med,
                 kd_f_white_black,
                 kd_f_white_hispan,
                 kd_f_hispan_black)

dec_f_out_long <- dec_f_out %>%
  pivot_longer(cols = c(Rate_eff, Comp_eff), names_to = "metric", values_to = "value") %>%
  mutate(grp = factor(grp, levels = unique(grp)))



ggplot(dec_f_out_long, aes(x = grp, y = value, fill = metric)) +
  geom_col(width = 0.6, position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "HWLE gap decomposition (Females aged 50)",
    x = " ",
    y = "Contribution (years)",
    fill = "Smoking"
  ) +
  scale_fill_manual(
    values = c("Comp_eff" = "steelblue", "Rate_eff" = "grey70"),
    labels = c("Comp_eff" = "Compositional effect", "Rate_eff" = "Rate effect")
  ) +
  coord_cartesian(ylim = c(-0.5, 3.5)) +   # <-- y-axis fixed here
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),      
    axis.text.x = element_text(angle = 1, hjust = 0.5),   
    axis.text.y = element_text(size = 12),                           
    strip.text = element_text(face = "bold", size = 12),             
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )



###### Counterfactual Function #####

counter_fact<-function(prevalence1,prevalence2,expectancy1,expectancy2){
  
  LEobs_1<-sum(prevalence1*c(t(expectancy1)))
  LEobs_2<-sum(prevalence2*c(t(expectancy2)))
  
  LEobs_counter<-sum(prevalence1*c(t(expectancy2)))
  
  diff_obs<-LEobs_1-LEobs_2
  diff_counter<-LEobs_1-LEobs_counter
  delta_diff<-diff_obs-diff_counter
  counter_factual<-delta_diff/diff_obs
  
  counter_factual
}

###### Males Counterfactual ####
#### Males Education  
cf_m_high_low<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_m %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & education==2 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==2) %>% select(working.healthy)),
  grp="High-Low")

cf_m_high_med<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_m %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & education==1 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==1) %>% select(working.healthy)),
  grp="High-Medium")

#### Males Race  
cf_m_white_black<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)),
  grp="White-Black")

cf_m_white_hispan<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy)),
  grp="White-Hispan")

cf_m_hispan_black<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_m %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_m %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy),
  expectancy2=res.ci_m_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)),
  grp="Hispan-Black")

rbind(cf_m_high_low,
      cf_m_high_med,
      cf_m_white_black,
      cf_m_white_hispan,
      cf_m_hispan_black)

###### Females Counterfactual ####
#### Females Education  
cf_f_high_low<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_f %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & education==2 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==2) %>% select(working.healthy)),
  grp="High-Low")

cf_f_high_med<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_f %>% filter(time==50 & education==0 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & education==1 & race!="Other") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==0) %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_educ_race %>% filter(start=="AVERAGE" & Race=="Total" & education==1) %>% select(working.healthy)),
  grp="High-Medium")

#### Females Race  
cf_f_white_black<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)),
  grp="White-Black")

cf_f_white_hispan<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="White") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="White") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy)),
  grp="White-Hispan")

cf_f_hispan_black<-data.frame(fact=counter_fact(
  prevalence1=estdata_forward_f %>% filter(time==50 & race=="Hispan") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  prevalence2=estdata_forward_f %>% filter(time==50 & race=="Black") %>% summarise(prob0=prop.table(table(smoke))[1],prob1=prop.table(table(smoke))[2],prob2=prop.table(table(smoke))[3]),
  expectancy1=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Hispan") %>% select(working.healthy),
  expectancy2=res.ci_f_smoking_race %>% filter(start=="AVERAGE" & Race=="Black") %>% select(working.healthy)),
  grp="Hispan-Black")

rbind(cf_f_high_low,
      cf_f_high_med,
      cf_f_white_black,
      cf_f_white_hispan,
      cf_f_hispan_black)

###### Average Marginal Effects (AME) ###############################################################
path <- "/Users/alessandroferaldi/Desktop/Nextcloud/randhrs1992_2020v1_STATA/"
filename <- "long_hrs50.Rdata"
load(paste0(path,filename))

### Select only Healthy and Unhealthy states 
### ADL
hrs_healthstate <- hrs |> mutate(state_adl_health=case_match(
  state_adl_sim,
  c("not working/healthy","working/healthy")~"0",
  c("not working/unhealthy","working/unhealthy")~"1",
  c("dead")~"2",
  .default=state_adl_sim)) |> filter(state_adl_health %in% c("0","1"))

### IADL
hrs_healthstate <- hrs_healthstate |> mutate(state_iadl_health=case_match(
  state_iadl_sim,
  c("not working/healthy","working/healthy")~"0",
  c("not working/unhealthy","working/unhealthy")~"1",
  c("dead")~"2",
  .default=state_iadl_sim)) |> filter(state_iadl_health %in% c("0","1"))

### Self Reported Health
hrs_healthstate <- hrs_healthstate |> mutate(state_srh_health=case_match(
  state_srh_sim,
  c("not working/healthy","working/healthy")~"0",
  c("not working/unhealthy","working/unhealthy")~"1",
  c("dead")~"2",
  .default=state_srh_sim)) |> filter(state_srh_health %in% c("0","1"))

### Smoking diseases
hrs_healthstate <- hrs_healthstate |> mutate(state_smkd_health=case_match(
  state_smkd_sim,
  c("not working/healthy","working/healthy")~"0",
  c("not working/unhealthy","working/unhealthy")~"1",
  c("dead")~"2",
  .default=state_smkd_sim)) |> filter(state_smkd_health %in% c("0","1"))

### Placebo (HBP, psychiatric problems, arthritis)
hrs_healthstate <- hrs_healthstate |> mutate(state_pla3_health=case_match(
  state_pla3_sim,
  c("not working/healthy","working/healthy")~"0",
  c("not working/unhealthy","working/unhealthy")~"1",
  c("dead")~"2",
  .default=state_pla3_sim)) |> filter(state_smkd_health %in% c("0","1"))

### Logistic Models
m1_pla3<-glm(as.numeric(state_pla3_health)~as.factor(smok3)+age+as.factor(ragender)+as.factor(education)+as.factor(race),data=hrs_healthstate,family=binomial(link="logit"))
m1_smk<-glm(as.numeric(state_smkd_health)~as.factor(smok3)+age+as.factor(ragender)+as.factor(education)+as.factor(race),data=hrs_healthstate,family=binomial(link="logit"))
m1_srh<-glm(as.numeric(state_srh_health)~as.factor(smok3)+age+as.factor(ragender)+as.factor(education)+as.factor(race),data=hrs_healthstate,family=binomial(link="logit"))
m1_adl<-glm(as.numeric(state_adl_health)~as.factor(smok3)+age+as.factor(ragender)+as.factor(education)+as.factor(race),data=hrs_healthstate,family=binomial(link="logit"))
m1_iadl<-glm(as.numeric(state_iadl_health)~as.factor(smok3)+age+as.factor(ragender)+as.factor(education)+as.factor(race),data=hrs_healthstate,family=binomial(link="logit"))


### AME Logistic Models
library(margins)

ame_smk<-margins(m1_smk, type = "response",data=hrs_healthstate)
ame_pla<-margins(m1_pla3, type = "response",data=hrs_healthstate)
ame_adl<-margins(m1_adl, type = "response",data=hrs_healthstate)
ame_sht<-margins(m1_srh, type = "response",data=hrs_healthstate)

cbind(mean(ame_smk$dydx_smok31)*100,mean(ame_smk$dydx_smok32)*100)
cbind(mean(ame_pla$dydx_smok31)*100,mean(ame_pla$dydx_smok32)*100)
cbind(mean(ame_adl$dydx_smok31)*100,mean(ame_adl$dydx_smok32)*100)
cbind(mean(ame_sht$dydx_smok31)*100,mean(ame_sht$dydx_smok32)*100)

summary(ame_smk)[8:9,]
summary(ame_pla)[8:9,]
summary(ame_adl)[8:9,]
summary(ame_sht)[8:9,]


###### Transition probability Fig2 Total pop by smoking (HW-HW) (HW-UW) (-Dead) #####
hrsdtms <- dtms(transient=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy"),
                absorbing="dead",
                timescale=seq(50,98,1),
                timestep=1:3)


controls_mod<-c("time","time2","smoke","dum65","dum66","dum67")
fit_mod <- dtms_fit(data=estdata_forward,controls=controls_mod)

hrspredict <- dtms(transient=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy"),
                   absorbing="dead",timescale=seq(50,98,1))

reused_mod <- list(time=seq(min(hrsdtms$timescale),98,1),time2=seq(min(hrsdtms$timescale),98,1)^2,
                   dum65=ifelse(seq(50,98,1)==65,1,0),
                   dum66=ifelse(seq(50,98,1)==66,1,0),
                   dum67=ifelse(seq(50,98,1)>=67,1,0))

probs_ns_mod    <- dtms_transitions(dtms=hrsdtms,model = fit_mod,controls=c(reused_mod,list(smoke=factor("0",levels=c("0","1","2")))))
probs_ex_mod    <- dtms_transitions(dtms=hrsdtms,model = fit_mod,controls=c(reused_mod,list(smoke=factor("1",levels=c("0","1","2")))))
probs_sm_mod    <- dtms_transitions(dtms=hrsdtms,model = fit_mod,controls=c(reused_mod,list(smoke=factor("2",levels=c("0","1","2")))))

Tp_ns_mod <- dtms_matrix(dtms=hrspredict, probs=probs_ns_mod)
Tp_ex_mod <- dtms_matrix(dtms=hrspredict, probs=probs_ex_mod)
Tp_sm_mod <- dtms_matrix(dtms=hrspredict, probs=probs_sm_mod)

S_ns_mod <- dtms_start(dtms=hrspredict,data=estdata_forward,start_time=c(min(hrsdtms$timescale):(min(hrsdtms$timescale)+6)),variables = list(smoke=factor("0",levels=c("0","1","2"))))
S_ex_mod <- dtms_start(dtms=hrspredict,data=estdata_forward,start_time=c(min(hrsdtms$timescale):(min(hrsdtms$timescale)+6)),variables = list(smoke=factor("1",levels=c("0","1","2"))))
S_sm_mod <- dtms_start(dtms=hrspredict,data=estdata_forward,start_time=c(min(hrsdtms$timescale):(min(hrsdtms$timescale)+6)),variables = list(smoke=factor("2",levels=c("0","1","2"))))

rbind(
  data.frame(dtms_expectancy(dtms=hrspredict,matrix=Tp_ns_mod,start_distr=S_ns_mod),smoking=rep(0,5),
             start=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy","AVERAGE"))[5,],
  data.frame(dtms_expectancy(dtms=hrspredict,matrix=Tp_ex_mod,start_distr=S_ex_mod),smoking=rep(1,5),
             start=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy","AVERAGE"))[5,],
  data.frame(dtms_expectancy(dtms=hrspredict,matrix=Tp_sm_mod,start_distr=S_sm_mod),smoking=rep(2,5),
             start=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy","AVERAGE"))[5,])


trans_prob_tot_pop<-rbind(data.frame(smoking=0,probs_ns_mod),data.frame(smoking=1,probs_ex_mod),data.frame(smoking=2,probs_sm_mod))
trans_prob_tot_pop$from<-substr(trans_prob_tot_pop$from,1,15);trans_prob_tot_pop$to<-substr(trans_prob_tot_pop$to,1,15)
trans_prob_tot_pop$p_c1<-trans_prob_tot_pop$P-1.96*trans_prob_tot_pop$se;trans_prob_tot_pop$p_c2<-trans_prob_tot_pop$P+1.96*trans_prob_tot_pop$se
tr11_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="working/healthy" & to=="working/healthy"))
tr12_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="working/healthy" & to=="working/unhealt"))
tr15_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="working/healthy" & to=="dead"))
tr25_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="working/unhealt" & to=="dead"))
tr35_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="not working/hea" & to=="dead"))
tr45_tot_pop_ns<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==0 & time<=85 & (from=="not working/unh" & to=="dead"))
tr11_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="working/healthy" & to=="working/healthy"))
tr12_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="working/healthy" & to=="working/unhealt"))
tr15_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="working/healthy" & to=="dead"))
tr25_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="working/unhealt" & to=="dead"))
tr35_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="not working/hea" & to=="dead"))
tr45_tot_pop_ex<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==1 & time<=85 & (from=="not working/unh" & to=="dead"))
tr11_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="working/healthy" & to=="working/healthy"))
tr12_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="working/healthy" & to=="working/unhealt"))
tr15_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="working/healthy" & to=="dead"))
tr25_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="working/unhealt" & to=="dead"))
tr35_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="not working/hea" & to=="dead"))
tr45_tot_pop_sm<-trans_prob_tot_pop %>% group_by(time) %>% filter(smoking==2 & time<=85 & (from=="not working/unh" & to=="dead"))


tr11_tot <- bind_rows(
  tr11_tot_pop_ns,
  tr11_tot_pop_ex,
  tr11_tot_pop_sm)
tr12_tot <- bind_rows(
  tr12_tot_pop_ns,
  tr12_tot_pop_ex,
  tr12_tot_pop_sm)

tr.5_tot_pop_ns <- data.frame(
  time = tr15_tot_pop_ns$time,  
  P    = tr15_tot_pop_ns$P + tr25_tot_pop_ns$P + tr35_tot_pop_ns$P + tr45_tot_pop_ns$P,
  p_c1 = tr15_tot_pop_ns$p_c1 + tr25_tot_pop_ns$p_c1 + tr35_tot_pop_ns$p_c1 + tr45_tot_pop_ns$p_c1,
  p_c2 = tr15_tot_pop_ns$p_c2 + tr25_tot_pop_ns$p_c2 + tr35_tot_pop_ns$p_c2 + tr45_tot_pop_ns$p_c2,
  smoking = 0)
tr.5_tot_pop_ex <- data.frame(
  time = tr15_tot_pop_ex$time,
  P    = tr15_tot_pop_ex$P + tr25_tot_pop_ex$P + tr35_tot_pop_ex$P + tr45_tot_pop_ex$P,
  p_c1 = tr15_tot_pop_ex$p_c1 + tr25_tot_pop_ex$p_c1 + tr35_tot_pop_ex$p_c1 + tr45_tot_pop_ex$p_c1,
  p_c2 = tr15_tot_pop_ex$p_c2 + tr25_tot_pop_ex$p_c2 + tr35_tot_pop_ex$p_c2 + tr45_tot_pop_ex$p_c2,
  smoking = 1)
tr.5_tot_pop_sm <- data.frame(
  time = tr15_tot_pop_sm$time,
  P    = tr15_tot_pop_sm$P + tr25_tot_pop_sm$P + tr35_tot_pop_sm$P + tr45_tot_pop_sm$P,
  p_c1 = tr15_tot_pop_sm$p_c1 + tr25_tot_pop_sm$p_c1 + tr35_tot_pop_sm$p_c1 + tr45_tot_pop_sm$p_c1,
  p_c2 = tr15_tot_pop_sm$p_c2 + tr25_tot_pop_sm$p_c2 + tr35_tot_pop_sm$p_c2 + tr45_tot_pop_sm$p_c2,
  smoking = 2)

tr.5_tot <- bind_rows(tr.5_tot_pop_ns, tr.5_tot_pop_ex, tr.5_tot_pop_sm)
tr11_tot$title<-"Probability of staying healthy and working"
tr12_tot$title<-"From healthy working to unhealthy working"
tr.5_tot$title<-"Probability of dying"

tr.5_tot <- tr.5_tot |>
  mutate(
    P   = pmin(P, 1.14),
    p_c1 = pmin(p_c1, 1.14),
    p_c2 = pmin(p_c2, 1.14)
  )
tr_title <- bind_rows(tr11_tot, tr12_tot, tr.5_tot)

tr_title_plot<-tr_title |> 
  mutate(SmokingStatus = factor(smoking, levels = c(0, 1, 2),labels = c("Never", "Ex", "Current")),
         title = factor(title, levels = unique(title)))

ggplot(tr_title_plot, aes(x = time, y = P, color = SmokingStatus, fill = SmokingStatus)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = p_c1, ymax = p_c2), alpha = 0.2, color = NA) +
  facet_wrap(~title, scales = "free_y", nrow = 1) +   # horizontal layout
  scale_color_manual(values = c("Never" = "black", "Ex" = "steelblue3", "Current" = "indianred2")) +
  scale_fill_manual(values = c("Never" = "black", "Ex" = "steelblue3", "Current" = "indianred2")) +
  labs(
    title = "",
    y = "Probability",
    x = "Age",
    color = "Smoking",
    fill = "Smoking"
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed",linewidth = 0.2),  # add dashed grid
    panel.grid.minor = element_line(color = "grey90", linetype = "dashed",linewidth = 0.2),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

###### Figure Smoking prevalence Appendix #########
path <- "/Users/alessandroferaldi/Desktop/Nextcloud/randhrs1992_2020v1_STATA/"
filename <- "long_hrs50.Rdata"
load(paste0(path,filename))

par(mfrow=c(1,3))
time_prev<-seq(2000,2020,by=2)
hrs_prev<-hrs
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Total Population",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(ragender==1)
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Males",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(ragender==2)
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Females",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(race=="White")
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="White",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(race=="Black")
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Black",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(race=="Hispan")
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Hispanic",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(education==0)
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="High education",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(education==1)
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Medium education",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 

hrs_prev<-hrs %>% filter(education==2)
prev_no<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[1]) %>% filter(wave>=5)
prev_ex<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[2]) %>% filter(wave>=5)
prev_sm<-hrs_prev %>% group_by(wave) %>% summarise(wave=wave[1],p=prop.table(table(smok3))[3]) %>% filter(wave>=5)
plot(sub="",main="Low education",time_prev,prev_no$p,t="l",ylab="Prevalence",xlab="Time",ylim=c(0,.6),lwd=1.75,yaxt="n",pch=19);axis(2,las=2)
lines(time_prev,prev_ex$p,col="steelblue3",lwd=1.75);grid()
lines(time_prev,prev_sm$p,col="indianred2",lwd=1.75)
points(time_prev, prev_no$p, pch=19) 
points(time_prev, prev_ex$p, col="steelblue3", pch=19) 
points(time_prev, prev_sm$p, col="indianred2", pch=19) 


###### Table smoking by gender, race, and education Appendix #######
hrs_prev<-estdata_forward_finale_bl

smoking_male<-round(prop.table(table(data.frame(hrs_prev %>% filter(gender==1))$smoke))*100)
smoking_female<-round(prop.table(table(data.frame(hrs_prev %>% filter(gender==2))$smoke))*100)
smoking_white<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="White"))$smoke))*100)
smoking_black<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Black"))$smoke))*100)
smoking_hispan<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Hispan"))$smoke))*100)
smoking_hig<-round(prop.table(table(data.frame(hrs_prev %>% filter(education==0))$smoke))*100)
smoking_med<-round(prop.table(table(data.frame(hrs_prev %>% filter(education==1))$smoke))*100)
smoking_low<-round(prop.table(table(data.frame(hrs_prev %>% filter(education==2))$smoke))*100)

smoking_white<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="White" & gender==1))$smoke))*100)
smoking_black<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Black" & gender==1))$smoke))*100)
smoking_hispan<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Hispan" & gender==1))$smoke))*100)

smoking_white<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="White" & gender==2))$smoke))*100)
smoking_black<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Black" & gender==2))$smoke))*100)
smoking_hispan<-round(prop.table(table(data.frame(hrs_prev %>% filter(race=="Hispan" & gender==2))$smoke))*100)

###### LE not by smoking ##########
hrsdtms<-dtms(transient=c("working/healthy","not working/healthy","working/unhealthy","not working/unhealthy"),
              absorbing="dead",timescale=seq(50,98,2))

res_forward_m_not.smoking<-data.frame(bootfun_nosm(estdata_forward_m,dtms=hrsdtms))
res_forward_f_not.smoking<-data.frame(bootfun_nosm(estdata_forward_f,dtms=hrsdtms))

res_forward_m_not.smoking[5,"working.unhealthy"]/
  (res_forward_m_not.smoking[5,"working.healthy"]+res_forward_m_not.smoking[5,"working.unhealthy"])*100

res_forward_f_not.smoking[5,"working.unhealthy"]/
  (res_forward_f_not.smoking[5,"working.healthy"]+res_forward_f_not.smoking[5,"working.unhealthy"])*100

# N.iter<-1000
# ci_m_not.smoking<-summary(dtms_boot(data = estdata_forward_m, dtms = hrsdtms, fun = bootfun_nosm, rep = N.iter))
# ci_f_not.smoking<-summary(dtms_boot(data = estdata_forward_f, dtms = hrsdtms, fun = bootfun_nosm, rep = N.iter))

# save(ci_m_not.smoking,file="/Users/alessandroferaldi/Desktop/ci_m_not.smoking.Rdata")
# save(ci_f_not.smoking,file="/Users/alessandroferaldi/Desktop/ci_f_not.smoking.Rdata")

# load(file="/Users/alessandroferaldi/Desktop/ci_m_not.smoking.Rdata") # ci_m_not.smoking
# load(file="/Users/alessandroferaldi/Desktop/ci_f_not.smoking.Rdata") # ci_f_not.smoking


###### Table A2 Appendix ######
res.ci_m_smoking_educ_race |> 
  filter(start == "AVERAGE" & Race!="Total") %>% select(Race,education,smoking,
                                                        TOTAL,c1.TOTAL,c2.TOTAL,
                                                        working.healthy,c1.working.healthy,c2.working.healthy,
                                                        not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
                                                        working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
                                                        not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy)
res.ci_f_smoking_educ_race |> 
  filter(start == "AVERAGE" & Race!="Total") %>% select(Race,education,smoking,
                                                        TOTAL,c1.TOTAL,c2.TOTAL,
                                                        working.healthy,c1.working.healthy,c2.working.healthy,
                                                        not.working.healthy,c1.not.working.healthy,c2.not.working.healthy,
                                                        working.unhealthy,c1.working.unhealthy,c2.working.unhealthy,
                                                        not.working.unhealthy,c1.not.working.unhealthy,c2.not.working.unhealthy)


