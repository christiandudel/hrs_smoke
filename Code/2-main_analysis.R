### Load packages ##############################################################

  library(dtms)
  library(tidyverse)

  
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

  # dtms
  hrsdtms <- dtms(transient=c("working/healthy","not working/healthy",
                              "working/unhealthy","not working/unhealthy"),
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
  
  # Race/ethnicity 
  estdata_wi <- estdata |> filter(race=="White")
  estdata_b <- estdata |> filter(race=="Black")
  estdata_h <- estdata |> filter(race=="Hispan")
  
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
  
  # Education + race/ethnicity 
  fit_wi <- dtms_fit(data=estdata_wi,controls=convaredu)
  fit_b <- dtms_fit(data=estdata_b,controls=convaredu)
  fit_h <- dtms_fit(data=estdata_h,controls=convaredu)
  
  # Gender + education + race/ethnicity
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
  hrspredict <- dtms(transient=c("working/healthy","not working/healthy",
                                 "working/unhealthy","not working/unhealthy"),
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
  
  # Probabilities: race/ethnicity + education
  probs_wi_0_ns <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ns_0)
  probs_wi_0_ex <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ex_0)
  probs_wi_0_sm <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_sm_0)
  probs_wi_1_ns <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ns_1)
  probs_wi_1_ex <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ex_1)
  probs_wi_1_sm <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_sm_1)
  probs_wi_2_ns <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ns_2)
  probs_wi_2_ex <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_ex_2)
  probs_wi_2_sm <- dtms_transitions(model=fit_wi,dtms=hrspredict,controls=reused_sm_2)
  
  probs_b_0_ns <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ns_0)
  probs_b_0_ex <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ex_0)
  probs_b_0_sm <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_sm_0)
  probs_b_1_ns <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ns_1)
  probs_b_1_ex <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ex_1)
  probs_b_1_sm <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_sm_1)
  probs_b_2_ns <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ns_2)
  probs_b_2_ex <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_ex_2)
  probs_b_2_sm <- dtms_transitions(model=fit_b,dtms=hrspredict,controls=reused_sm_2)

  probs_h_0_ns <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ns_0)
  probs_h_0_ex <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ex_0)
  probs_h_0_sm <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_sm_0)
  probs_h_1_ns <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ns_1)
  probs_h_1_ex <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ex_1)
  probs_h_1_sm <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_sm_1)
  probs_h_2_ns <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ns_2)
  probs_h_2_ex <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_ex_2)
  probs_h_2_sm <- dtms_transitions(model=fit_h,dtms=hrspredict,controls=reused_sm_2)
  
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
  
  # Transition matrix: education + race/ethnicity
  tmat_wi_0_ns <- dtms_matrix(probs=probs_wi_0_ns,dtms=hrspredict)
  tmat_wi_0_ex <- dtms_matrix(probs=probs_wi_0_ex,dtms=hrspredict)
  tmat_wi_0_sm <- dtms_matrix(probs=probs_wi_0_sm,dtms=hrspredict)
  tmat_wi_1_ns <- dtms_matrix(probs=probs_wi_1_ns,dtms=hrspredict)
  tmat_wi_1_ex <- dtms_matrix(probs=probs_wi_1_ex,dtms=hrspredict)
  tmat_wi_1_sm <- dtms_matrix(probs=probs_wi_1_sm,dtms=hrspredict)
  tmat_wi_2_ns <- dtms_matrix(probs=probs_wi_2_ns,dtms=hrspredict)
  tmat_wi_2_ex <- dtms_matrix(probs=probs_wi_2_ex,dtms=hrspredict)
  tmat_wi_2_sm <- dtms_matrix(probs=probs_wi_2_sm,dtms=hrspredict)
  
  tmat_b_0_ns <- dtms_matrix(probs=probs_b_0_ns,dtms=hrspredict)
  tmat_b_0_ex <- dtms_matrix(probs=probs_b_0_ex,dtms=hrspredict)
  tmat_b_0_sm <- dtms_matrix(probs=probs_b_0_sm,dtms=hrspredict)
  tmat_b_1_ns <- dtms_matrix(probs=probs_b_1_ns,dtms=hrspredict)
  tmat_b_1_ex <- dtms_matrix(probs=probs_b_1_ex,dtms=hrspredict)
  tmat_b_1_sm <- dtms_matrix(probs=probs_b_1_sm,dtms=hrspredict)
  tmat_b_2_ns <- dtms_matrix(probs=probs_b_2_ns,dtms=hrspredict)
  tmat_b_2_ex <- dtms_matrix(probs=probs_b_2_ex,dtms=hrspredict)
  tmat_b_2_sm <- dtms_matrix(probs=probs_b_2_sm,dtms=hrspredict)
  
  tmat_h_0_ns <- dtms_matrix(probs=probs_h_0_ns,dtms=hrspredict)
  tmat_h_0_ex <- dtms_matrix(probs=probs_h_0_ex,dtms=hrspredict)
  tmat_h_0_sm <- dtms_matrix(probs=probs_h_0_sm,dtms=hrspredict)
  tmat_h_1_ns <- dtms_matrix(probs=probs_h_1_ns,dtms=hrspredict)
  tmat_h_1_ex <- dtms_matrix(probs=probs_h_1_ex,dtms=hrspredict)
  tmat_h_1_sm <- dtms_matrix(probs=probs_h_1_sm,dtms=hrspredict)
  tmat_h_2_ns <- dtms_matrix(probs=probs_h_2_ns,dtms=hrspredict)
  tmat_h_2_ex <- dtms_matrix(probs=probs_h_2_ex,dtms=hrspredict)
  tmat_h_2_sm <- dtms_matrix(probs=probs_h_2_sm,dtms=hrspredict)
  
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
  start_ns <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=0),start_time=50:55)
  start_ex <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=1),start_time=50:55)
  start_sm <- dtms_start(data=estdata,dtms=hrspredict,variables=list(smoke=2),start_time=50:55)
  
  # Gender
  start_m_ns <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=0),start_time=50:55)
  start_m_ex <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=1),start_time=50:55)
  start_m_sm <- dtms_start(data=estdata_m,dtms=hrspredict,variables=list(smoke=2),start_time=50:55)

  start_w_ns <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=0),start_time=50:55)
  start_w_ex <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=1),start_time=50:55)
  start_w_sm <- dtms_start(data=estdata_w,dtms=hrspredict,variables=list(smoke=2),start_time=50:55)
  
  # Education + race/ethnicity 
  start_wi_0_ns <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_wi_0_ex <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_wi_0_sm <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_wi_1_ns <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_wi_1_ex <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_wi_1_sm <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_wi_2_ns <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_wi_2_ex <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_wi_2_sm <- dtms_start(data=estdata_wi,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_b_0_ns <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_b_0_ex <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_b_0_sm <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_b_1_ns <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_b_1_ex <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_b_1_sm <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_b_2_ns <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_b_2_ex <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_b_2_sm <- dtms_start(data=estdata_b,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)

  start_h_0_ns <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_h_0_ex <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_h_0_sm <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_h_1_ns <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_h_1_ex <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_h_1_sm <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_h_2_ns <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_h_2_ex <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_h_2_sm <- dtms_start(data=estdata_h,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)

  # Gender + education + race/ethnicity 
  start_w_m_0_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_w_m_0_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_w_m_0_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_w_m_1_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_w_m_1_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_w_m_1_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_w_m_2_ns <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_w_m_2_ex <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_w_m_2_sm <- dtms_start(data=estdata_w_m,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_w_w_0_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_w_w_0_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_w_w_0_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_w_w_1_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_w_w_1_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_w_w_1_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_w_w_2_ns <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_w_w_2_ex <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_w_w_2_sm <- dtms_start(data=estdata_w_w,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_b_m_0_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_b_m_0_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_b_m_0_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_b_m_1_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_b_m_1_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_b_m_1_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_b_m_2_ns <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_b_m_2_ex <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_b_m_2_sm <- dtms_start(data=estdata_b_m,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_b_w_0_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_b_w_0_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_b_w_0_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_b_w_1_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_b_w_1_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_b_w_1_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_b_w_2_ns <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_b_w_2_ex <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_b_w_2_sm <- dtms_start(data=estdata_b_w,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_h_m_0_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_h_m_0_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_h_m_0_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_h_m_1_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_h_m_1_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_h_m_1_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_h_m_2_ns <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_h_m_2_ex <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_h_m_2_sm <- dtms_start(data=estdata_h_m,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  start_h_w_0_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=0),start_time=50:55)
  start_h_w_0_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=0),start_time=50:55)
  start_h_w_0_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=0),start_time=50:55)
  start_h_w_1_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=1),start_time=50:55)
  start_h_w_1_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=1),start_time=50:55)
  start_h_w_1_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=1),start_time=50:55)
  start_h_w_2_ns <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=0,education=2),start_time=50:55)
  start_h_w_2_ex <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=1,education=2),start_time=50:55)
  start_h_w_2_sm <- dtms_start(data=estdata_h_w,dtms=hrspredict,variables=list(smoke=2,education=2),start_time=50:55)
  
  
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
  
  # Education + race/ethnicity 
  hwle_wi_0_ns <- dtms_expectancy(matrix=tmat_wi_0_ns,start_distr=start_wi_0_ns,dtms=hrspredict)
  hwle_wi_0_ex <- dtms_expectancy(matrix=tmat_wi_0_ex,start_distr=start_wi_0_ex,dtms=hrspredict)
  hwle_wi_0_sm <- dtms_expectancy(matrix=tmat_wi_0_sm,start_distr=start_wi_0_sm,dtms=hrspredict)
  hwle_wi_1_ns <- dtms_expectancy(matrix=tmat_wi_1_ns,start_distr=start_wi_1_ns,dtms=hrspredict)
  hwle_wi_1_ex <- dtms_expectancy(matrix=tmat_wi_1_ex,start_distr=start_wi_1_ex,dtms=hrspredict)
  hwle_wi_1_sm <- dtms_expectancy(matrix=tmat_wi_1_sm,start_distr=start_wi_1_sm,dtms=hrspredict)
  hwle_wi_2_ns <- dtms_expectancy(matrix=tmat_wi_2_ns,start_distr=start_wi_2_ns,dtms=hrspredict)
  hwle_wi_2_ex <- dtms_expectancy(matrix=tmat_wi_2_ex,start_distr=start_wi_2_ex,dtms=hrspredict)
  hwle_wi_2_sm <- dtms_expectancy(matrix=tmat_wi_2_sm,start_distr=start_wi_2_sm,dtms=hrspredict)
  
  hwle_b_0_ns <- dtms_expectancy(matrix=tmat_b_0_ns,start_distr=start_b_0_ns,dtms=hrspredict)
  hwle_b_0_ex <- dtms_expectancy(matrix=tmat_b_0_ex,start_distr=start_b_0_ex,dtms=hrspredict)
  hwle_b_0_sm <- dtms_expectancy(matrix=tmat_b_0_sm,start_distr=start_b_0_sm,dtms=hrspredict)
  hwle_b_1_ns <- dtms_expectancy(matrix=tmat_b_1_ns,start_distr=start_b_1_ns,dtms=hrspredict)
  hwle_b_1_ex <- dtms_expectancy(matrix=tmat_b_1_ex,start_distr=start_b_1_ex,dtms=hrspredict)
  hwle_b_1_sm <- dtms_expectancy(matrix=tmat_b_1_sm,start_distr=start_b_1_sm,dtms=hrspredict)
  hwle_b_2_ns <- dtms_expectancy(matrix=tmat_b_2_ns,start_distr=start_b_2_ns,dtms=hrspredict)
  hwle_b_2_ex <- dtms_expectancy(matrix=tmat_b_2_ex,start_distr=start_b_2_ex,dtms=hrspredict)
  hwle_b_2_sm <- dtms_expectancy(matrix=tmat_b_2_sm,start_distr=start_b_2_sm,dtms=hrspredict)
  
  hwle_h_0_ns <- dtms_expectancy(matrix=tmat_h_0_ns,start_distr=start_h_0_ns,dtms=hrspredict)
  hwle_h_0_ex <- dtms_expectancy(matrix=tmat_h_0_ex,start_distr=start_h_0_ex,dtms=hrspredict)
  hwle_h_0_sm <- dtms_expectancy(matrix=tmat_h_0_sm,start_distr=start_h_0_sm,dtms=hrspredict)
  hwle_h_1_ns <- dtms_expectancy(matrix=tmat_h_1_ns,start_distr=start_h_1_ns,dtms=hrspredict)
  hwle_h_1_ex <- dtms_expectancy(matrix=tmat_h_1_ex,start_distr=start_h_1_ex,dtms=hrspredict)
  hwle_h_1_sm <- dtms_expectancy(matrix=tmat_h_1_sm,start_distr=start_h_1_sm,dtms=hrspredict)
  hwle_h_2_ns <- dtms_expectancy(matrix=tmat_h_2_ns,start_distr=start_h_2_ns,dtms=hrspredict)
  hwle_h_2_ex <- dtms_expectancy(matrix=tmat_h_2_ex,start_distr=start_h_2_ex,dtms=hrspredict)
  hwle_h_2_sm <- dtms_expectancy(matrix=tmat_h_2_sm,start_distr=start_h_2_sm,dtms=hrspredict)
  
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
  
  # Weights
  weights <- estdata |> filter(time%in%45:55) |> select(education,race) |> table() |> prop.table()
  weights_m <- estdata_m |> filter(time%in%45:55) |> select(education,race) |> table() |> prop.table()
  weights_w <- estdata_w |> filter(time%in%45:55) |> select(education,race) |> table() |> prop.table()
  
  # Total
  hwle_ns_adj <- hwle_wi_0_ns * weights["0","White"]+
                 hwle_h_0_ns * weights["0","Hispan"]+
                 hwle_b_0_ns * weights["0","Black"]+
                 hwle_wi_1_ns * weights["1","White"]+
                 hwle_h_1_ns * weights["1","Hispan"]+
                 hwle_b_1_ns * weights["2","Black"]+
                 hwle_wi_2_ns * weights["2","White"]+
                 hwle_h_2_ns * weights["2","Hispan"]+
                 hwle_b_2_ns * weights["2","Black"] 
  
  hwle_ex_adj <- hwle_wi_0_ex * weights["0","White"]+
                 hwle_h_0_ex * weights["0","Hispan"]+
                 hwle_b_0_ex * weights["0","Black"]+
                 hwle_wi_1_ex * weights["1","White"]+
                 hwle_h_1_ex * weights["1","Hispan"]+
                 hwle_b_1_ex * weights["2","Black"]+
                 hwle_wi_2_ex * weights["2","White"]+
                 hwle_h_2_ex * weights["2","Hispan"]+
                 hwle_b_2_ex * weights["2","Black"] 

  hwle_sm_adj <- hwle_wi_0_sm * weights["0","White"]+
                 hwle_h_0_sm * weights["0","Hispan"]+
                 hwle_b_0_sm * weights["0","Black"]+
                 hwle_wi_1_sm * weights["1","White"]+
                 hwle_h_1_sm * weights["1","Hispan"]+
                 hwle_b_1_sm * weights["2","Black"]+
                 hwle_wi_2_sm * weights["2","White"]+
                 hwle_h_2_sm * weights["2","Hispan"]+
                 hwle_b_2_sm * weights["2","Black"] 
  
  # Men
  hwle_m_ns_adj <- hwle_w_m_0_ns * weights_m["0","White"]+
                   hwle_h_m_0_ns * weights_m["0","Hispan"]+
                   hwle_b_m_0_ns * weights_m["0","Black"]+
                   hwle_w_m_1_ns * weights_m["1","White"]+
                   hwle_h_m_1_ns * weights_m["1","Hispan"]+
                   hwle_b_m_1_ns * weights_m["2","Black"]+
                   hwle_w_m_2_ns * weights_m["2","White"]+
                   hwle_h_m_2_ns * weights_m["2","Hispan"]+
                   hwle_b_m_2_ns * weights_m["2","Black"]
  
  hwle_m_ex_adj <- hwle_w_m_0_ex * weights_m["0","White"]+
                   hwle_h_m_0_ex * weights_m["0","Hispan"]+
                   hwle_b_m_0_ex * weights_m["0","Black"]+
                   hwle_w_m_1_ex * weights_m["1","White"]+
                   hwle_h_m_1_ex * weights_m["1","Hispan"]+
                   hwle_b_m_1_ex * weights_m["2","Black"]+
                   hwle_w_m_2_ex * weights_m["2","White"]+
                   hwle_h_m_2_ex * weights_m["2","Hispan"]+
                   hwle_b_m_2_ex * weights_m["2","Black"]
  
  hwle_m_sm_adj <- hwle_w_m_0_sm * weights_m["0","White"]+
                   hwle_h_m_0_sm * weights_m["0","Hispan"]+
                   hwle_b_m_0_sm * weights_m["0","Black"]+
                   hwle_w_m_1_sm * weights_m["1","White"]+
                   hwle_h_m_1_sm * weights_m["1","Hispan"]+
                   hwle_b_m_1_sm * weights_m["2","Black"]+
                   hwle_w_m_2_sm * weights_m["2","White"]+
                   hwle_h_m_2_sm * weights_m["2","Hispan"]+
                   hwle_b_m_2_sm * weights_m["2","Black"]
  
  # Women
  hwle_w_ns_adj <- hwle_w_w_0_ns * weights_w["0","White"]+
                   hwle_h_w_0_ns * weights_w["0","Hispan"]+
                   hwle_b_w_0_ns * weights_w["0","Black"]+
                   hwle_w_w_1_ns * weights_w["1","White"]+
                   hwle_h_w_1_ns * weights_w["1","Hispan"]+
                   hwle_b_w_1_ns * weights_w["2","Black"]+
                   hwle_w_w_2_ns * weights_w["2","White"]+
                   hwle_h_w_2_ns * weights_w["2","Hispan"]+
                   hwle_b_w_2_ns * weights_w["2","Black"]
  
  hwle_w_ex_adj <- hwle_w_w_0_ex * weights_w["0","White"]+
                   hwle_h_w_0_ex * weights_w["0","Hispan"]+
                   hwle_b_w_0_ex * weights_w["0","Black"]+
                   hwle_w_w_1_ex * weights_w["1","White"]+
                   hwle_h_w_1_ex * weights_w["1","Hispan"]+
                   hwle_b_w_1_ex * weights_w["2","Black"]+
                   hwle_w_w_2_ex * weights_w["2","White"]+
                   hwle_h_w_2_ex * weights_w["2","Hispan"]+
                   hwle_b_w_2_ex * weights_w["2","Black"]
  
  hwle_w_sm_adj <- hwle_w_w_0_sm * weights_w["0","White"]+
                   hwle_h_w_0_sm * weights_w["0","Hispan"]+
                   hwle_b_w_0_sm * weights_w["0","Black"]+
                   hwle_w_w_1_sm * weights_w["1","White"]+
                   hwle_h_w_1_sm * weights_w["1","Hispan"]+
                   hwle_b_w_1_sm * weights_w["2","Black"]+
                   hwle_w_w_2_sm * weights_w["2","White"]+
                   hwle_h_w_2_sm * weights_w["2","Hispan"]+
                   hwle_b_w_2_sm * weights_w["2","Black"]
  
  
### Group differences ##########################################################  
  
  # Unadjusted
  unadjusted_sm <- hwle_sm-hwle_ns
  unadjusted_ex <- hwle_ex-hwle_ns
  unadjusted_m_sm <- hwle_m_sm-hwle_m_ns
  unadjusted_m_ex <- hwle_m_ex-hwle_m_ns
  unadjusted_w_sm <- hwle_w_sm-hwle_w_ns
  unadjusted_w_ex <- hwle_w_ex-hwle_w_ns

  # Adjusted
  adjusted_sm <- hwle_sm_adj-hwle_ns_adj
  adjusted_ex <- hwle_ex_adj-hwle_ns_adj
  adjusted_m_sm <- hwle_m_sm_adj-hwle_m_ns_adj
  adjusted_m_ex <- hwle_m_ex_adj-hwle_m_ns_adj
  adjusted_w_sm <- hwle_w_sm_adj-hwle_w_ns_adj
  adjusted_w_ex <- hwle_w_sm_adj-hwle_w_ns_adj
  
  
### Table 1 ####################################################################
  
  restable <- matrix(data=NA,ncol=9,nrow=7)
  
  colnames(restable) <- c("HWLE/Total","UWLE/Total","LE/Total",
                        "HWLE/Men","UWLE/Men","LE/Men",
                        "HWLE/Women","UWLE/Women","LE/Women")
  
  rownames(restable) <- c("Never smokers","Former smokers","Current smokers",
                       "Effect former (unadjusted)","Effect current (unadjusted)",
                       "Effect former (adjusted)","Effect current (adjusted)")
  
  # Non-smokers levels
  restable[1,1] <- hwle_ns["AVERAGE","working/healthy"]
  restable[1,2] <- hwle_ns["AVERAGE","working/unhealthy"]
  restable[1,3] <- hwle_ns["AVERAGE","TOTAL"]
  restable[1,4] <- hwle_m_ns["AVERAGE","working/healthy"]
  restable[1,5] <- hwle_m_ns["AVERAGE","working/unhealthy"]
  restable[1,6] <- hwle_m_ns["AVERAGE","TOTAL"]
  restable[1,7] <- hwle_w_ns["AVERAGE","working/healthy"]
  restable[1,8] <- hwle_w_ns["AVERAGE","working/unhealthy"]
  restable[1,9] <- hwle_w_ns["AVERAGE","TOTAL"]
  
  # Former smokers, levels
  restable[2,1] <- hwle_ex["AVERAGE","working/healthy"]
  restable[2,2] <- hwle_ex["AVERAGE","working/unhealthy"]
  restable[2,3] <- hwle_ex["AVERAGE","TOTAL"]
  restable[2,4] <- hwle_m_ex["AVERAGE","working/healthy"]
  restable[2,5] <- hwle_m_ex["AVERAGE","working/unhealthy"]
  restable[2,6] <- hwle_m_ex["AVERAGE","TOTAL"]
  restable[2,7] <- hwle_w_ex["AVERAGE","working/healthy"]
  restable[2,8] <- hwle_w_ex["AVERAGE","working/unhealthy"]
  restable[2,9] <- hwle_w_ex["AVERAGE","TOTAL"]
  
  # Current smokers, levels
  restable[3,1] <- hwle_sm["AVERAGE","working/healthy"]
  restable[3,2] <- hwle_sm["AVERAGE","working/unhealthy"]
  restable[3,3] <- hwle_sm["AVERAGE","TOTAL"]
  restable[3,4] <- hwle_m_sm["AVERAGE","working/healthy"]
  restable[3,5] <- hwle_m_sm["AVERAGE","working/unhealthy"]
  restable[3,6] <- hwle_m_sm["AVERAGE","TOTAL"]
  restable[3,7] <- hwle_w_sm["AVERAGE","working/healthy"]
  restable[3,8] <- hwle_w_sm["AVERAGE","working/unhealthy"]
  restable[3,9] <- hwle_w_sm["AVERAGE","TOTAL"]
  
  # Former smokers effects (unadjusted)
  restable[4,1] <- unadjusted_ex["AVERAGE","working/healthy"]
  restable[4,2] <- unadjusted_ex["AVERAGE","working/unhealthy"]
  restable[4,3] <- unadjusted_ex["AVERAGE","TOTAL"]
  restable[4,4] <- unadjusted_m_ex["AVERAGE","working/healthy"]
  restable[4,5] <- unadjusted_m_ex["AVERAGE","working/unhealthy"]
  restable[4,6] <- unadjusted_m_ex["AVERAGE","TOTAL"]
  restable[4,7] <- unadjusted_w_ex["AVERAGE","working/healthy"]
  restable[4,8] <- unadjusted_w_ex["AVERAGE","working/unhealthy"]
  restable[4,9] <- unadjusted_w_ex["AVERAGE","TOTAL"]
  
  # Current smokers effects (unadjusted)
  restable[5,1] <- unadjusted_sm["AVERAGE","working/healthy"]
  restable[5,2] <- unadjusted_sm["AVERAGE","working/unhealthy"]
  restable[5,3] <- unadjusted_sm["AVERAGE","TOTAL"]
  restable[5,4] <- unadjusted_m_sm["AVERAGE","working/healthy"]
  restable[5,5] <- unadjusted_m_sm["AVERAGE","working/unhealthy"]
  restable[5,6] <- unadjusted_m_sm["AVERAGE","TOTAL"]
  restable[5,7] <- unadjusted_w_sm["AVERAGE","working/healthy"]
  restable[5,8] <- unadjusted_w_sm["AVERAGE","working/unhealthy"]
  restable[5,9] <- unadjusted_w_sm["AVERAGE","TOTAL"]
  
  # Former smokers effects (adjusted)
  restable[6,1] <- adjusted_ex["AVERAGE","working/healthy"]
  restable[6,2] <- adjusted_ex["AVERAGE","working/unhealthy"]
  restable[6,3] <- adjusted_ex["AVERAGE","TOTAL"]
  restable[6,4] <- adjusted_m_ex["AVERAGE","working/healthy"]
  restable[6,5] <- adjusted_m_ex["AVERAGE","working/unhealthy"]
  restable[6,6] <- adjusted_m_ex["AVERAGE","TOTAL"]
  restable[6,7] <- adjusted_w_ex["AVERAGE","working/healthy"]
  restable[6,8] <- adjusted_w_ex["AVERAGE","working/unhealthy"]
  restable[6,9] <- adjusted_w_ex["AVERAGE","TOTAL"]
  
  # Smokers effects (adjusted)
  restable[7,1] <- adjusted_sm["AVERAGE","working/healthy"]
  restable[7,2] <- adjusted_sm["AVERAGE","working/unhealthy"]
  restable[7,3] <- adjusted_sm["AVERAGE","TOTAL"]
  restable[7,4] <- adjusted_m_sm["AVERAGE","working/healthy"]
  restable[7,5] <- adjusted_m_sm["AVERAGE","working/unhealthy"]
  restable[7,6] <- adjusted_m_sm["AVERAGE","TOTAL"]
  restable[7,7] <- adjusted_w_sm["AVERAGE","working/healthy"]
  restable[7,8] <- adjusted_w_sm["AVERAGE","working/unhealthy"]
  restable[7,9] <- adjusted_w_sm["AVERAGE","TOTAL"]
  
  
### Save selected results ######################################################  
  
  savelist <- c("restable","hwle_ns","hwle_ex","hwle_sm",
                "hwle_m_ns","hwle_m_ex","hwle_m_sm",
                "hwle_w_ns","hwle_w_ex","hwle_w_sm",
                "unadjusted_ex","unadjusted_sm",
                "unadjusted_m_ex","unadjusted_m_sm",
                "unadjusted_w_ex","unadjusted_w_sm",
                "adjusted_ex","adjusted_sm",
                "adjusted_m_ex","adjusted_m_sm",
                "adjusted_w_ex","adjusted_w_sm")
  
  save(file="Results/main.Rda",list=savelist)
  