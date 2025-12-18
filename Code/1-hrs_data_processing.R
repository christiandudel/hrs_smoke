### Load Packages ##############################################################

  library(readstata13)
  require(tidyverse)
  library(readr)
  library(dtms)


### Load data ##################################################################

  # rda file, makes reloading a lot faster
  rdafile <- "Data/hrs.Rda"
  
  if(!file.exists(rdafile)) { 
    
    # Data; can be obtained from https://hrs.isr.umich.edu
    dtafile <- "Data/randhrs1992_2022v1.dta"
    
    # Load (slow!)
    hrs <- read.dta13(file=dtafile,
                      convert.factors=FALSE) 
    
    # Save
    save(hrs,file=rdafile)
    
  } else load(rdafile)


  
### Select variables ###########################################################

  hrs <- hrs %>% select(hhidpn,ragender,raddate,radyear,raeduc,raedyrs,rahispan,raracem,rabdate,rabyear,
                       # Wave status: Response indicator (1= in wave)
                       inw1,inw2,inw3,inw4,inw5,inw6,inw7,inw8,inw9,inw10,inw11,inw12,inw13,inw14,inw15,inw16,
                       # Interview status (5 & 6 = dead)
                       r1iwstat,r2iwstat,r3iwstat,r4iwstat,r5iwstat,r6iwstat,r7iwstat,r8iwstat,r9iwstat,r10iwstat,r11iwstat,r12iwstat,r13iwstat,r14iwstat,r15iwstat,r16iwstat,
                       # Interview midpoint date
                       r1iwmid,r2iwmid,r3iwmid,r4iwmid,r5iwmid,r6iwmid,r7iwmid,r8iwmid,r9iwmid,r10iwmid,r11iwmid,r12iwmid,r13iwmid,r14iwmid,r15iwmid,r16iwmid,
                       # Age in years at interview month
                       r1agey_e,r2agey_e,r3agey_e,r4agey_e,r5agey_e,r6agey_e,r7agey_e,r8agey_e,r9agey_e,r10agey_e,r11agey_e,r12agey_e,r13agey_e,r14agey_e,r15agey_e,r16agey_e,
                       # Sum of ADL responses
                       r2adl5a,r3adl5a,r4adl5a,r5adl5a,r6adl5a,r7adl5a,r8adl5a,r9adl5a,r10adl5a,r11adl5a,r12adl5a,r13adl5a,r14adl5a,r15adl5a,r16adl5a,
                       # Sum of IADL responses
                       r2iadl5a,r3iadl5a,r4iadl5a,r5iadl5a,r6iadl5a,r7iadl5a,r8iadl5a,r9iadl5a,r10iadl5a,r11iadl5a,r12iadl5a,r13iadl5a,r14iadl5a,r15iadl5a,r16iadl5a,
                       # Self reported health (1 = excellent, 5=very bad)
                       r1shlt,r2shlt,r3shlt,r4shlt,r5shlt,r6shlt,r7shlt,r8shlt,r9shlt,r10shlt,r11shlt,r12shlt,r13shlt,r14shlt,r15shlt,r16shlt,
                       # Doctor diagnosed high blood pressure
                       r1hibp,r2hibp,r3hibp,r4hibp,r5hibp,r6hibp,r7hibp,r8hibp,r9hibp,r10hibp,r11hibp,r12hibp,r13hibp,r14hibp,r15hibp,r16hibp,
                       # Diabetes
                       r1diab,r2diab,r3diab,r4diab,r5diab,r6diab,r7diab,r8diab,r9diab,r10diab,r11diab,r12diab,r13diab,r14diab,r15diab,r16diab,
                       # Cancer
                       r1cancr,r2cancr,r3cancr,r4cancr,r5cancr,r6cancr,r7cancr,r8cancr,r9cancr,r10cancr,r11cancr,r12cancr,r13cancr,r14cancr,r15cancr,r16cancr,
                       # Cancer ever
                       r1cancre,r2cancre,r3cancre,r4cancre,r5cancre,r6cancre,r7cancre,r8cancre,r9cancre,r10cancre,r11cancre,r12cancre,r13cancre,r14cancre,r15cancre,r16cancre,
                       # Lung disease
                       r1lung,r2lung,r3lung,r4lung,r5lung,r6lung,r7lung,r8lung,r9lung,r10lung,r11lung,r12lung,r13lung,r14lung,r15lung,r16lung,
                       # Heart problems
                       r1heart,r2heart,r3heart,r4heart,r5heart,r6heart,r7heart,r8heart,r9heart,r10heart,r11heart,r12heart,r13heart,r14heart,r15heart,r16heart,
                       # Stroke 
                       r1strok,r2strok,r3strok,r4strok,r5strok,r6strok,r7strok,r8strok,r9strok,r10strok,r11strok,r12strok,r13strok,r14strok,r15strok,r16strok,
                       # Psychatric problems
                       r1psych,r2psych,r3psych,r4psych,r5psych,r6psych,r7psych,r8psych,r9psych,r10psych,r11psych,r12psych,r13psych,r14psych,r15psych,r16psych,
                       # Arthritis
                       r1arthr,r2arthr,r3arthr,r4arthr,r5arthr,r6arthr,r7arthr,r8arthr,r9arthr,r10arthr,r11arthr,r12arthr,r13arthr,r14arthr,r15arthr,r16arthr,
                       # Smokes now
                       r1smoken,r2smoken,r3smoken,r4smoken,r5smoken,r6smoken,r7smoken,r8smoken,r9smoken,r10smoken,r11smoken,r12smoken,r13smoken,r14smoken,r15smoken,r16smoken,
                       # Ever smoked
                       r1smokev,r2smokev,r3smokev,r4smokev,r5smokev,r6smokev,r7smokev,r8smokev,r9smokev,r10smokev,r11smokev,r12smokev,r13smokev,r14smokev,r15smokev,r16smokev,
                       # Labor force status
                       r1lbrf,r2lbrf,r3lbrf,r4lbrf,r5lbrf,r6lbrf,r7lbrf,r8lbrf,r9lbrf,r10lbrf,r11lbrf,r12lbrf,r13lbrf,r14lbrf,r15lbrf,r16lbrf,
                       # CESD score (mental health)
                       r2cesd,r3cesd,r4cesd,r5cesd,r6cesd,r7cesd,r8cesd,r9cesd,r10cesd,r11cesd,r12cesd,r13cesd,r14cesd,r15cesd,r16cesd,
                       # Marital Status
                       r1mstat,r2mstat,r3mstat,r4mstat,r5mstat,r6mstat,r7mstat,r8mstat,r9mstat,r10mstat,r11mstat,r12mstat,r13mstat,r14mstat,r15mstat,r16mstat,
                       # BMI
                       r1bmi,r2bmi,r3bmi,r4bmi,r5bmi,r6bmi,r7bmi,r8bmi,r9bmi,r10bmi,r11bmi,r12bmi,r13bmi,r14bmi,r15bmi,r16bmi,
                       # Light Physical Activity
                       r7ltactx,r8ltactx,r9ltactx,r10ltactx,r11ltactx,r12ltactx,r13ltactx,r14ltactx,r15ltactx,r16ltactx)

  
### Education, race, gender ####################################################

  # Education
  hrs <- hrs |> mutate(education=case_match(raeduc,
                                            5~2, # College or more
                                            c(3,4)~1, # High-school, some college
                                            c(1,2)~0)) # GED or less than high-school
  
  # Race recode
  hrs <- hrs |> mutate(race=NA) |> 
    mutate(race=ifelse(raracem%in%1,"White",race),
           race=ifelse(raracem%in%2,"Black",race),
           race=ifelse(rahispan%in%1,"Hispan",race),
           race=ifelse(raracem%in%3 & rahispan%in%0,"Other",race),
           race=ifelse(raracem%in%3 & is.na(rahispan),"Other",race))
  
  # Gender
  hrs <- hrs |> mutate(gender=case_match(ragender,
                                         2~1, # Women 
                                         1~0))# Men

  
### Rename vars for easier reshaping below #####################################
  
  # Get names of time varying variables consistent
  hrs <- hrs |> rename_with(~paste0("r",1:16,"inw"),starts_with("inw"))
  hrs <- hrs |> rename_with(~paste0("r",2:16,"iadl"),ends_with("iadl5a"))
  hrs <- hrs |> rename_with(~paste0("r",2:16,"adl"),ends_with("adl5a"))
  hrs <- hrs |> rename_with(~paste0("r",1:16,"age"),ends_with("agey_e"))
  hrs <- hrs |> rename_with(~paste0("r",7:16,"ltact"),ends_with("ltactx"))
  
  # Change name format of time varying variables (not a good solution, but works)
  hrsnames <- str_split_fixed(names(hrs),"r[[:digit:]]{1,2}",2)
  hrsnames <- apply(hrsnames,1,function(x) {paste0(x,collapse="")})
  hrsnumbers <- parse_number(names(hrs))
  hrswhich <- !is.na(hrsnumbers)
  hrsnames[hrswhich] <- paste(hrsnames[hrswhich],hrsnumbers[hrswhich],sep="_")
  names(hrs) <- hrsnames
  
  # Empty vars for reshaping later (required by reshape function)
  hrs <- hrs |> mutate(adl_1=NA,
                       iadl_1=NA,
                       cesd_1=NA,
                       ltact_1=NA,
                       ltact_2=NA,
                       ltact_3=NA,
                       ltact_4=NA,
                       ltact_5=NA,
                       ltact_6=NA)

  
### Reshape ####################################################################

  # Get names of longitudinal vars and their ordering right 
  repvars <- grepl("_",names(hrs))   
  repvars <- names(hrs)[repvars]
  repvars <- unique(unlist(lapply(strsplit(repvars,split="_"),function(x)x[1])))
  repvars <- paste(rep(repvars, each = length(1:16)), 1:16, sep = "_")

  # Reshape (pivot_longer is just not intuitive to me, sorry)
  hrs <- reshape(data=as.data.frame(hrs),
                 direction="long",
                 varying=repvars,
                 sep="_",
                 idvar="hhidpn",
                 #times=1:15,
                 timevar="wave")
  
  # Sort 
  hrs <- hrs |> arrange(hhidpn,wave)

  
### Restrict to observed person years ##########################################
  
  # Drop people after death, and when not (yet) in wave
  hrs <- hrs |> filter(iwstat%in%c(1,5))
  
  
### Variables: age/birth cohort/state pension age ##############################

  # Age is missing in the year of death, ad
  hrs <- hrs |> mutate(age=ifelse(iwstat==5,radyear-rabyear,age))
  
  # If year of death is not known: year of wave
  hrs <- hrs |> mutate(waveyear=case_match(wave,
                                           1~1992,
                                           2~1994,
                                           3~1996,
                                           4~1998,
                                           5~2000,
                                           6~2002,
                                           7~2004,
                                           8~2006,
                                           9~2008,
                                          10~2010, 
                                          11~2012, 
                                          12~2014,
                                          13~2016, 
                                          14~2018,
                                          15~2020, 
                                          16~2022))
  
  hrs <- hrs |> mutate(age=ifelse(iwstat==5 & is.na(age),waveyear-rabyear,age))
  
  # Cohort & pension age
  hrs <- hrs |> mutate(birth_coh=ifelse(rabyear<=1942,"1942",
                                        ifelse(rabyear>1942 & rabyear<=1959,"1943-1959",
                                               ifelse(rabyear>=1960,"1960",NA))),
                       # Pension age (in years)                       
                       state_pension=ifelse(rabyear<=1942,65,
                                            ifelse(rabyear>1942 & rabyear<=1959,66,
                                                   ifelse(rabyear>=1960,67,NA))))
  

### Variable: employment #######################################################
  
  # Employment (slightly more detailed)
  hrs <- hrs |> mutate(workstatus=case_match(lbrf,
                                             1:2~"working",
                                             3  ~"unemployed",
                                             4:5~"retired",
                                             6:7~"inactive"),
                       # Not working and above statutory retirement age = retired
                       workstatus=ifelse(lbrf%in%6:7&age>=state_pension,
                                         "retired",
                                         workstatus),
                       # Above age 79 no one works anymore
                       workstatus=ifelse(lbrf%in%1:3&age>=79,
                                         "retired",
                                         workstatus),
                       # Simplified employment status
                       worksimple=case_match(workstatus,
                                             c("unemployed","inactive")~"not working",
                                              .default=workstatus)) 
  
  
### Variables: smoking #########################################################

  # Current smoker (1=yes)
  hrs <- hrs |> rename('smok'="smoken")
  
  # Smoking 3 categories (now=2, former=1, never=0)
  hrs <- hrs |> mutate(smok3 = NA,
                       smok3 = ifelse(smok==1,2,smok3),
                       smok3 = ifelse(smok==0 & smokev==1,1,smok3),
                       smok3 = ifelse(smok==0 & smokev==0,0,smok3))
  
  
### Variables: health, smoking related #########################################  

  # Smoking-related diseases (1=cancr | lung | heart | stroke| diab)
  hrs <- hrs |> mutate(smkd=ifelse(cancr==1 | lung==1 | heart==1 | strok==1 | diab==1,
                                   1, # 1=smoking related diseases
                                   0))# 0=no smoking related diseases
  
  # Non-smoking related diseases
  hrs <- hrs |> mutate(pla3=ifelse(hibp==1 | psych==1 | arthr==1,
                                   1, #1=any non-smoking related diseases
                                   0))#0=no non-smoking related diseases
  
  # "mixed" diseases, moderate or unclear association with smoking
  hrs <- hrs |> mutate(mix6=ifelse(lung==1 | cancr==1 | heart==1 | strok==1 | diab==1 | hibp==1,
                                   1, # Any
                                   0))# None
  
  # Cancer in wave
  hrs <- hrs |> mutate(cancr=ifelse(cancr==1,1,0))
  
  # Cancer ever reported
  hrs <- hrs |> mutate(cancre=ifelse(cancre==1,1,0))
  
  # Carry forward: ever reported
  hrs <- dtms_forward(data=hrs,statevar="smkd",idvar="hhidpn",timevar="wave",state=1)
  hrs <- dtms_forward(data=hrs,statevar="pla3",idvar="hhidpn",timevar="wave",state=1)
  hrs <- dtms_forward(data=hrs,statevar="mix6",idvar="hhidpn",timevar="wave",state=1)
  hrs <- dtms_forward(data=hrs,statevar="cancr",idvar="hhidpn",timevar="wave",state=1)
  hrs <- dtms_forward(data=hrs,statevar="cancre",idvar="hhidpn",timevar="wave",state=1)
  

### Variables: ADL, iADL, SRH  #################################################
  
  # ADL, iADL, self-rated health (1=unhealthy/disabled)
  hrs <- hrs |> mutate(adl=case_match(adl,
                                      0~0, # No ADL
                                      1:5~1), # At least one ADL
                       iadl=case_match(iadl, 
                                       0~0, # No iADL, at least one ADL
                                       1:5~1),
                       shlt=case_match(shlt,
                                       1:3~0, # Self-rated health "exellent", "very good", or "good"
                                       4:5~1)) # Self rated health "fair" or "poor"
  
  
# Work & disability/health (combined) ##########################################
  
  # Work and Smoking-related diseases
  hrs <- hrs |> mutate(worksmkd=NA,
                       worksmkd=ifelse(worksimple=="working" & smkd==0,"working/healthy",worksmkd),
                       worksmkd=ifelse(worksimple=="working" & smkd==1,"working/unhealthy",worksmkd),
                       worksmkd=ifelse(worksimple=="retired" & smkd==0,"retired/healthy",worksmkd),
                       worksmkd=ifelse(worksimple=="retired" & smkd==1,"retired/unhealthy",worksmkd),
                       worksmkd=ifelse(worksimple=="not working" & smkd==0,"not working/healthy",worksmkd),
                       worksmkd=ifelse(worksimple=="not working" & smkd==1,"not working/unhealthy",worksmkd))
  
  # Work and Placebo diseases
  hrs <- hrs |> mutate(workpla3=NA,
                       workpla3=ifelse(worksimple=="working" & pla3==0,"working/healthy",workpla3),
                       workpla3=ifelse(worksimple=="working" & pla3==1,"working/unhealthy",workpla3),
                       workpla3=ifelse(worksimple=="retired" & pla3==0,"retired/healthy",workpla3),
                       workpla3=ifelse(worksimple=="retired" & pla3==1,"retired/unhealthy",workpla3),
                       workpla3=ifelse(worksimple=="not working" & pla3==0,"not working/healthy",workpla3),
                       workpla3=ifelse(worksimple=="not working" & pla3==1,"not working/unhealthy",workpla3))
  
  # Work and Mixed Smoking diseases
  hrs <- hrs |> mutate(workmix6=NA,
                       workmix6=ifelse(worksimple=="working" & mix6==0,"working/healthy",workmix6),
                       workmix6=ifelse(worksimple=="working" & mix6==1,"working/unhealthy",workmix6),
                       workmix6=ifelse(worksimple=="retired" & mix6==0,"retired/healthy",workmix6),
                       workmix6=ifelse(worksimple=="retired" & mix6==1,"retired/unhealthy",workmix6),
                       workmix6=ifelse(worksimple=="not working" & mix6==0,"not working/healthy",workmix6),
                       workmix6=ifelse(worksimple=="not working" & mix6==1,"not working/unhealthy",workmix6))
  
  # Work and Cancer
  hrs <- hrs |> mutate(workcancr=NA,
                       workcancr=ifelse(worksimple=="working" & cancr==0,"working/healthy",workcancr),
                       workcancr=ifelse(worksimple=="working" & cancr==1,"working/unhealthy",workcancr),
                       workcancr=ifelse(worksimple=="retired" & cancr==0,"retired/healthy",workcancr),
                       workcancr=ifelse(worksimple=="retired" & cancr==1,"retired/unhealthy",workcancr),
                       workcancr=ifelse(worksimple=="not working" & cancr==0,"not working/healthy",workcancr),
                       workcancr=ifelse(worksimple=="not working" & cancr==1,"not working/unhealthy",workcancr))
  
  # Work and Cancer Ever
  hrs <- hrs |> mutate(workcancre=NA,
                       workcancre=ifelse(worksimple=="working" & cancre==0,"working/healthy",workcancre),
                       workcancre=ifelse(worksimple=="working" & cancre==1,"working/unhealthy",workcancre),
                       workcancre=ifelse(worksimple=="retired" & cancre==0,"retired/healthy",workcancre),
                       workcancre=ifelse(worksimple=="retired" & cancre==1,"retired/unhealthy",workcancre),
                       workcancre=ifelse(worksimple=="not working" & cancre==0,"not working/healthy",workcancre),
                       workcancre=ifelse(worksimple=="not working" & cancre==1,"not working/unhealthy",workcancre))
  
  # Work and ADL
  hrs <- hrs |> mutate(workadl=NA,
                       workadl=ifelse(worksimple=="working" & adl==0,"working/healthy",workadl),
                       workadl=ifelse(worksimple=="working" & adl==1,"working/unhealthy",workadl),
                       workadl=ifelse(worksimple=="retired" & adl==0,"retired/healthy",workadl),
                       workadl=ifelse(worksimple=="retired" & adl==1,"retired/unhealthy",workadl),
                       workadl=ifelse(worksimple=="not working" & adl==0,"not working/healthy",workadl),
                       workadl=ifelse(worksimple=="not working" & adl==1,"not working/unhealthy",workadl))
  
  # Work and iADL
  hrs <- hrs |> mutate(workiadl=NA,
                       workiadl=ifelse(worksimple=="working" & iadl==0,"working/healthy",workiadl),
                       workiadl=ifelse(worksimple=="working" & iadl==1,"working/unhealthy",workiadl),
                       workiadl=ifelse(worksimple=="retired" & iadl==0,"retired/healthy",workiadl),
                       workiadl=ifelse(worksimple=="retired" & iadl==1,"retired/unhealthy",workiadl),
                       workiadl=ifelse(worksimple=="not working" & iadl==0,"not working/healthy",workiadl),
                       workiadl=ifelse(worksimple=="not working" & iadl==1,"not working/unhealthy",workiadl))
  
  # Work and self-rated health
  hrs <- hrs |> mutate(worksrh=NA,
                       worksrh=ifelse(worksimple=="working" & shlt==0,"working/healthy",worksrh),
                       worksrh=ifelse(worksimple=="working" & shlt==1,"working/unhealthy",worksrh),
                       worksrh=ifelse(worksimple=="retired" & shlt==0,"retired/healthy",worksrh),
                       worksrh=ifelse(worksimple=="retired" & shlt==1,"retired/unhealthy",worksrh),
                       worksrh=ifelse(worksimple=="not working" & shlt==0,"not working/healthy",worksrh),
                       worksrh=ifelse(worksimple=="not working" & shlt==1,"not working/unhealthy",worksrh))
  

### State variables including death ############################################

  # State using Smoking-related diseases
  hrs <- hrs |> mutate(state_smkd=NA,
                       state_smkd=ifelse(iwstat==1,worksmkd,state_smkd),
                       state_smkd=ifelse(iwstat==5,"dead",state_smkd))
  
  # State using Placebo diseases
  hrs <- hrs |> mutate(state_pla3=NA,
                       state_pla3=ifelse(iwstat==1,workpla3,state_pla3),
                       state_pla3=ifelse(iwstat==5,"dead",state_pla3))
  
  # State using Mixed smoking diseases
  hrs <- hrs |> mutate(state_mix6=NA,
                       state_mix6=ifelse(iwstat==1,workmix6,state_mix6),
                       state_mix6=ifelse(iwstat==5,"dead",state_mix6))
  
  # State using Cancer
  hrs <- hrs |> mutate(state_cancr=NA,
                       state_cancr=ifelse(iwstat==1,workcancr,state_cancr),
                       state_cancr=ifelse(iwstat==5,"dead",state_cancr))
  
  # State using Cancer Ever
  hrs <- hrs |> mutate(state_cancre=NA,
                       state_cancre=ifelse(iwstat==1,workcancre,state_cancre),
                       state_cancre=ifelse(iwstat==5,"dead",state_cancre))
  
  # State using ADL
  hrs <- hrs |> mutate(state_adl=NA,
                       state_adl=ifelse(iwstat==1,workadl,state_adl),
                       state_adl=ifelse(iwstat==5,"dead",state_adl))

  # State using iADL
  hrs <- hrs |> mutate(state_iadl=NA,
                       state_iadl=ifelse(iwstat==1,workiadl,state_iadl),
                       state_iadl=ifelse(iwstat==5,"dead",state_iadl))
  
  # State using self-rated health
  hrs <- hrs |> mutate(state_srh=NA,
                       state_srh=ifelse(iwstat==1,worksrh,state_srh),
                       state_srh=ifelse(iwstat==5,"dead",state_srh))


### Simplified states ##########################################################
  
  # Smoking-related diseases
  hrs <- hrs |> mutate(state_smkd_sim=case_match(
    state_smkd,
    c("not working/healthy","retired/healthy")~"not working/healthy",
    c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
    .default=state_smkd))
  
  # Negative control outcome
  hrs <- hrs |> mutate(state_pla3_sim=case_match(
    state_pla3,
    c("not working/healthy","retired/healthy")~"not working/healthy",
    c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
    .default=state_pla3))
  
  # Mixed diseases
  hrs <- hrs |> mutate(state_mix6_sim=case_match(
    state_mix6,
    c("not working/healthy","retired/healthy")~"not working/healthy",
    c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
    .default=state_mix6))
  
  # Cancer
  hrs <- hrs |> mutate(state_cancr_sim=case_match(
    state_cancr,
    c("not working/healthy","retired/healthy")~"not working/healthy",
    c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
    .default=state_cancr))
  
  # Cancer
  hrs <- hrs |> mutate(state_cancre_sim=case_match(
    state_cancre,
    c("not working/healthy","retired/healthy")~"not working/healthy",
    c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
    .default=state_cancre))
  
  # State using ADL
  hrs <- hrs |> mutate(state_adl_sim=case_match(
                        state_adl,
                        c("not working/healthy","retired/healthy")~"not working/healthy",
                        c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
                        .default=state_adl))
  
  # State using iADL
  hrs <- hrs |> mutate(state_iadl_sim=case_match(
                        state_iadl,
                        c("not working/healthy","retired/healthy")~"not working/healthy",
                        c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
                        .default=state_iadl))
  
  # Self-rated health
  hrs <- hrs |> mutate(state_srh_sim=case_match(
                        state_srh,
                        c("not working/healthy","retired/healthy")~"not working/healthy",
                        c("not working/unhealthy","retired/unhealthy")~"not working/unhealthy",
                        .default=state_srh))

  
### Dropping observations ######################################################  
  
  # Restrict to waves after 2000
  hrs <- hrs |> filter(wave>=5)
  
  # Drop if education and/or race is missing
  hrs <- hrs |> filter(!is.na(race) & !is.na(education))
  
  # Drop if smoking status missing 
  hrs <- hrs |> filter( (!is.na(smok) & !is.na(smokev) & iwstat==1)|iwstat==5)
  
  # Drop if employment missing workstatus
  hrs <- hrs |> filter( (!is.na(workstatus) & iwstat==1) |iwstat==5)
  
  # Drop if key health variables missing
  hrs <- hrs |> filter( (!is.na(smkd) & !is.na(pla3) & !is.na(mix6) & iwstat==1)|iwstat==5)
  
  # Drop if race/ethnicity = "Other" (few person years, heterogeneous group)
  hrs <- hrs |> filter(race!="Other")
  
  
### Saving #####################################################################

  save(hrs,file="Data/long_hrs50.Rdata")