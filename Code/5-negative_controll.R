### Load packages ##############################################################

  library(dtms)
  library(tidyverse)
  library(margins)

### Load data ##################################################################

  load("Data/long_hrs50.Rdata")


### Editing ####################################################################

  # Predictors
  hrs$age2 <- hrs$age^2
  hrs$education <- factor(hrs$education)
  hrs$race <- factor(hrs$race)
  hrs$gender <- factor(hrs$gender)
  hrs$smok3 <- factor(hrs$smok3)
  
  # Work outcomes
  hrs$wrk <- as.numeric(hrs$workstatus=="working")
  hrs$hwl <- as.numeric(hrs$state_smkd_sim=="working/healthy")

  
### Models #####################################################################

  # Smoking related
  fit_out <- glm(smkd~smok3+education*race*gender+age+age2,data=hrs,family=binomial(link="logit"))

  # Negative control
  fit_nco <- glm(pla3~smok3+education*race*gender+age+age2,data=hrs,family=binomial(link="logit"))
  
  # In-between
  fit_mix <- glm(mix6~smok3+education*race*gender+age+age2,data=hrs,family=binomial(link="logit"))
  
  # Work 
  fit_wrk <- glm(wrk~smok3+education*race*gender+age+age2,data=hrs,family=binomial(link="logit"))
  
  # Healthy work
  fit_hwl <- glm(hwl~smok3+education*race*gender+age+age2,data=hrs,family=binomial(link="logit"))
  
  
### Results ####################################################################
  
  # Marginal effect probability
  ame_out <- margins(fit_out, type = "response",data=hrs)
  ame_nco <- margins(fit_nco, type = "response",data=hrs)
  ame_mix <- margins(fit_mix, type = "response",data=hrs)
  ame_wrk <- margins(fit_wrk, type = "response",data=hrs)
  ame_hwl <- margins(fit_hwl, type = "response",data=hrs)
  
  # Marginal effect link/odds
  aml_out <- margins(fit_out, type = "link",data=hrs)
  aml_nco <- margins(fit_nco, type = "link",data=hrs)
  aml_mix <- margins(fit_mix, type = "link",data=hrs)
  aml_wrk <- margins(fit_wrk, type = "link",data=hrs)
  aml_hwl <- margins(fit_wrk, type = "link",data=hrs)
  
  
### Key results ################################################################
  
  vars <- c("smok31","smok32")
  
  # Prob
  ame_out <- summary(ame_out) |> filter(factor%in%vars) 
  ame_nco <- summary(ame_nco) |> filter(factor%in%vars) 
  ame_mix <- summary(ame_mix) |> filter(factor%in%vars) 
  ame_wrk <- summary(ame_wrk) |> filter(factor%in%vars) 
  ame_hwl <- summary(ame_hwl) |> filter(factor%in%vars) 
  
  # Odds
  res_out <- summary(aml_out) |> filter(factor%in%vars) |> mutate(AME=exp(AME),lower=exp(lower),upper=exp(upper))
  res_nco <- summary(aml_nco) |> filter(factor%in%vars) |> mutate(AME=exp(AME),lower=exp(lower),upper=exp(upper))
  res_mix <- summary(aml_mix) |> filter(factor%in%vars) |> mutate(AME=exp(AME),lower=exp(lower),upper=exp(upper))
  res_wrk <- summary(aml_wrk) |> filter(factor%in%vars) |> mutate(AME=exp(AME),lower=exp(lower),upper=exp(upper))
  res_hwl <- summary(aml_hwl) |> filter(factor%in%vars) |> mutate(AME=exp(AME),lower=exp(lower),upper=exp(upper))
  
  
### Save #######################################################################  
  
  save(list=c("ame_out","ame_nco","ame_mix","ame_wrk","ame_wrk",
              "res_out","res_nco","res_mix","res_wrk","res_hwl"),
       file="Results/nco.Rda")
  