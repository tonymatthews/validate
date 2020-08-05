
# Create empty data frome for results 
results_180 <- data.frame()


for(i in outcomes) {
  
#Create an outcome value that is the analysis that is taking palce
  
  if (i =="composite") {
    outcome <- "composite"
    outcome1 <- "Composite"
    dat <- dat_composite
  }
  
  if (i == "death") {
    outcome <- "death"
    outcome1 <- "Death"
    dat <- dat_death
  }
  
  if (i == "mi") {
    outcome <- "mi"
    outcome1 <- "Myocardial infarction"
    dat <- dat_mi
  }
  
  if (i == "bleed") {
    outcome <- "bleed"
    outcome1 <- "Bleeding"
    dat <- dat_bleed
  }

# Bring in data, keep only variables that are needed, merge in weights, and create daily rows
 
  #Create ipweights, and use the indikation function for EM analysese
  if (p == "sens_stemi_" | p == "sens_nstemi_") {
    func_ipweights_indikation(dat_ipweights)
  } else {
    func_ipweights(dat_ipweights)
  }
  
  
  #Expand to a row per day until event
  dat_model <- dat %>%
              merge(ipweights, by="lopnr") %>%
              mutate(survtime = ifelse(event_180==0, 180,
                                       date_event-interdat)) %>%
              expandRows("survtime", drop=F) %>%
              mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
              mutate(event = ifelse(time==survtime-1 & event_180==1, 1, 0)) %>%
              mutate(timesq = time^2) 
  

  ###### Model to predict IP weighted survival curves  ########
  ipw_model <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) + 
                     time + timesq, family=binomial(), weight=sw,
                   data=dat_model)
  summary(ipw_model)
  exp(ipw_model$coefficients)

  
  #Create datasets with all time points under each treatment level
  ipw_exp0 <- data.frame(cbind(seq(0,179),0,(seq(0,179))^2))
  ipw_exp1 <- data.frame(cbind(seq(0,179),1,(seq(0,179))^2))
  
  colnames(ipw_exp0) <- c("time", "exp", "timesq")
  colnames(ipw_exp1) <- c("time", "exp", "timesq")
  
  # assignment of estimates (1-hazard) to each person day
  ipw_exp0$p_noevent0 <- predict(ipw_model, ipw_exp0, type="response")
  ipw_exp1$p_noevent1 <- predict(ipw_model, ipw_exp1, type="response")
  
  #Compute survival for each person day
  ipw_exp0$surv0 <- cumprod(ipw_exp0$p_noevent0)
  ipw_exp1$surv1 <- cumprod(ipw_exp1$p_noevent1)
  
  #merge both datasets together
  ipw_graph <- merge(ipw_exp0, ipw_exp1, by=c("time", "timesq"))
  
  #create the differece in survival
  ipw_graph$survdiff <- ipw_graph$surv1 - ipw_graph$surv0
  
  ipw_graph <- ipw_graph %>%
    arrange(time)
  
  surv0 <- ipw_graph$surv0[180]
  surv1 <- ipw_graph$surv1[180]
  Y_0 <- 1-surv0
  Y_1 <- 1-surv1
  riskdiff <- Y_1 - Y_0
  riskratio <- Y_1/Y_0
  
  
  
  ####### BOOTSTRAP CIS ########
    
  res <- NULL
  set.seed(1)
    
  #Create a data frame that will be used to take samples from
  #this includes outcomes and covariates, so can calculate ipweights again
  dat_boot <- dat %>%
      merge(dat_ipweights, by=c("lopnr", "exp", "interdat"))
  
  for (z in 1:numboot) {
  
    index <- sample(1:nrow(dat_boot), nrow(dat_boot), replace=T)
    boot_dat <- dat_boot[index, ]
    
    
    #Create ipweights 
    if (p == "sens_stemi_" | p == "sens_nstemi_") {
      func_ipweights_indikation(dat_ipweights)
    } else {
      func_ipweights(dat_ipweights)
    }
    
    # merge in weights and create person month data
    boot_dat1 <- boot_dat %>%
      merge(ipweights, by="lopnr") %>%
      mutate(survtime = ifelse(event_180==0, 180,
                               date_event - interdat)) %>%
      expandRows("survtime", drop=F) %>%
      mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
      mutate(event = ifelse(time==survtime-1 & event_180==1, 1, 0)) %>%
      mutate(timesq = time^2)
    
    ipw_model_boot <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) + 
                            time + timesq, family=binomial(), weight=sw,
                          data=boot_dat1)  
    
    #Create datasets with all time points undr each treatment level
    ipw_exp0_boot <- data.frame(cbind(seq(0,179),0,(seq(0,179))^2))
    ipw_exp1_boot <- data.frame(cbind(seq(0,179),1,(seq(0,179))^2))
    
    colnames(ipw_exp0_boot) <- c("time", "exp", "timesq")
    colnames(ipw_exp1_boot) <- c("time", "exp", "timesq")
    
    
    # assignment of estimates (1-hazard) to each person day
    ipw_exp0_boot$p_noevent0 <- predict(ipw_model_boot, ipw_exp0_boot, type="response")
    ipw_exp1_boot$p_noevent1 <- predict(ipw_model_boot, ipw_exp1_boot, type="response")
    
    #Compute survival for each person day
    ipw_exp0_boot$surv0 <- cumprod(ipw_exp0_boot$p_noevent0)
    ipw_exp1_boot$surv1 <- cumprod(ipw_exp1_boot$p_noevent1)
    
    #merge both datasets together
    ipw_boot <- merge(ipw_exp0_boot, ipw_exp1_boot, by=c("time", "timesq"))
    
    
    ipw_boot <- ipw_boot %>%
      filter(time==179) %>%
      mutate(risk0 = 1-surv0) %>%
      mutate(risk1 = 1-surv1) %>%
      mutate(riskdiff = risk1-risk0) %>%
      mutate(logriskratio = log(risk1/risk0)) %>%
      select(risk0,risk1,riskdiff, logriskratio)
  
    
    res <- rbind(res, cbind(ipw_boot$risk0, ipw_boot$risk1, ipw_boot$riskdiff, ipw_boot$logriskratio))
  }
  
  
  
  ######### CREATE ESTIMATES AND CIS ########
  res_sd <- apply(res,2,sd)
  
  lclY_0 <- Y_0 - 1.96*res_sd[1]
  uclY_0 <- Y_0 + 1.96*res_sd[1]
  
  lclY_1 <- Y_1 - 1.96*res_sd[2]
  uclY_1 <- Y_1 + 1.96*res_sd[2]
  
  lcldiff <- riskdiff - 1.96*res_sd[3]
  ucldiff <- riskdiff + 1.96*res_sd[3]
  
  lclratio <- exp(log(riskratio) - 1.96*res_sd[4])
  uclratio <- exp(log(riskratio) + 1.96*res_sd[4])
  
  result1 <- cbind(paste0(outcome1), "Y_1=1", paste0(format(round(Y_1*100,1), nsmall=1) ,
                                                " (" , format(round(lclY_1*100,1), nsmall=1), "," ,
                                                       format(round(uclY_1*100,1), nsmall=1), ")" ))
  result2 <- cbind(paste0(outcome1), "Y_0=1", paste0(format(round(Y_0*100,1), nsmall=1) ,
                                                " (" , format(round(lclY_0*100,1), nsmall=1), "," ,
                                                       format(round(uclY_0*100,1), nsmall=1), ")" ))  
  result3 <- cbind(paste0(outcome1), "RD", paste0(format(round(riskdiff*100,1), nsmall=1), 
                                                " (" , format(round(lcldiff*100,1), nsmall=1), "," ,
                                                       format(round(ucldiff*100,1), nsmall=1), ")" ))
  result4 <- cbind(paste0(outcome1), "RR", paste0(format(round(riskratio,2), nsmall=2), 
                                                " (" , format(round(lclratio,2), nsmall=2), "," ,
                                                       format(round(uclratio,2), nsmall=2), ")" ))
  result5 <- cbind(paste0(outcome1), "HR", paste0(format(round(hazardratio,2), nsmall=2), 
                                                  " (" , format(round(lclhazardratio,2), nsmall=2), "," ,
                                                  format(round(uclhazardratio,2), nsmall=2), ")" ))
  
  
  results <- rbind(result1, result2, result3, result4, result5)
  results_180 <- rbind(results_180, results)

} # end for(i in outcomes)

# organise final results
results_180 <- results_180 %>%
              rename(outcome=V1, measure=V2, effect=V3) %>%
              spread(measure, effect) %>%
              select(outcome, "Y_1=1", "Y_0=1", "RD", "RR") %>%
              rename("Outcome" = "outcome",
                     "Bivalirudin"="Y_1=1", 
                     "Heparin"="Y_0=1", 
                     "180-day risk difference (95% CI)"="RD", 
                     "180-day risk ratio (95% CI)"="RR")


rm(boot_dat, boot_dat1, ipw_boot, ipw_exp0, ipw_exp0_boot, ipw_exp1,
   ipw_exp1_boot, ipw_graph, res, result1, result2, result3, result4, result5, results,
   Y_0, Y_1, i, index, ipw_model, ipw_model_boot, lclY_0, 
   lclY_1, lcldiff, outcome, outcome1, dat_model, ipweights, dat,
   res_sd, surv0, surv1, survdiff, uclY_0, uclY_1, ucldiff, z)

#write the results to a txt file
write.table(results_180, paste0("logfiles/", p, "results_180_", period), sep="\t", quote=FALSE, row.names=FALSE)





