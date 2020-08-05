
# Create empty data frame for results
results_180 <- data.frame()

for(i in outcomes) {
  
  #Create an outcome value that is the analysis that is taking palce
  
  if (i =="composite") {
    outcome <- "composite"
    outcome1 <- "Composite"
    dat_base <- dat_composite
  }
  
  if (i == "death") {
    outcome <- "death"
    outcome1 <- "Death"
    dat_base <- dat_death
  }
  
  if (i == "mi") {
    outcome <- "mi"
    outcome1 <- "Myocardial infarction"
    dat_base <- dat_mi
  }
  
  if (i == "bleed") {
    outcome <- "bleed"
    outcome1 <- "Bleeding"
    dat_base <- dat_bleed
  }
  
  
  dat <- merge(dat_base, dat_ipweights, by=c("lopnr", "interdat", "exp")) 
  
  # One row per person, per day
  dat_surv <- dat %>%
              mutate(survtime = ifelse(event_180==0, 180,
                                 date_event-interdat)) %>%
              expandRows("survtime", drop=F) %>%
              mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
              mutate(event = ifelse(time==survtime-1 & event_180==1, 1, 0)) %>%
              mutate(timesq = time^2) 
  
  #Outcome model
  gf_model <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) +
                    time + timesq +
                    as.factor(indikation) + as.factor(killipklass) + as.factor(fynd) + as.factor(gender) + as.factor(uni_hosp) + age + I(age^2) +
                    gfr + I(gfr^2) + weight + I(weight^2) + as.factor(bleed) + as.factor(warfarin) +
                    as.factor(hemoglobin_cat) + as.factor(smoking_status) + as.factor(diabetes) +
                    as.factor(hyperton) + as.factor(hyperlip) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + heart_rate + I(heart_rate^2) +
                    + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2), 
                    data=dat_surv, family=binomial())
  
  
  #create dataset with all time points for each individual under each treatment
  gf_exp0 <- expandRows(dat, count=180, count.is.col=F)
  gf_exp0$time <- rep(seq(0,179), nrow(dat))
  gf_exp0$timesq <- gf_exp0$time^2
  gf_exp0$exp <- 0
  
  gf_exp1 <- gf_exp0
  gf_exp1$exp <- 1
  
  #use the model to predict the probability of no event for each dataset
  #one where everyone is exposed, the other where no one is exposed
  gf_exp0$p_noevent0 <- predict(gf_model, gf_exp0, type="response")
  gf_exp1$p_noevent1 <- predict(gf_model, gf_exp1, type="response")
  
  gf_exp0_surv <- gf_exp0 %>%
                  group_by(lopnr) %>%
                  mutate(surv0 = cumprod(p_noevent0))
  
  gf_exp1_surv <- gf_exp1 %>%
                  group_by(lopnr) %>%
                  mutate(surv1 = cumprod(p_noevent1))
  
  gf_surv0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv0")]
  gf_surv1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv1")]
  
  gf <- merge(gf_surv0, gf_surv1, by=c("time"))
  gf$survdiff <- gf_surv1-gf_surv0
  
  gf <- gf %>%
    arrange(time)
  
  surv0 <- gf$surv0[180]
  surv1 <- gf$surv1[180]
  Y_0 <- 1-surv0
  Y_1 <- 1-surv1
  riskdiff <- Y_1 - Y_0
  riskratio <- Y_1/Y_0
  
  rm(dat, dat_surv, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, gf_model, gf_surv0, gf_surv1, gf)
  
  
  ###### bootstraps ########
  res <- NULL
  set.seed(1)
  
  dat_boot <- merge(dat_base, dat_ipweights, by=c("lopnr", "interdat", "exp")) 
  
  for (z in 1:numboot) {
    
      index <- sample(1:nrow(dat_boot), nrow(dat_boot), replace=T)
      dat <- dat_boot[index, ]
      
      dat_surv <- dat %>%
                  mutate(survtime = ifelse(event_180==0, 180,
                                           date_event-interdat)) %>%
                  expandRows("survtime", drop=F) %>%
                  mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
                  mutate(event = ifelse(time==survtime-1 & event_180==1, 1, 0)) %>%
                  mutate(timesq = time^2) 
      
      gf_model <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) +
                        time + timesq +
                        as.factor(indikation) + as.factor(killipklass) + as.factor(fynd) + as.factor(gender) + as.factor(uni_hosp) + age + I(age^2) +
                        gfr + I(gfr^2) + weight + I(weight^2) + as.factor(bleed) + as.factor(warfarin) +
                        as.factor(hemoglobin_cat) + as.factor(smoking_status) + as.factor(diabetes) +
                        as.factor(hyperton) + as.factor(hyperlip) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + heart_rate + I(heart_rate^2) +
                        + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2), 
                      data=dat_surv, family=binomial())
      
      #+ as.factor(smoking_status) + as.factor(diabetes) +
      #as.factor(hyperton) + as.factor(hyperlip) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg)
      
      #create dataset with all time points for each individual under each treatment
      gf_exp0 <- expandRows(dat, count=180, count.is.col=F)
      gf_exp0$time <- rep(seq(0,179), nrow(dat))
      gf_exp0$timesq <- gf_exp0$time^2
      gf_exp0$exp <- 0
      
      gf_exp1 <- gf_exp0
      gf_exp1$exp <- 1
      
      #use the model to predict the probability of no event for each dataset
      #one where everyone is exposed, the other where no one is exposed
      gf_exp0$p_noevent0 <- predict(gf_model, gf_exp0, type="response")
      gf_exp1$p_noevent1 <- predict(gf_model, gf_exp1, type="response")
      
      gf_exp0_surv <- gf_exp0 %>%
        group_by(lopnr) %>%
        mutate(surv0 = cumprod(p_noevent0))
      
      gf_exp1_surv <- gf_exp1 %>%
        group_by(lopnr) %>%
        mutate(surv1 = cumprod(p_noevent1))
      
      gf_surv0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv0")]
      gf_surv1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv1")]
      
      gf_boot <- merge(gf_surv0, gf_surv1, by=c("time"))
      gf_boot$survdiff <- gf_surv1-gf_surv0
      
      gf_boot <- gf_boot %>%
        filter(time==179) %>%
        mutate(risk0 = 1-surv0) %>%
        mutate(risk1 = 1-surv1) %>%
        mutate(riskdiff = risk1-risk0) %>%
        mutate(logriskratio = log(risk1/risk0)) %>%
        select(risk0,risk1,riskdiff, logriskratio)
      
      res <- rbind(res, cbind(gf_boot$risk0, gf_boot$risk1, gf_boot$riskdiff, gf_boot$logriskratio))
      
      rm(dat_surv, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, gf_model, gf_surv0, gf_surv1, gf_boot)
  
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
  
  
  
  results <- rbind(result1, result2, result3, result4)
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
         "Risk ratio (95% CI)"="RR")


rm(dat, dat_base, res, result1, result2, result3, result4, results, results_180,
   i, index, lcldiff, lclratio, lclY_0, lclY_1, outcome, outcome1, 
   res_sd, riskdiff, riskratio, surv0, surv1, ucldiff, uclratio, uclY_0, 
   uclY_1, Y_0, Y_1, z)


# write to txt file
write.table(results_180, paste0("logfiles/results_180_gformula_", period), sep="\t", quote=FALSE, row.names=FALSE)
