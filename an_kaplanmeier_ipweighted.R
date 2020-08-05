

outcomes <- c("composite", "death", "mi", "bleed")

for(i in outcomes) {
  
  if (i == "composite") {
    dat_surv <- dat_composite
  }
  
  if (i == "death") {
    dat_surv <- dat_death
  }
  
  if (i == "mi") {
    dat_surv <- dat_mi
  }
  
  if (i == "bleed") {
    dat_surv <- dat_bleed
  }
  
  #create ipweights
  func_ipweights(dat_ipweights)
  
  #create survival times
  dat_surv <- dat_surv %>%
              mutate(survtime = ifelse(event_180==0, 180,
                                       date_event-interdat)) %>%
              merge(ipweights, by="lopnr") %>% 
              arrange(survtime)
  
  # IP weighted survival model
  assign(paste0("model_", i), survfit(Surv(survtime, event_180) ~ exp, weights=sw, data=dat_surv)) 

} # end for(i in outcomes)

# IP weighted KM plots

splots <- list()
splots[[1]] <- ggpar(ggsurvplot(model_composite,
                          ylim=c(0.85,1),
                          xlim=c(0,180),
                          censor = FALSE, 
                          break.time.by = 30,
                          title = "Composite",
                          font.main = c(22),
                          font.x = c(22),
                          font.y = c(22),
                          legend = c(0.3,0.3),
                          legend.title = "",
                          legend.labs = c("Heparin", "Bivalirudin"),
                          palette = c("#00BFC4", "#F8766D"),
                          linetype = c("dotted", "solid"),
                          xlab = "Days", 
                          risk.table = FALSE, 
                          tables.theme = theme_cleantable()), 
                          font.legend = list(size=18))


splots[[3]] <-  ggpar(ggsurvplot(model_death, 
                           ylim=c(0.85,1),
                           xlim=c(0,180),
                           censor = FALSE, 
                           break.time.by = 30,
                           title = "Death",
                           font.main = c(22),
                           font.x = c(22),
                           font.y = c(22),
                           legend = c(0.3,0.3),
                           legend.title = "",
                           legend.labs = c("Heparin", "Bivalirudin"),
                           linetype = c("dotted", "solid"),
                           palette = c("#00BFC4", "#F8766D"),
                           xlab = "Days", 
                           risk.table = FALSE, 
                           tables.theme = theme_cleantable()), 
                            font.legend = list(size=18))

splots[[2]] <- ggpar(ggsurvplot(model_mi, 
                          ylim=c(0.85,1),
                          xlim=c(0,180),
                          censor = FALSE, 
                          break.time.by = 30,
                          title = "Myocardial infarction",
                          font.main = c(22),
                          font.x = c(22),
                          font.y = c(22),
                          legend = c(0.3,0.3),
                          legend.title = "",
                          legend.labs = c("Heparin", "Bivalirudin"), 
                          linetype = c("dotted", "solid"),
                          palette = c("#00BFC4", "#F8766D"),
                          xlab = "Days", 
                          risk.table = FALSE, 
                          tables.theme = theme_cleantable()), 
                          font.legend = list(size=18))

splots[[4]] <- ggpar(ggsurvplot(model_bleed, 
                          ylim=c(0.85,1),
                          xlim=c(0,180),
                          censor = FALSE, 
                          break.time.by = 30,
                          title = "Bleeding",
                          font.main = c(22),
                          font.x = c(22),
                          font.y = c(22),
                          legend = c(0.3,0.3),
                          legend.title = "",
                          legend.labs = c("Heparin", "Bivalirudin"), 
                          linetype = c("dotted", "solid"),
                          palette = c("#00BFC4", "#F8766D"),
                          xlab = "Days", 
                          risk.table = FALSE, 
                          tables.theme = theme_cleantable()), 
                          font.legend = list(size=18))

#Put all of above plots in 2x2 panel
all_plots <- arrange_ggsurvplots(splots, print = TRUE, ncol = 2, nrow = 2)

#save to tiff
ggsave(paste0("logfiles/kmplots_180_ipweighted_", period, ".png"), all_plots, scale=2)

rm(all_plots, splots, model_composite, model_death, model_mi, model_bleed, ipweights)