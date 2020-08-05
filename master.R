
setwd("I:/EPI/Data/Anthony Matthews/validate/")

# Load packages
library(tableone)
library(labelled)
library(haven)
library(survival)
library(ggplot2)
library(dplyr)
library(survminer)
library(gridExtra)
library(ggpubr)
library(geepack)
library(splitstackshape)
library(boot)
library(tidyr)


# Time code starts
starttime <- Sys.time()


######## CREATE VALUES NEEDED ########

# Outcomes
outcomes <- c("composite", "death", "mi", "bleed")

# Time period
period <- "jan12tomay14"

# Number of bootstraps
numboot <- 500


####### CREATE FUNCTIONS ########

#create ipweights with data in the argument, using the variables selected in this script
source("scripts/func_ipweights.R")
source("scripts/func_ipweights_indikation.R")


######## IDENTIFY BASE DATA #######
dat <- data.frame(read_dta(paste0("mydata/cr_finaldataset_", period, ".dta")))


###### CREATE DATA FRAMES ########

# Create a data frame that only contains the information for each outcome, and one that can be used to calculate ipweights
source("scripts/cr_data.R")


######## RUN ANALYSES ########

## Make a table one
source("scripts/an_tableone.R")

#Kaplan Meier plots
source("scripts/an_kaplanmeier.R")

#Kaplan Meier plots - IP weighted
source("scripts/an_kaplanmeier_ipweighted.R")

# IP weighted risk, risk diff, and risk ratio at 180/30/14 days
source("scripts/an_ipwsurvival_180.R")
source("scripts/an_ipwsurvival_30.R")
source("scripts/an_ipwsurvival_14.R")

# G-formula 180 days
source("scripts/an_gformula_180.R")


#### SENSITIVITY ANALYSES & EFFECT MODIFICATION ######

# Censor at death sensitivity
source("scripts/sens_censdeath_an_ipwsurvival_180.R")

#Other sensitivities & STEMI/NSTEMI EM analysis
analysis <- c(
              "sens_stemi_",
              "sens_nstemi_",
              "sens_allcancer_", 
              "sens_exp_", 
              "sens_bleed_", 
              "sens_confounderlookback_", 
              "sens_completecase_", 
              "sens_older_", 
              "sens_allP2Y12_", 
              "sens_allhepbef_", 
              "sens_includegfr_"
              )

for (p in analysis) {

######## UPDATE BASE DATA #######
dat <- data.frame(read_dta(paste0("mydata/", p, "cr_finaldataset_", period, ".dta")))

###### UPDATE OUTCOME  DATA FRAMES #####
source("scripts/cr_data.R")


#### ANALYSES (run the sensitivity datasets through the previous analysis files) ####
source("scripts/sens_an_ipwsurvival_180.R")
source("scripts/sens_an_ipwsurvival_30.R")

}


endtime <- Sys.time()

#time to run all of the code
endtime-starttime
