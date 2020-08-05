###################################################################################
#Author - Anthony Matthews
#Date created - 12/11/2019
#Date of last update - 05/08/2019
#Purpose - Create a table one for a certain time period in VALIDATE
###################################################################################

dat <- data.frame(read_dta(paste("mydata/cr_finaldataset_", period, ".dta", sep="")))

#Exposure 
dat$exp <- factor(dat$exp, levels=c(1,0),
          labels=c("Bivalirudin", "Heparin"))


# Make categorical variables factors
varsToFactor <- c("indikation", 
                  "killipklass", 
                  "uni_hosp", 
                  "gender", 
                  "bleed", 
                  "warfarin", 
                  "noac", 
                  "thrombolysis", 
                  "hemoglobin_cat",
                  "smoking_status", 
                  "diabetes", 
                  "hyperton", 
                  "hyperlip", 
                  "prev_infarction", 
                  "prev_pci", 
                  "prev_cabg", 
                  "fynd",
                  "composite_30",
                  "composite_180", 
                  "death_30", 
                  "death_180", 
                  "mi_30", 
                  "mi_180", 
                  "bleed_30", 
                  "bleed_180")

dat[varsToFactor] <- lapply(dat[varsToFactor], factor)


# Variable labels
varLabelList <- list(exp = "Exposure",
                     indikation = "STEMI",
                     killipklass = "Killip Class",
                     uni_hosp = "University Hospital",
                     gender = "Female",
                     age = "Age (yrs)",
                     weight = "Weight (kg)",
                     bleed = "Prior Bleeding",
                     warfarin = "Warfarin before or under PCI",
                     noac = "NOAC before or under PCI",
                     thrombolysis = "Thrombolysis before or under PCI",
                     ticfore = "Ticgrelor before PCI",
                     prafore = "Prasugrel before PCI",
                     hemoglobin_cat = "Anemia severity category",
                     hemoglobin = "Hemoglobin (g/L)",
                     gfr = "Kidney Function (gfr)",
                     composite_30 = "Composite outcome (30 days)",
                     composite_180 = "Composite outcome (180 days)",
                     death_30 = "Death (30 days)",
                     death_180 = "Death (180 days)",
                     mi_30 = "Myocardial infarction (30 days)",
                     mi_180 = "Myocardial infarction (180 days)",
                     bleed_30 = "Bleeding (30 days)",
                     bleed_180 = "Bleeding (180 days)", 
                     smoking_status = "Smoking status",
                     diabetes = "Diabetes" ,
                     hyperton = "Prior treatment for hypertension",
                     hyperlip = "Prior lipid lowering medication", 
                     prev_infarction = "Prior myocardial infarction", 
                     prev_pci = "Prior PCI", 
                     prev_cabg = "Prior coronary artery bypass grafting", 
                     fynd = "Angiography finding", 
                     heart_rate = "Heart rate (beats per min)",
                     systolic_blood_pressure = "Systolic blood pressure (mmHg)", 
                     diastolic_blood_pressure = "Diastolic blood pressure (mmHg)"
                     
)

var_label(dat) <-  varLabelList

# Create variable list to include in table
vars <- c("gender",
          "age",
          "uni_hosp",
          "indikation", 
          "killipklass", 
          "fynd", 
          "heart_rate",
          "systolic_blood_pressure",
          "diastolic_blood_pressure",
          "hemoglobin_cat", 
          "hemoglobin",
          "weight", 
          "smoking_status",
          "prev_infarction", 
          "prev_pci", 
          "prev_cabg", 
          "bleed", 
          "diabetes", 
          "gfr",  
          "hyperton", 
          "hyperlip", 
          "warfarin", 
          "composite_30", 
          "composite_180", 
          "death_30", 
          "death_180", 
          "mi_30", 
          "mi_180", 
          "bleed_30", 
          "bleed_180")

# Identify the variables in which will calculate median 
median <- c("age", 
            "weight", 
            "hemoglobin", 
            "gfr", 
            "heart_rate", 
            "systolic_blood_pressure", 
            "diastolic_blood_pressure")

#Create table one stratified by exp
tableOne <- CreateTableOne(vars = vars, 
                           strata = c("exp"), 
                           includeNA = TRUE, 
                           data = dat) 

tableOnePrint <- print(tableOne, 
                       nonnormal=median, 
                       contDigits = 1, 
                       test = FALSE, 
                       missing = TRUE, 
                       quote = FALSE, 
                       varLabel = TRUE, 
                       dropEqual = TRUE, 
                       noSpace = TRUE)

# Write to .txt file
write.table(tableOnePrint, 
            file=paste("logfiles/table1_", period, ".txt", sep=""), 
            sep="\t", 
            quote = FALSE, 
            row.names=TRUE, 
            col.names=NA)
       
rm(tableOnePrint, 
   vars, 
   tableOne, 
   median, 
   varsToFactor, 
   varLabelList)


