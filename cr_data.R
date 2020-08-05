


dat_composite <- dat %>%
                 rename(date_event = date_composite) %>%
                 rename(event_14 = composite_14) %>%
                 rename(event_30 = composite_30) %>%
                 rename(event_180 = composite_180) %>%
                 select(lopnr, interdat, exp, date_event, event_14, event_30, event_180)

dat_death <- dat %>%
                  rename(date_event = date_death) %>%
                  rename(event_14 = death_14) %>%
                  rename(event_30 = death_30) %>%
                  rename(event_180 = death_180) %>%
                  select(lopnr, interdat, exp, date_event, event_14, event_30, event_180)

dat_mi <- dat %>%
                rename(date_event = date_mi) %>%
                rename(event_14 = mi_14) %>%
                rename(event_30 = mi_30) %>%
                rename(event_180 = mi_180) %>%
                select(lopnr, interdat, exp, date_event, event_14, event_30, event_180)


dat_bleed <- dat %>%
              rename(date_event = date_bleed) %>%
              rename(event_14 = bleed_14) %>%
              rename(event_30 = bleed_30) %>%
              rename(event_180 = bleed_180) %>%
              select(lopnr, interdat, exp, date_event, event_14, event_30, event_180)


dat_ipweights <- dat %>%
                 select(lopnr, interdat, exp, indikation, killipklass, fynd, gender, age, gfr, hemoglobin_cat,
                       bleed, warfarin, noac, ticfore, prafore, centreid, centre_g, uni_hosp, weight, 
                       smoking_status, diabetes, hyperton, hyperlip, prev_infarction, prev_pci, prev_cabg, heart_rate, 
                       systolic_blood_pressure, diastolic_blood_pressure) %>%
  
                 #Organise missing variable for analysis
                 mutate(killipklass= replace(killipklass, is.na(killipklass), 9)) %>%
                 mutate(hemoglobin_cat= replace(hemoglobin_cat, is.na(hemoglobin_cat), 9)) %>% 
                 mutate(smoking_status= replace(smoking_status, is.na(smoking_status), 9)) %>% 
                 mutate(heart_rate= replace(heart_rate, is.na(heart_rate), median(heart_rate, na.rm=TRUE))) %>% 
                 mutate(systolic_blood_pressure= replace(systolic_blood_pressure, is.na(systolic_blood_pressure), median(systolic_blood_pressure, na.rm=TRUE))) %>% 
                 mutate(diastolic_blood_pressure= replace(diastolic_blood_pressure, is.na(diastolic_blood_pressure), median(diastolic_blood_pressure, na.rm=TRUE))) %>%  
                 mutate(gfr= replace(gfr, is.na(gfr), median(gfr, na.rm=TRUE))) %>% 
                 mutate(weight= replace(weight, is.na(weight), median(weight, na.rm=TRUE)))  

rm(dat)