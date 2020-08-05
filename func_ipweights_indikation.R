
# CREATE IP WEIGHTS AT BASELINE

#### ESTIMATION OF IP WEIGHTS

func_ipweights_indikation <- function(input_data) { 
  
  #ipw denominator model
  p_denom <- glm(exp ~ 
                   as.factor(killipklass) + 
                   as.factor(fynd) + 
                   as.factor(gender) + 
                   as.factor(uni_hosp) + 
                   age + I(age^2) +
                   gfr + I(gfr^2) + 
                   weight + I(weight^2) + 
                   as.factor(bleed) + 
                   as.factor(warfarin) +
                   as.factor(hemoglobin_cat) + 
                   as.factor(smoking_status) + 
                   as.factor(diabetes) +
                   as.factor(hyperton) +
                   as.factor(hyperlip) + 
                   as.factor(prev_infarction) + 
                   as.factor(prev_pci) + 
                   as.factor(prev_cabg) + 
                   heart_rate + I(heart_rate^2) +
                   systolic_blood_pressure + I(systolic_blood_pressure^2) + 
                   diastolic_blood_pressure + I(diastolic_blood_pressure^2), 
                 family=binomial(), data=input_data)
  
  
  # ipw numerator model
  p_num <- glm(exp ~ 1, family=binomial(), data=input_data)
  
  input_data$pd_exp <- predict(p_denom, input_data, type="response")
  
  input_data$pn_exp <- predict(p_num, input_data, type="response")
  
  #Compute estimated weights
  input_data$sw <- ifelse(input_data$exp==1, input_data$pn_exp/input_data$pd_exp, 
                          (1-input_data$pn_exp)/(1-input_data$pd_exp))
  
  #Truncate to 99th percentile, and keep only weights and lopnr
  input_data <- input_data %>%
    mutate(sw = ifelse(sw >= quantile(sw, 0.99), quantile(sw, 0.99),sw)) %>%
    mutate(sw = ifelse(sw <= quantile(sw, 0.01), quantile(sw, 0.01),sw)) %>%
    select(lopnr, sw)
  
  rm(p_denom, p_num)
  
  ipweights <<- input_data
  
}


