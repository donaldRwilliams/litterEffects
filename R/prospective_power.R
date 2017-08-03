#' Prospective Power Analyses
#'
#' This function computes power
#'@export


prospective_power <- function(nsims, delta_t, icc, v_overall, n_litters, 
                              pups_litter, method, parameter = "treatment", correction){
  m <- mean(replicate(nsims, pow_func(delta_t = delta_t, icc = icc, 
                                      v_overall = v_overall, n_litters = n_litters, 
                                      pups_litter = pups_litter, 
                                      method = method, parameter = parameter, 
                                      correction = correction)) < 0.05)
  if(method == "MLM" && parameter == "treatment"){ 
    list(MLM_power_treatment = m)
  }else if(method == "GEE" && parameter == "treatment"){
    list(GEE_power_treatment = m)
  }else if(method == "LM" && parameter == "treatment"){ 
    list(Litter_mean_power_treatment = m)
  }else if(method == "MLM" && parameter == "litter")  {
    list(MLM_power_litter = m)
  }else if(method == "GEE" && parameter == "litter"){
    print("GEE Cannot Estimate Litter Effect: parameter must be treatment")
  }else if(method == "LM" && parameter == "litter"){
    print("Litter Means Cannot Estimate Litter Effect: parameter must be treatment")
  }}


