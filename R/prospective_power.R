#' Prospective Power Analyses
#'
#' This function computes power
#'@export
#'@example 
#'example that works
#'prospective_power(nsims =  20, delta_t = .25, icc = .5, v_overall = 20, 
#'                   n_litters = 50, pups_litter = 4, method = "GEE", 
#'                   correction = "d5", parameter = "treatment")
#'example that results in error (litter means cannot estimate litter effect)
#'prospective_power(nsims =  20, delta_t = .25, icc = .5, v_overall = 20, 
#'                   n_litters = 50, pups_litter = 4, method = "LM", 
#'                   correction = "d5", parameter = "litter")
#'
#'
#'
#'



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


