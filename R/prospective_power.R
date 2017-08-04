#' Prospective Power Analyses
#'
#'This function computes power for multilevel models, generalized estimating equations,
#'and litter means. For multilevel models, power for the treatment and litter effects can 
#'be caluculated.
#' 
#'Power for the litter effect (random intercept) can be calculated by setting the parameter
#'to "litter". Note that computing power for a litter effect is not possible with GEE or LM
#'(litter means). The bias correction is tenatively set to "d5" (what was used in the paper). 
#'Further details can be found in bias correction reference section. Note that, in the future, 
#'we may remove this default and user will have to select the most approapriate bias correction.
#'@param nsims number of iterations. Should be > 1,000, but smaller values can 
#'             build intution for expected power
#'@param delta_t standardized effect size. Follows Cohen's guidelines
#'@param icc residual variance explained by litter (0 - 0.99)
#'@param v_overall total variance in the model
#'@param n_litters number of litters (must be even)
#'@param pups_litter number of pups per litter (must be even)
#'@param method model: "MLM", "GEE", "LM" for litter means
#'@param parameter default is "treatment" (see details)
#'@param correction bias correction for GEE (see details) 
#'                  "d5" was used in manuscript
#'                  see saws package documentation for other options
#'@export
#'@examples
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
                              pups_litter, method, parameter = "treatment", correction = "d5"){
  m <- mean(replicate(nsims, pow_func(delta_t = delta_t, icc = icc, 
                                      v_overall = v_overall, n_litters = n_litters, 
                                      pups_litter = pups_litter, 
                                      method = method, parameter = parameter, 
                                      correction = "d5")) < 0.05)
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


