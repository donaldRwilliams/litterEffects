bias_correction <- function(nsim, b_0, b_treat, icc, v_overall, 
                            n_litters, pups_litter){
  p_values <-  replicate(nsim, bias_func(b_0, b_treat, 
                                         icc, v_overall, n_litters, pups_litter)) 
  results(p_values)
  
}

