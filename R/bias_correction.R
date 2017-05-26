#' GEE Bias Correction
#'
#' This function allows the user to determine the "correct" bias correction
#' for generalized estimating equations. The corrections are to ensure the
#' standard error of the estimates is not underestimated in small sample
#' situations. When uncorrected, GEE's can have error rates that exceed
#' nominal levels (alpha = 0.05).
#' It should also be noted that this function is for a specific design, that
#' has observations completely nested within a higher level unit. For example,
#' litter mates (rodents from same mother) are all categorized one way. In reference
#' to educational settings, this would be equivalent to entire schools receiving 
#' an intervention and the students were the observational units.
#' @param nsim Number of simulations. Default is set to 500 
#' @param icc  Intra-class correlation coefficient for the litter effect (between 0.05 to 0.95 of the total variance)
#' @param n_litters Number of litters in the study (or number of schools)
#' @param pups_litter Number observations per litter (or student from a given school)
#' @keywords GEE
#' @export
#' @details n_litters must be even. Returned value closest to pretermined alpha level should be used as correction 
#' @examples
#' bias_correction()

bias_correction <- function(nsim = 500, icc, v_overall, 
                            n_litters, pups_litter){
  p_values <-  replicate(nsim, data_func(icc, v_overall, n_litters, pups_litter)) 
  results(p_values)
  
}


