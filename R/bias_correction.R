#' GEE Bias Correction dfgdsgsf
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
#' @param v_overall  total variance (litter + residual variance)
#' @param icc  Intra-class correlation coefficient for the litter effect (between 0.05 to 0.95 of the total variance)
#' @param n_litters Number of litters in the study (or number of schools)
#' @param pups_litter Number observations per litter (or student from a given school)
#' @keywords GEE
#' @export
#' @details n_litters must be even. Returned value closest to pretermined alpha level should be used as correction 
#' @references Fay and Graubard (2001). Small-Sample Adjustments for 
#' Wald-Type Tests Using Sandwich Estimators. Biometrics 57: 1198-1206.  
#' 
#' Gunsolley, J. C., Getchell, C., & Chinchilli, V. M. (1995). Small sample characteristics of generalized 
#' estimating equations. Communications in Statistics-simulation and Computation, 24: 869-878.
#' 
#' Williams, D.R., Burkner, P.C., Carlsson, R. (2017). Between-litter variation in developmental studies of hormones and behavior: 
#' effects on false positives and statistical power. https://osf.io/wydmc/
#' @examples
#' bias_correction()
#' 
#' # for loop to investigate type I according to litter number
#' 
#' # number of litters
#' n_lit <- seq(5, 20, 5)
#' 
#' # matrix to store results
#' mat <- matrix(ncol = 6, nrow = length(n_lit))
#'  
#' # simulaton
#' 
#' for(j in 1:length(n_lit)){
#'       lit <- n_lit[j] 
#'       mat[j, 1:6] <- bias_correction(nsim = 500, 
#'       icc = 0.5, v_overall = 10, n_litters = lit , 
#'       pups_litter = 4)[1:6]
#'}
#'    
#' res_df <- as.data.table(cbind(n_lit, mat))
#' colnames(res_df) <- c("n_litters", "d1", "d2", 
#'                        "d3", "d4", "d5", "dm")
#' res_df

bias_correction <- function(nsim = 500, icc, v_overall, n_litters, pups_litter){ 
                   p_values <-  replicate(nsim, data_func(icc, v_overall, 
                                          n_litters, pups_litter)) 
results(p_values)
}


