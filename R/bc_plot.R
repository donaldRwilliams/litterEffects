#' Bias Correction Plot 
#'
#'This function plots the results of bias_correction. It allows for 
#'visuallizing error rates deemed acceptable by the user
#'
#'@param p a vector of p-values returned from bias_correction
#'@param lower lower bound of what is considered an acceptable error rate 
#'@param upper upper bound of what is considered an acceptable error rate
#'@param col_accept color of bias corrected p-value that falls within lower and upper
#'@param col_reject color of bias corrected p-value that does not fall within lower and upper
#'@export
#'@examples
#' bc_plot(p, lower = 0.025, upper = 0.065, col_accept = "purple", col_reject = "orange")
#'
#'# plot based on for loop results
#'      
#'# number of litters    
#'      n_lit <- seq(5, 20, 5)
#'      
#'#--ncol = 4 (four returned false positive rates)
#'#--nrow = length of n_lit

#'  mat <- matrix(ncol = 6, nrow = length(n_lit))

#'  for(j in 1:length(n_lit)){
#'  lit <- n_lit[j] 
#'  mat[j, 1:6] <- bias_correction(nsim = 100, icc = 0.5, 
#'                                        v_overall = 10, n_litters = lit , 
#'                                        pups_litter = 4)[1:6]
#'                                        }
#'                                        
#'res_df <- as.data.table(cbind(n_lit, mat))
#'colnames(res_df) <- c("n_litters", "d1", "d2", "d3", "d4", "d5", "dm")
#'res_df
#'
#'
#'par(mfrow=c(2,2))
#'
#'for(i in 1:nrow(res_df)){
#'cond <- mat[i,-1]
#'bc_plot(cond)
#'abline(h = 0.05, col = "red", lty = 10, lwd = 2)
#'abline(h = 0)
#'}
#'
#'dev.off()
#'  

bc_plot <- function(p, lower = 0.045, upper = 0.055, col_accept = "lightblue", 
col_reject = "indianred") { 
  barplot(p, col = ifelse(p > lower & p < upper,  
                          col_accept, col_reject), ylab = "Type I Error Rate", 
          xlab = "Bias Correction Method")
  abline(h = 0.05, col = "red", lty = 10, lwd = 2)
  abline(h = 0)
}