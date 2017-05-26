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
bc_plot <- function(p, lower = 0.045, upper = 0.055, col_accept = "lightblue", 
col_reject = "indianred") { 
  barplot(p, col = ifelse(p > lower & p < upper,  
                          col_accept, col_reject), ylab = "Type I Error Rate", 
          xlab = "Bias Correction Method")
  abline(h = 0.05, col = "red", lty = 10, lwd = 2)
  abline(h = 0)
}