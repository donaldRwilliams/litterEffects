#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' bias_correction()
#'
#' @export
bias_correction <-
  bias_correction <- function(nsim, b_0, b_treat, icc, v_overall, 
                              n_litters, pups_litter){
    p_values <-  replicate(nsim, bias_func(b_0, b_treat, 
                                           icc, v_overall, n_litters, pups_litter)) 
    results(p_values)
    
  }

