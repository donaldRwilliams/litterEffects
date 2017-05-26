results <- function(x){
  m <- reshape2::melt(x)
  m <- data.table::data.table(m)
  colnames(m) <- c("correction", "sim", "value")
  res <-  m[, list(type_1_error = mean(value < 0.05)), 
            by = list(correction)]
  res <-  dplyr::mutate(res, correction =  c("d1", "d2", "d3", "d4", "d5", "dm"))
  res
  }


bias_correction(250, b_0 = 5, b_treat =  0, icc = .5 , v_overall = 10, 
                n_litters = 10, pups_litter = 4)  
