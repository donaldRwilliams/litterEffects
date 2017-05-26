#' Called by GEE Bias Correction
#' @export
#' @examples
#' results()


results <- function(x, nsim){
  m <- data.table::melt(x)
  m <-  data.table::as.data.table(m)
  m$correction <- rep(c("d1", "d2", "d3", "d4", "d5", "dm"), nsim)
  tapply(m$value, m$correction, function(x){ sum(x < 0.05)/length(x)})
}
