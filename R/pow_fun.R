#' Called by prospective power
#' Please not this function is included so that the underlying code
#' can be seen. This funtion does nothing by itself, but is called
#' by the function prospective power analyses
#' @export
#' @examples
#' power_func()
pow_func <- function(delta_t, icc, v_overall, n_litters, pups_litter, 
                     method, parameter = "treatment", correction = "d5"){
  b_treat = delta_t * sqrt(v_overall)
  dat <- litterEffects::data_generator(b_0 = 5,
                                       b_treat = b_treat, v_overall = v_overall,
                                       icc = icc, n_litters = n_litters,
                                       pups_litter = pups_litter)
  if (method == "MLM" && parameter == "treatment"){
    m_lmer <- lmerTest::lmer(y ~ treat + (1|litter), data = dat)
    lmerTest::anova(m_lmer)[1, 6]
  }else if(method == "MLM" && parameter == "litter"){
    m_lmer <- lmerTest::lmer(y ~ treat + (1|litter), data = my_data)
    r <- lmerTest::rand(m_lmer)$rand.table
    r[,3]
  }else if(method == "GEE"){                          
    capture.output(m_gee <- suppressMessages(
      gee::gee(y ~ treat, id = litter, data = dat, 
               family = gaussian, corstr = "exchangeable")))
    saws_gee <- saws::geeUOmega(m_gee)
    saws::saws(saws_gee, method = correction)[[9]][[2]]
  }else{
    dat <-  dat %>% group_by(litter, treat) %>% summarise(y = mean(y))
    m_lm <- stats::lm(y ~ treat, data = dat)
    anova(m_lm)[1, 5]
  }
}