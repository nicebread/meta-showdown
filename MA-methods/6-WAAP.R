# Weighted least squares estimator: Stanley, T. D., & Doucouliagos, H. (2015). Neither fixed nor random: weighted least squares meta-analysis. Statistics in Medicine, 34(13), 2116–2127. http://doi.org/10.1002/sim.6481

WLS.est <- function(d, v, long=TRUE) {
  se <- sqrt(v)
  d.precision <- 1/se
  d.stand <- d/se
  l1 <- lm(d.stand ~ 0 + d.precision)
  s1 <- summary(l1)
  res <- data.frame(
    method = "WLS",
    term = "b0",
    estimate = 	s1$coefficients[1, 1],
    std.error = s1$coefficients[1, 2],
    statistic = s1$coefficients[1, 2],
    p.value = s1$coefficients[1, 4],
    conf.low = confint(l1)[1],
    conf.high = confint(l1)[2]
  )
  returnRes(res, long)
}

# Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. A. (2017). Finding the power to reduce publication bias. Statistics in Medicine, 54(3), 30–19. http://doi.org/10.1002/sim.7228
WAAP.est <- function(d, v, est=c("WAAP-WLS"), long=TRUE) {
  
  # 1. determine which studies are in the top-N set
  
  # "Here, we employ FE (or, equivalently, WLS) as the proxy for ‘true’ effect."
  WLS.all  <- WLS.est(d, v, long=FALSE)
  true.effect <- WLS.all$estimate
  
  # only select studies that are adequatly powered (Stanley uses a two-sided test)
  powered <- true.effect/2.8 >= sqrt(v)
  
  # 2. compute the unrestricted weighted average (WLS) rma of either all or only adequatly powered studies
  # "Thus, the estimate we employ here is a hybrid between WAAP and WLS; thereby called, ‘WAAP-WLS’"
  # "If there is no or only one adequately powered study in a systematic review, WAAP’s standard error and confidence interval are undefined. In this case, we compute WLS across the entire research record."
  # "When there are two or more adequately powered studies, WAAP is calculated using WLS’s formula from this subset of adequately powered studies."
  # Combined estimator: WAAP-WLS	
  kAdequate <- sum(powered,na.rm=T)
  
  if (kAdequate >= 2) {
    res <- WLS.est(d[powered], v[powered], long=FALSE)
    res$method <- "WAAP-WLS"
    res <- plyr::rbind.fill(res, data.frame(method="WAAP-WLS", term="estimator", type="WAAP", kAdequate=kAdequate))
  } else {
    res <- WLS.all
    res$method <- "WAAP-WLS"
    res <- plyr::rbind.fill(res, data.frame(method="WAAP-WLS", term="estimator", type="WLS", kAdequate=kAdequate))
  }
  returnRes(res, long)
}

# dat1 <- data.frame(dataMA(k=50, delta=0.4, tau=0.1, empN=T, maxN=500, minN=0,meanN=0,selProp=0.7, qrpEnv='none'))
# WLS(dat1$d, dat1$v)
# RMA.est(d=dat1$d, v=dat1$v)
# WAAP.est(d=dat1$d, v=dat1$v, n1=dat1$n1, n2=dat1$n2)
