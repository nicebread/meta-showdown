# ---------------------------------------------------------------------
# Felix: Look at single results, check for inconsistencies

load("../res.wide.RData")
load("../res.wide.red.RData")

# inconsistencies between p-value and CI?
res.3PSM <- res.wide.red %>% filter(method == "3PSM")
table("p-value NA" = is.na(res.3PSM$b0_p.value), "CI NA" = is.na(res.3PSM$b0_conf.low))

res.TF <- res.wide.red %>% filter(method == "TF")
table("p-value NA" = is.na(res.TF$b0_p.value), "CI NA" = is.na(res.TF$b0_conf.low))


res.3PSM$H0.reject <- (res.3PSM$b0_p.value < .05) & (is.na(res.3PSM$b0_estimate) | res.3PSM$b0_estimate > 0)

table("H0reject" = res.3PSM$H0.reject, "p < .05" = res.3PSM$b0_p.value < .05)

table("H0reject" = res.3PSM$H0.reject, "p < .05" = res.3PSM$b0_p.value < .05, "est > 0" = res.3PSM$b0_estimate > 0)

summ.3PSM <- res.3PSM %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label) %>% 
	dplyr::summarise(
		meanEst		= mean(b0_estimate, na.rm=TRUE),
		meanEst.pos	= mean(posify(b0_estimate), na.rm=TRUE),
		ME 			= mean(b0_estimate - delta, na.rm=TRUE),
		RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
		ME.pos = mean(posify(b0_estimate) - delta, na.rm=TRUE),
		RMSE.pos = sqrt(mean((posify(b0_estimate) - delta)^2, na.rm=TRUE)),
		MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
		perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
		perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
		perc2.5.pos		= quantile(posify(b0_estimate), probs=.025, na.rm=TRUE),
		perc97.5.pos	= quantile(posify(b0_estimate), probs=.975, na.rm=TRUE),
		coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high, na.rm=TRUE) / sum(!is.na(b0_conf.high)),
		consisZero  = sum(0 > b0_conf.low & 0 < b0_conf.high, na.rm=TRUE) / sum(!is.na(b0_conf.high)),		
		n.ci = sum(!is.na(b0_conf.high)),
		coverage.pos 	= sum(delta > b0_conf.low & delta < b0_conf.high & b0_estimate > 0, na.rm=TRUE) / sum(!is.na(b0_conf.high) & b0_estimate > 0),
		consisZero.pos = sum(b0_conf.low < 0, na.rm=TRUE) / sum(!is.na(b0_conf.low)),
		H0.reject = mean(H0.reject)
	)

print(summ.3PSM, n=50)





# Joe debug for power vs CI coverage

load("summ.Rdata")
load("res.hyp.Rdata")

# Comparison of summ's CI and res.hyp's sigtest ----
res <- res.hyp %>% 
  group_by(condition, k, delta, qrpEnv, selProp, tau, method) %>% 
  summarize(null.rate = 1-mean(H0.reject, na.rm = T),
            n.sig = sum(!is.na(H0.reject)))
summ2 <- summ %>% 
  ungroup %>% 
  select(condition, k, delta, qrpEnv, selProp, tau, method, consisZero, consisZero.pos, n.ci)

allres <- full_join(res, summ2) %>% 
  filter(!(method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve")))

# NA counts ----
with(allres, table(method, n.sig == n.ci))

ggplot(allres, aes(x = n.sig, y = n.ci, col = method)) +
  geom_point() +
  facet_wrap(~method) +
  scale_x_continuous("p-values considered") +
  scale_y_continuous("95% CIs considered")+
  geom_abline(intercept = 0, slope = 1)

filter(allres, method == "3PSM") %>% 
  ggplot(aes(x = n.sig, y = n.ci)) +
  geom_point() +
  scale_x_continuous("p-values considered", limits = c(0, 1000)) +
  scale_y_continuous("95% CIs considered", limits = c(0, 1000))+
  geom_abline(intercept = 0, slope = 1)

# When are these different?
filter(allres, n.sig != n.ci) %>% View() # 3PSM seems to return a p-value in some cases where there's no CI
# TODO: Should double-check the parameter associated with that p-value?

# p-value vs 95% CI ----
with(allres, table(method, abs(null.rate - consisZero.pos) > .02))

ggplot(allres, aes(x = null.rate, y = consisZero.pos, col = method)) +
  geom_point()

ggplot(allres, aes(x = null.rate, y = consisZero.pos, col = method)) +
  geom_point() +
  facet_wrap(~method) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous("Nulls retained via p-value (%)") +
  scale_y_continuous("Nulls retained via 95%CI (%)")

# Is log scale enlightening?
ggplot(allres, aes(x = log(null.rate), y = log(consisZero.pos), col = method)) +
  geom_point() +
  facet_wrap(~method) +
  scale_x_continuous("Nulls retained via p-value (logit)") +
  scale_y_continuous("Nulls retained via 95%CI (logit)")

# What about consisZero instead of consisZero.pos?
# Huge discrepancy, probably caused by significant negative estimates
ggplot(allres, aes(x = null.rate, y = consisZero, col = method)) +
  geom_point() +
  facet_wrap(~method) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous("Nulls retained via p-value") +
  scale_y_continuous("Nulls retained via 95%CI")



















# Old stuff ----
# 
# # CI coverage gives alpha = 31%, vanilla and posified
# summ %>% 
#   filter(delta == 0, tau == 0.4, qrpEnv == "none", k == 100, method == "TF", selProp == 0) %>% 
#   ungroup() %>% 
#   select(coverage:consisZero.pos)
# # sig p-value gives alpha = 16%
# res.hyp %>% 
#   filter(delta == 0, tau == 0.4, qrpEnv == "none", k == 100, method == "TF", selProp == 0) %>% 
#   summarize(alpha = mean(H0.reject, na.rm = T),
#             NA.count = sum(is.na(H0.reject)))
# 
# # does trimfill return NA ever? ----
# summ.tf <- summ %>% 
#   ungroup() %>% 
#   filter(method == "TF") %>% 
#   select(condition:tau, -ends_with(".label"), meanEst, meanEst.pos, coverage.pos, consisZero.pos, n.ci)
# ggplot(summ.tf, aes(x = n.ci)) +
#   geom_histogram() # once in 428,000 times
# 
# dat1 <- data.frame(yi = 1,
#                    sei = .5)
# trimfill(rma(yi = yi, sei = sei, data = dat1)) # NA b/c k = 1
# dat2 <- rbind(dat1,
#               dat1)
# trimfill(rma(yi = yi, sei = sei, data = dat2)) # NA b/c res$b[2] < 0 is NA
# dat3 <- rbind(dat2,
#               data.frame("yi" = .7, "sei" = .3))
# trimfill(rma(yi = yi, sei = sei, data = dat3)) # NA b/c res$b[2] < 0 is NA
# 
# # heterogeneity and trimfill?
# dat10 <- data.frame(yi = c(-.5, 0, .5, 1, .8, 1, .7, -.3, .5),
#                     sei = c(.25, .2, .15, .3, .4, .12, .24, .29, .05))
# rma(yi = yi, sei = sei, data = dat10, method = "DL")
# funnel(rma(yi = yi, sei = sei, data = dat10, method = "DL"))
# trimfill(rma(yi = yi, sei = sei, data = dat10, method = "DL"))
# trimfill(rma(yi = yi, sei = sei, data = dat10, method = "FE"))
# 
# # is this logic correct? ----
# sigTest <- function(p.value, p.crit, b0_estimate) {
#   (p.value < p.crit) & (is.na(b0_estimate) | b0_estimate > 0)
# }
# 
# testCase <- expand.grid(p.value = c(.01, .06, NA),
#                         b0_estimate = c(-1, 0, 1, NA))
# testCase$p.crit <- .05
# 
# testCase$sig <- sigTest(testCase$p.value, testCase$p.crit, testCase$b0_estimate)
# testCase
# 
# # coverage stats
# coverage <- sum(delta > b0_conf.low & delta < b0_conf.high, na.rm=TRUE) / sum(!is.na(b0_conf.high))
# # delta within interval; NA intervals are discarded
# 
# consisZero <- sum(0 > b0_conf.low & 0 < b0_conf.high, na.rm=TRUE) / sum(!is.na(b0_conf.high))
# # 0 within interval; NA intervals are discarded
# 
# consisZero.pos  <- sum(0 > b0_conf.low & 0 < b0_conf.high & b0_estimate > 0, na.rm=TRUE) / 
#                     sum(!is.na(b0_conf.high) & b0_estimate > 0)
# # 0 within interval AND point estimate is > 0; NA intervals and negative point estimates are discarded
# 
# consisZero.pos.new  <-  sum(b0_conf.low < 0, na.rm=TRUE) / sum(!is.na(b0_conf.low))
# # zero is within lower bound; NA intervals are discarded

