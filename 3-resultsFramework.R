## ======================================================================
## This file loads the results of the meta-analyses (which were generated in 2-analysisFramework.R)
## and computes summaries of them (such as mean error, MSE, coverage, etc.)
## ======================================================================

source("start.R")

# load the results files which were generated in 2-analysisFramework.R,
# combine them into one large data frame
analysisFiles <- list.files("analysisParts", pattern=".*\\.RData", full.names=TRUE)

print(paste0("Collecting results from ", length(analysisFiles), " analysis files."))

# loop through all files
res_list <- list()
for (f in analysisFiles) {
	print(f)
	load(f)	# the simulation data frame always is called "res"
	#res$id <- paste0(f, "_", res$id)
	res_list[[f]] <- res
}
res.final <- bind_rows(res_list)
str(res.final)

# final data set in long format:
save(res.final, file="res.final.RData")
#load(file="res.final.RData")


# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

res.final <- res.final %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res.final, id + condition + k + delta + qrpEnv + selProp + tau + method ~ term + variable, value.var="value")
head(res.wide, 16)

# define some meaningful labels for the plots
res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))
res.wide$selProp.label <- factor(res.wide$selProp, levels=unique(res.wide$selProp), labels=paste0("selProp = ", unique(res.wide$selProp)))
res.wide$tau.label <- factor(res.wide$tau, levels=unique(res.wide$tau), labels=paste0("tau = ", unique(res.wide$tau)))

# sanity check
tab <- res.wide %>% group_by(k, delta, qrpEnv, selProp, tau) %>% select(id) %>% dplyr::summarise(n.MA=length(unique(id)))
print(tab, n=50)


# how many simulations do we have in each condition, after we removed all k<4 fpr p-curve etc.?
tab2 <- res.wide %>% group_by(k, delta, qrpEnv, selProp, tau) %>% select(id, method, kSig_estimate) %>% filter(method=="pcurve") %>%  dplyr::summarise(nMA.with.kSig.larger.3=sum(!is.na(kSig_estimate) & kSig_estimate >= 4))
print(tab2, n=54)

res.wide <- inner_join(res.wide, tab2)

save(res.wide, file="res.wide.RData", compress="gzip")
#load(file="res.wide.RData")

# ---------------------------------------------------------------------
#  save a filtered version

res.wide.red <- res.wide

## set estimate of p-curve and p-uniform with < 4 studies to NA
res.wide.red[res.wide.red$method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform") & !is.na(res.wide.red$kSig_estimate) & res.wide.red$kSig_estimate < 4, c("b0_estimate", "b0_conf.low", "b0_conf.high", "b0_p.value", "skewtest_p.value")] <- NA

# set pcurve and puniform estimates to NA for all conditions which have less than 500/1000 successful meta-analyses
res.wide.red[res.wide.red$method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform") & res.wide.red$nMA.with.kSig.larger.3 < 500, c("b0_estimate", "b0_conf.low", "b0_conf.high", "b0_p.value", "skewtest_p.value")] <- NA
				 
save(res.wide.red, file="res.wide.red.RData", compress="gzip")
#load(file="res.wide.red.RData")


# ---------------------------------------------------------------------
#  Compute summary measures across replications

posify <- function(x) {x[x<0] <- 0; return(x)}

summ <- res.wide.red %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% 
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
		n.ci = sum(!is.na(b0_conf.high))
	)

print(summ, n=50)

# How many 3PSM estimates have no CI?
t3 <- res.wide.red %>% filter(method=="3PSM") %>% group_by(selProp, delta, k, tau, qrpEnv) %>% summarise(
	n.CI = sum(!is.na(b0_conf.high)),
	n.p_value = sum(!is.na(b0_p.value))
)
summary(t3$n.CI)
print(t3, n=432)


# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="summ.csv")
save(summ, file="summ.RData")
#load("summ.RData")


# ---------------------------------------------------------------------
# Compute summary file for hypothesis test plot

# load("res.wide.red.RData")
res.hyp <- res.wide.red %>% select(1:8, b0_estimate, b0_p.value, skewtest_p.value, 41:45) %>% filter(!method %in% c("pcurve", "pcurve.lack"))

# define critical p-value for each method
res.hyp$p.crit <- .05

# merge two p-value columns into one
res.hyp$p.value <- ifelse(!is.na(res.hyp$b0_p.value), res.hyp$b0_p.value, res.hyp$skewtest_p.value)
res.hyp <- res.hyp %>% select(-b0_p.value, -skewtest_p.value)

# compute rejection:
# Reject H0 if test is significant AND estimate in correct direction.
# In case of p-curve skewness tests, there is no estimate; estimate is set to NA there.
res.hyp$H0.reject <- (res.hyp$p.value < res.hyp$p.crit) & (is.na(res.hyp$b0_estimate) | res.hyp$b0_estimate > 0)

save(res.hyp, file="res.hyp.RData", compress="gzip")