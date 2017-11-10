## ======================================================================
## This file loads the results of the meta-analyses (which were generated in 2-analysisFramework.R)
## and computes summaries of them (such as mean error, MSE, coverage, etc.)
## ======================================================================

# run this file:
# source("3-resultsFramework.R", echo=TRUE)

source("0-start.R")

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
save(res.final, file="dataFiles/res.final.RData")
#load(file="dataFiles/res.final.RData")


# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, censor, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=432)
all(tab$n.MA == 1000)

res.final <- res.final %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res.final, id + condition + k + delta + qrpEnv + censor + tau + method ~ term + variable, value.var="value")
head(res.wide, 16)

# define some meaningful labels for the plots
res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))
res.wide$censor.label <- factor(res.wide$censor, levels=unique(res.wide$censor), labels=paste0("Publication bias = ", unique(res.wide$censor)))
res.wide$tau.label <- factor(res.wide$tau, levels=unique(res.wide$tau), labels=paste0("tau = ", unique(res.wide$tau)))
res.wide$estimator_type <- factor(res.wide$estimator_type, levels=c(1, 2, 3, 4), labels=c("WAAP", "WLS", "PET", "PEESE"))

# sanity check: each condition should have 1000 sims
tab <- res.wide %>% group_by(k, delta, qrpEnv, censor, tau) %>% dplyr::select(id) %>% dplyr::summarise(n.MA=length(unique(id)))
print(tab, n=50)


# how many simulations do we have in each condition, after we removed all k<4 for p-curve etc.?
tab2 <- res.wide %>% group_by(k, delta, qrpEnv, censor, tau) %>% select(id, method, kSig_estimate) %>% filter(method=="pcurve") %>%  dplyr::summarise(nMA.with.kSig.larger.3=sum(!is.na(kSig_estimate) & kSig_estimate >= 4))
print(tab2, n=54)

res.wide <- inner_join(res.wide, tab2)

save(res.wide, file="dataFiles/res.wide.RData", compress="gzip")
#load(file="dataFiles/res.wide.RData")

# ---------------------------------------------------------------------
#  save a reduced version that applies some filters

res.wide.red <- res.wide

## RULE 1: set estimate of p-curve and p-uniform with < 4 significant studies to NA
#res.wide.red[res.wide.red$method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform") & !is.na(res.wide.red$kSig_estimate) & res.wide.red$kSig_estimate < 4, c("b0_estimate", "b0_conf.low", "b0_conf.high", "b0_p.value", "skewtest_p.value")] <- NA

## RULE 2: Ignore 3PSM when it doesn't provide a p-value
## TODO: I think this is not necessary with the weightr pacakge
#res.wide.red[res.wide.red$method == "3PSM" & is.na(res.wide.red$b0_p.value), c("b0_estimate", "b0_conf.low", "b0_conf.high", "b0_p.value")] <- NA

## RULE 3: Ignore p-uniform when it doesn't provide a lower CI (very rare cases)

PUNI <- res.wide[res.wide$method == "puniform" & !is.na(res.wide$b0_conf.high), ]
table(is.na(PUNI$b0_conf.low))
res.wide.red[res.wide.red$method == "puniform" & is.na(res.wide.red$b0_conf.low), c("b0_estimate", "b0_conf.low", "b0_conf.high", "b0_p.value")] <- NA

				 
# ---------------------------------------------------------------------
# For hypothesis test: Add H0.rejection rule & whether the CI is consistent with zero (i.e., includes zero)

# merge two p-value columns into one (p-curve uses "skewtest_p.value", all other use "b0_p.value")
res.wide.red$p.value <- res.wide.red$b0_p.value
res.wide.red$p.value[res.wide.red$method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack")] <- res.wide.red$skewtest_p.value[res.wide.red$method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack")]
res.wide.red <- res.wide.red %>% select(-b0_p.value, -skewtest_p.value)

# compute H0 rejection: we have three cases:
# 1. Any H0 rejection (i.e., p < .05): variable 'H0.reject'
# 2. Posified H0 rejection (i.e., p < .05 AND estimate in correct direction). Significant p-values in the wrong direction are treated as "no H0 rejection": variable 'H0.reject.pos'  --> this is the main variable for our analyses, and how we think the hypothesis test should be treated in practice
# 3. H0 rejection in wrong direction (i.e., p < .05 AND estimate in wrong direction): variable 'H0.reject.wrongSign'

# The p-curve skewness tests only uses directionally consistent studies for computation, there is no estimate; estimate is set to NA there. Therefore, we treat this only as 'H0.reject.pos'
# p-uniform does a one-sided test by default. The CI is based on profile likelihood and is *two-sided*. The p-value of p-uniform has been doubled in MA-methods/5-p-uniform.R. Therefore, only 'H0.reject.pos' exists for p-uniform.

res.wide.red$H0.reject <- (res.wide.red$p.value < .05)
res.wide.red$H0.reject[res.wide.red$method %in% c("puniform", "pcurve.evidence", "pcurve.hack", "pcurve.lack")] <- NA

res.wide.red$H0.reject.pos <- (res.wide.red$p.value < .05) & (is.na(res.wide.red$b0_estimate) | res.wide.red$b0_estimate > 0)
res.wide.red$H0.reject.pos[is.na(res.wide.red$p.value)] <- NA # if no p-value is provided, set to NA

res.wide.red$H0.reject.wrongSign <- (res.wide.red$p.value < .05) & (res.wide.red$b0_estimate < 0)
res.wide.red$H0.reject.wrongSign[is.na(res.wide.red$p.value)] <- NA
res.wide.red$H0.reject.wrongSign[res.wide.red$method %in% c("puniform", "pcurve.evidence", "pcurve.hack", "pcurve.lack")] <- NA

# consisZero computation

res.wide.red$consisZero <- (0 > res.wide.red$b0_conf.low) & (0 < res.wide.red$b0_conf.high)
res.wide.red$consisZero.pos <- (0 > res.wide.red$b0_conf.low)

# sanity check: H0.reject and !consisZero should be identical except for p-curve
table(!res.wide.red$consisZero, res.wide.red$method, useNA="a")
table(res.wide.red$H0.reject, res.wide.red$method, useNA="a")

table(!res.wide.red$consisZero.pos, res.wide.red$method, useNA="a")
table(res.wide.red$H0.reject.pos, res.wide.red$method, useNA="a")

# --> 284 cases are still inconsistent in puniform. This happens when the CI borderline excludes 0 but p is slightly above .05 (such as .05002)

save(res.wide.red, file="dataFiles/res.wide.red.RData", compress="gzip")
#load(file="dataFiles/res.wide.red.RData")


# ---------------------------------------------------------------------
#  Compute summary measures across replications

posify <- function(x) {x[x<0] <- 0; return(x)}

summ <- res.wide.red %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label, method) %>% 
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
		coverage.pos 	= sum(delta > b0_conf.low & delta < b0_conf.high & b0_estimate > 0, na.rm=TRUE) / sum(!is.na(b0_conf.high) & b0_estimate > 0),
		n.ci = sum(!is.na(b0_conf.high)),
		
		consisZero.rate  = sum(consisZero, na.rm=TRUE) / sum(!is.na(consisZero)),				
		consisZero.rate.pos = sum(consisZero.pos, na.rm=TRUE) / sum(!is.na(consisZero.pos)),
		H0.reject.rate = sum(H0.reject, na.rm=TRUE)/sum(!is.na(H0.reject)),
		H0.reject.pos.rate = sum(H0.reject.pos, na.rm=TRUE)/sum(!is.na(H0.reject.pos)),
		H0.reject.wrongSign.rate = sum(H0.reject.wrongSign, na.rm=TRUE)/sum(!is.na(H0.reject.wrongSign)),

		n.p.values = sum(!is.na(H0.reject.pos)),
		n.validEstimates = sum(!is.na(b0_estimate), na.rm=TRUE)
	)

print(summ, n=50)


# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="dataFiles/summ.csv")
save(summ, file="dataFiles/summ.RData")

# also export into Shiny app
save(summ, file="Shiny/metaExplorer/summ.RData")
#load("dataFiles/summ.RData")
