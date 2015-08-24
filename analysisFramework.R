# run this file:
# source("analysisFramework.R", echo=TRUE)

# load all functions and packages
source("start.R")


# load simulated data
load("simData/simDataTest.RData")
sim <- data.frame(sim)

# for testing purposes: reduce data set
sim <- sim[sim$replication <= 2, ]


# analyze each meta-analysis

n.MA <- length(unique(sim$unique))		# overall number of MAs
res <- matrix(NA, nrow=n.MA, ncol=22)	# TODO: adjust ncol

counter <- 1

# Now, loop through all meta-analyses
for (i in 1:n.MA) {
	print(paste0("Computing ", i, "/", n.MA))

	# select rows from one single MA
	MAdat <- sim[sim$unique==unique(sim$unique)[i], ]
	
	# analyze with all MA techniques
	res0 <- as.matrix(cbind(
		
		# save settings of condition to results:
		MAdat[1, 1:8],
		
		# save analysis results:
		#pcurve.est = pcurve_estimate_d(MAdat$t, MAdat$N-2)
		#commented out p-curve until we have it getting CIs for coverage...
		re.est = reEst(MAdat$d,MAdat$v),  
                lm.est = lmVarEst(MAdat$d,MAdat$v)
		## TODO: add all other MA techniques, add CIs
	))
	res[counter, ] <- res0
	counter <- counter+1
}

colnames(res) <- colnames(res0)
res <- data.frame(res)

save(res, file="simData/analysis.RData")

# ---------------------------------------------------------------------
# A quick visualization

# assign meaningful factor levels
res$QRP <- factor(res$QRP, labels=c("No QRPs", "QRPs"))
res$k <- paste0("k = ", res$k, " studies")

library(ggplot2)
ggplot(res, aes(x=pcurve.est, color=factor(d_true), group=factor(d_true))) + geom_density() + facet_grid(k~QRP) + geom_vline(aes(xintercept=unique(res$d_true)), color="grey40", linetype="dotted")

res %>% group_by(d_true, QRP, k) %>% summarise(pcurve.est=mean(pcurve.est))
