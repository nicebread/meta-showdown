## ======================================================================
## This file takes the simulated data sets, which are stored in separate
## files in folder /simParts, and runs all meta-analytic techniques on them.
## Then it saves the analyses in separate files in the /analysisParts folder.
## ======================================================================

# run this file:
# source("2-analysisFramework.R", echo=TRUE)

# load all functions and packages
source("0-start.R")

library(doParallel)
# detectCores()
registerDoParallel(cores=20)

(ncores <- getDoParWorkers())	# number of parallel processes

# simDatFiles stores the names of all simulated data files in the folder "simParts"
simDatFiles <- list.files("simPartsRev2", pattern=".*\\.RData", full.names=TRUE)

library(gtools)
simDatFiles <- mixedsort(simDatFiles)


# loop through all simParts files
for (f in simDatFiles) {

	load(f)	# the simulation data frame always is called "sim"
	
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))

	## slice up the file into ncores pieces
	if (length(unique(sim$id)) %% ncores != 0) {
		warning(paste0("Number of MAs (", length(unique(sim$id)), ") not dividable by number of cores (", ncores, ")"))
	}
	
	flush.console()

	# build translation table: which unique sim ID goes into which core?
	translation <- rep(1:ncores, each=length(unique(sim$id))/ncores)
	names(translation) <- unique(sim$id)
	sim$core <- translation[as.character(sim$id)]

	# Now, loop through all meta-analyses, each core gets its share of studies
	res <- foreach(batch=1:ncores, .combine=rbind) %dopar% {    

		counter <- 1
		reslist <- list()	# each MA is stored as 1 list element, which is later combined to a single data frame
	
		sim.piece <- sim[sim$core==batch, ]
		n.MA.piece <- length(unique(sim.piece$id))
		for (i in 1:n.MA.piece) {
			print(paste0(Sys.time(), ", batch=", batch, ": Computing ", i, "/", n.MA.piece))

			# select rows from one single MA
			MAdat <- sim.piece[sim.piece$id == unique(sim.piece$id)[i], ]
			rownames(MAdat) <- NULL
	
			# analyze with all MA techniques
			res0 <- rbind(
				RMA.est(d=MAdat$d, v=MAdat$v, long=TRUE),
				PETPEESE.est(MAdat$d, MAdat$v, PP.test = "one-sided", long=TRUE, runRMA=FALSE),
				pc_skew(t=MAdat$t, df=MAdat$N-2, long=TRUE),
				pcurveEst(t=MAdat$t, df=MAdat$N-2, progress=FALSE, long=TRUE, CI=FALSE),
				puniformEst(t.value=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, skipBarelySignificant=TRUE),
				#onePSM.McShane.est(t.obs=MAdat$t, n1=MAdat$n1, n2=MAdat$n2),
				threePSM.est(d=MAdat$d, v=MAdat$v, min.pvalues=0, long=TRUE),
				fourPSM.est(d=MAdat$d, v=MAdat$v, min.pvalues=0, long=TRUE, fallback=FALSE),
				WAAP.est(d=MAdat$d, v=MAdat$v, long=TRUE)#,
				#betaSM.est(d=MAdat$d, v=MAdat$v, long=TRUE)
			)
			
			## add some extra informations:			
			
			# the average true delta of all positive significant studies (=estimand of pcurve)
			delta.included.M <- mean(MAdat$D[MAdat$p < .05 & MAdat$D >= 0])
			if (is.nan(delta.included.M)) delta.included.M <- NA
			
			res0 <- rbind(res0,
				data.frame(method="pcurve", term="delta.included", variable="mean", value = delta.included.M)	
			)
			
	
			# collect results
			res1 <- cbind(
		
				# save settings of condition to results:
				MAdat[rep(1, nrow(res0)), c("id", "condition", "k", "delta", "qrpEnv", "censor", "tau", "qrp")],
		
				# save analysis results:
				res0
			)
			reslist[[counter]] <- res1
			counter <- counter+1
		}

		res2 <- bind_rows(reslist)
		return(res2)
	} # of dopar
	
	save(res, file=paste0("analysisPartsRev2/analysis_", basename(f)), compress="gzip")
} # of "f in simDatFiles"

