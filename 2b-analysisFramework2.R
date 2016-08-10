## ======================================================================
## This file takes the simulated data sets, which are stored in separate
## files in folder /simParts, and runs all meta-analytic techniques on them.
## Then it saves the analyses in separate files in the /analysisParts folder.
## ======================================================================

# run this file:
# source("2-analysisFramework.R", echo=TRUE)

# load all functions and packages
source("start.R")

library(doParallel)
# detectCores()
registerDoParallel(cores=2)

(ncores <- getDoParWorkers())	# number of parallel processes

# simDatFiles stores the names of all simulated data files in the folder "simParts"
simDatFiles <- list.files("simParts", pattern=".*\\.RData", full.names=TRUE)

library(gtools)
simDatFiles <- mixedsort(simDatFiles)
# f <- simDatFiles[[33]]


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
			re.est <- reEst(d=MAdat$d, v=MAdat$v, long=TRUE)
			lm.est <- lmVarEst(MAdat$d, MAdat$v, long=TRUE)
			pcurve.skew <- pc_skew(t=MAdat$t, df=MAdat$N-2, long=TRUE)
	
			# combine analysis results
			res0 <- rbind(re.est, lm.est, pcurve.skew)
	
			# collect results
			res1 <- cbind(
		
				# save settings of condition to results:
				MAdat[rep(1, nrow(res0)), c("id", "condition", "k", "delta", "qrpEnv", "selProp", "tau", "kFD", "sel", "qrp")],
		
				# save analysis results:
				res0
			)
			reslist[[counter]] <- res1
			counter <- counter+1
		}

		res2 <- bind_rows(reslist)
		return(res2)
	} # of dopar
	
	save(res, file=paste0("analysisParts/analysis_", basename(f)))
} # of "f in simDatFiles"


print(paste0(Sys.time(), ": Finished analyzing."))


load("analysisParts/analysis_simData_B1_condition_1.RData")
