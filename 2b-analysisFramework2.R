## ======================================================================
## This file takes the simulated data sets, which are stored in separate
## files in folder /simParts, and runs all meta-analytic techniques on them.
## Then it saves the analyses in separate files in the /analysisParts folder.
## ======================================================================

# run this file:
# source("2b-analysisFramework2.R", echo=TRUE)

# load all functions and packages
source("start.R")

library(doParallel)
# detectCores()
registerDoParallel(cores=1)

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
			res0 <- rbind(
				reEst(d=MAdat$d, v=MAdat$v, long=TRUE),
				lmVarEst(MAdat$d, MAdat$v, long=TRUE),
				pc_skew(t=MAdat$t, df=MAdat$N-2, long=TRUE),
				puniformEst(t.value=MAdat$t, n1=MAdat$n1, n2=MAdat$n2),
				topN(MAdat$d, MAdat$v, MAdat$n1, MAdat$n2, est="fixed", fixed.effect=0.3),
				topN(MAdat$d, MAdat$v, MAdat$n1, MAdat$n2, est="rma"),
				topN(MAdat$d, MAdat$v, MAdat$n1, MAdat$n2, est="PEESE")	
			)
			
	
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
	
	# send a push notification after each 200 finished conditions:
	if (which(simDatFiles == f) %% 200 == 0) {
	  userkey <- "uY7zyarxM2HoNaTLeX8HXjWvpFA4Cp" #Define user key
	  send_push(userkey, paste0("Condition ", which(simDatFiles == f), " finished"))
	}
} # of "f in simDatFiles"


print(paste0(Sys.time(), ": Finished analyzing."))
userkey <- "uY7zyarxM2HoNaTLeX8HXjWvpFA4Cp" #Define user key
send_push(userkey, "Analyses finished!", title="Linux")


#load("analysisParts/analysis_simData_B1_condition_1.RData")
#res %>% filter(term=="b0", variable=="estimate") %>% ggplot(aes(x=method, y=value)) + geom_point()