# run this file:
# source("simFramework.R", echo=TRUE)

# load all functions and packages
source("start.R")

# register CPU cores for parallel processing
library(doParallel)
registerDoParallel(cores=2)

## ======================================================================
## SETTINGS
## Define parameter space here
## ======================================================================

# ---------------------------------------------------------------------
#  experimental factors
# Delta = 0, .2, .5, .8
# Tau = 0, .25, .5
# k = 10, 30, 60, 100
# selProp = 0, .6, 1
# qrpEnv = none, med, high

k_set <- c(10, 30, 60, 100)
delta_set <- c(0, .2, .5, .8)
qrpEnv_Set <- c("none", "med", "high")
selProp_set <- c(0, .6, 1)
tau_set <- c(0, .25, .5)

# params stores all possible combinations of experimental factors
# Here, I always use strong pub bias with 100%
params <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, selProp=selProp_set, tau=tau_set)
rownames(params) <- NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))


# other settings
B <- 1000	# number of simulation replications per condition (should be dividable by getDoParWorkers())


## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())
cat(paste0("New simulation started at: ", start), file="output.txt", append=FALSE, sep = "\n")


# use %dopar% for parallel processing, or %do% for single thread
# "batch" stores the id of the parallel process
sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %dopar% {    
	
	# how many replications are simulated within each fork?
	b <- round(B/getDoParWorkers())
	
	# res stores the results, pre-allocate memory
	# TODO: adjust ncol to final number of columns
	res <- matrix(NA, nrow=sum(params$k)*b, ncol=21)
	counter <- 1	# counter stores the current position in the results matrix
	
	# run b replications, over the full parameter space
	# (i.e., each fork runs all experimental conditions)
	for (j in 1:nrow(params)) {
		log1 <- paste0(Sys.time(), ", NEW CONDITION: batch=", batch, ": computing condition ", j, "/", nrow(params))
		print(log1)
		cat(log1, file="output.txt", append=TRUE, sep = "\n")
		for (i in 1:b) {
			
			log2 <- paste0(Sys.time(), ", batch=", batch, ": computing condition ", j, "/", nrow(params), "; rep = ", i)
			print(log2)
			cat(log2, file="output.txt", append=TRUE, sep = "\n")
			
			# ENTER ACTUAL COMPUTATIONS HERE ...
			# dummy computation
			
			#' @param k the number of studies in the MA
			#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
			#' @param tau the SD around the true effect
			#' @param empN a logical, whether to use the empirical per-group N distribution
			#' @param maxN the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
			#' @param minN the min of the truncated normal for sample size
			#' @param meanN the average of the truncated normal for sample size
			#' @param selProp the proportion of the sample affected by bias
			#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'

			# dataMA <- function(k,delta,tau,
			#                   empN,maxN,meanN,minN,
			#                   selProp,qrpEnv) 
			
			MA1 <- dataMA(k=params[j, "k"], delta=params[j, "delta"], tau=params[j, "tau"], empN=TRUE, maxN=500, minN=0, meanN=0, selProp=params[j, "selProp"], qrpEnv=params[j, "qrpEnv"])

							  

		  # remove rownames (otherwise cbind complains)
		  rownames(MA1) <- NULL
		  p <- params[j, ]
		  rownames(p) <- NULL
			
			
		  # combine sim settings and results
		  res0 <- as.matrix(cbind(
		  			  batch		= batch, 
		  			  replication	= i, 
					  condition	= j,
			  
		  			  # settings of the condition
		  			  p,
				
		  			  # results of the computation
		  			  as.matrix(MA1)))

		  # collect results in the matrix
		  res[counter:(counter+nrow(MA1)-1), ] <- res0
		  counter <- counter+nrow(MA1)
		}
	} # of j (loop through paramneter combinations)
	
	# send a push notification after each finished condition:
	userkey <- "[YOUR PUSHOVER USER KEY]" #Define user key
	send_push(userkey, paste0("Condition ", j, "/", nrow(params), " finished"))
	
	colnames(res) <- colnames(res0)
	unique <- 1000*(res[, "batch"]*10^(floor(log10(max(res[, "replication"]))+1)) + res[, "replication"]) + res[, "condition"]
	res2 <- cbind(unique=unique, res)
	
	return(res2)
}


# sanity check: We should have b replications in each experimental cell
data.frame(sim) %>% group_by(condition) %>% summarise(replications=n()/k[1])

save(sim, file="simData/simData_2015-10-26.RData")

# ---------------------------------------------------------------------
# batch = ID of the parallel process
# replication = ID of replication within batch
# unique = unique ID for each meta-analysis (across parallel forks)
# condition = ID of condition (= row of "params")
# k d_true sel propB QRP = settings of the data generating process
# d p t N v se pow n1 n2 = output of dataMA function

end <- Sys.time()
print(Sys.time())
print(end - start)
