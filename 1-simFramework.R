# run this file:
# source("1-simFramework.R", echo=TRUE)

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
B <- 10	# number of simulation replications per condition (should be dividable by getDoParWorkers())


## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())
cat(paste0("New simulation started at: ", start), file="output.txt", append=FALSE, sep = "\n")



for (j in 335:nrow(params)) {
	log1 <- paste0(Sys.time(), ", NEW CONDITION: computing condition ", j, "/", nrow(params))
	print(log1)
	cat(log1, file="output.txt", append=TRUE, sep = "\n")

	# use %dopar% for parallel processing, or %do% for single thread
	# "batch" stores the id of the parallel process
	sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %dopar% {    
	
		# how many replications are simulated within each fork?
		b <- round(B/getDoParWorkers())
	
		# res stores the results, pre-allocate memory
		# TODO: adjust ncol to final number of columns
		#res <- matrix(NA, nrow=sum(params$k[j])*b, ncol=21)
		res <- data.frame()
		#counter <- 1	# counter stores the current position in the results matrix
	
		for (i in 1:b) {
			
			log2 <- paste0(Sys.time(), ", batch=", batch, ": computing condition ", j, "/", nrow(params), "; rep = ", i)
			print(log2)
			cat(log2, file="output.txt", append=TRUE, sep = "\n")
			
			MA1 <- dataMA(k=params[j, "k"], delta=params[j, "delta"], tau=params[j, "tau"], empN=TRUE, maxN=500, minN=0, meanN=0, selProp=params[j, "selProp"], qrpEnv=params[j, "qrpEnv"])

							  
			# remove rownames (otherwise cbind complains)
			rownames(MA1) <- NULL
			p <- params[j, ]
			rownames(p) <- NULL
			
			
			# combine sim settings and results
			res0 <- cbind(
	  			  batch		= batch, 
	  			  replication	= i, 
				  condition	= j,
		  
	  			  # settings of the condition
	  			  p,
			
	  			  # results of the computation
	  			  as.matrix(MA1))

			  # collect results in the matrix
			  #res[counter:(counter+nrow(MA1)-1), ] <- res0
			  #counter <- counter+nrow(MA1)
			  res <- rbind(res, res0)
		} # of b-loop

		#colnames(res) <- colnames(res0)
		return(res)
	} # of foreach loop
		
	sim <- sim %>% mutate(id=1000*(batch*10^(floor(log10(max(replication))+1)) + replication) + condition)	
	save(sim, file=paste0("simPartsDemo/simData_condition_", j, ".RData"), compression="gzip")
	
	# send a push notification after each finished condition:
	# userkey <- "uY7zyarxM2HoNaTLeX8HXjWvpFA4Cp" #Define user key
	# send_push(userkey, paste0("Condition ", j, "/", nrow(params), " finished"))
} # of j (loop through parameter combinations)


# ---------------------------------------------------------------------
# batch = ID of the parallel process
# replication = ID of replication within batch
# id = unique ID for each meta-analysis (across parallel forks)
# condition = ID of condition (= row of "params")
# k d_true sel propB QRP = settings of the data generating process
# d p t N v se pow n1 n2 = output of dataMA function

end <- Sys.time()
print(Sys.time())
print(end - start)
