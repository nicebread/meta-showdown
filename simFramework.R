library(dplyr)
library(data.table)

# register CPU cores for parallel processing
library(foreach)
library(doMC)
registerDoMC(2)
getDoParWorkers()


## ======================================================================
## SETTINGS
## Define parameter space here
## ======================================================================

# experimental factors
N_set <- c(20, 40, 80)
d_set <- c(0.2, 0.4, 0.6)
bias_set <- c(0, 1)

# params stores all possible combinations of experimental factors
params <- expand.grid(N=N_set, d=d_set, bias=bias_set)

# other settings
B <- 1000	# number of simulation replications per condition (should be dividable by getDoParWorkers())


## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())


# use %dopar% for parallel processing, or %do% for single thread
# "batch" stores the number of the parallel process
sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %do% {    
	
	# how many replications are simulated within each fork?
	b <- round(B/getDoParWorkers())
	
	# res stores the results
	res <- matrix(NA, nrow=b*nrow(params), ncol=6)
	
	# run b replications, over the full parameter space
	for (j in 1:nrow(params)) {
		for (i in 1:b) {
			
			# ENTER ACTUAL COMPUTATIONS HERE ...
			# dummy computation
			res[(j-1)*b+i, ] <- c(batch=batch, replication=i, params$N[j], params$d[j], params$bias[j], MSE=rnorm(1))
		}
	}
	
	return(res)
}

colnames(sim) <- c("batch", "replication", "N", "d", "bias", "MSE")

# sanity check: We should have b replications in each experimental cell
data.frame(sim) %>% group_by_(.dots=colnames(params)) %>% summarise(n=n())


end <- Sys.time()
print(Sys.time())
print(end - start)
