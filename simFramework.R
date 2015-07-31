# load all functions and packages
source("start.R")

# register CPU cores for parallel processing
registerDoMC(20)
getDoParWorkers()

## ======================================================================
## SETTINGS
## Define parameter space here
## ======================================================================

# ---------------------------------------------------------------------
#  experimental factors: Pub bias, but no hacking
k_set <- c(20, 40, 80, 120)
d_true_set <- c(0.3, 0.5, 0.7)
QRP_set <- c(0, 1)

# params stores all possible combinations of experimental factors
params <- expand.grid(k=k_set, d_true=d_true_set, sel=1, propB=1, QRP=QRP_set)
rownames(params) <- NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))


# other settings
B <- 1000	# number of simulation replications per condition (should be dividable by getDoParWorkers())


## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())


# use %dopar% for parallel processing, or %do% for single thread
# "batch" stores the id of the parallel process
sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %dopar% {    
	
	# how many replications are simulated within each fork?
	b <- round(B/getDoParWorkers())
	
	# res stores the results, pre-allocate memory
	# TODO: adjust ncol to final number of columns
	res <- matrix(NA, nrow=sum(params$k)*b, ncol=17)
	counter <- 1	# counter stores the current position in the results matrix
	
	# run b replications, over the full parameter space
	# (i.e., each fork runs all experimental conditions)
	for (j in 1:nrow(params)) {
		print(paste0(Sys.time(), ", batch=", batch, ": computing condition ", j, "/", nrow(params)))
		for (i in 1:b) {
			
			# ENTER ACTUAL COMPUTATIONS HERE ...
			# dummy computation
			MA1 <- dataMA(k=params[j, "k"], meanD=params[j, "d_true"], sigma=0, maxN=5000,
			                  minN=15, meanN=40, sdN=20,

							  # Parameters for publication bias
							  sel=1, propB=1,
	
							  # parameters for QRP
							  QRP=params[j, "QRP"], cbdv=0, multDV=0, out=0, mod=0, colLim=0, add=0, verbose=TRUE)		

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
	}
	
	colnames(res) <- colnames(res0)
	unique <- 1000*(res[, "batch"]*10^(floor(log10(max(res[, "replication"]))+1)) + res[, "replication"]) + res[, "condition"]
	res2 <- cbind(unique=unique, res)
	
	return(res2)
}


# sanity check: We should have b replications in each experimental cell
data.frame(sim) %>% group_by(condition) %>% summarise(replications=n()/k[1])

save(sim, file="simData/simData.RData")

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
