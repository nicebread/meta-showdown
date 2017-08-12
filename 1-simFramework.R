## ======================================================================
## This file generates the simulated data sets, which are stored in separate
## files in folder /simParts
## ======================================================================

# run this file:
# source("1-simFramework.R", echo=TRUE)

# load all functions and packages
source("0-start.R")

# register CPU cores for parallel processing
library(doParallel)
registerDoParallel(cores=20)

## ======================================================================
## SETTINGS
## Define parameter space here
## ======================================================================

# ---------------------------------------------------------------------
#  experimental factors
k_set <- c(10, 30, 60, 100)							# number of studies in each MA
delta_set <- c(0, .2, .5, .8)						# true mean of effect sizes
qrpEnv_Set <- c("none", "med", "high")	# QRP environment
censor_set <- c("0", "A", "B")					# publication bias
tau_set <- c(0, .2, .4)									# heterogeneity; assumed to follow a normal distribution

# params stores all possible combinations of experimental factors
params <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, censor=censor_set, tau=tau_set)
rownames(params) <- NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))

# write conditions to file
sink(file("conditions.txt", open = "wt"))
print(data.frame(condition=1:nrow(params), params), row.names=FALSE)
sink()


# other settings
B <- 1000	# number of simulation replications per condition (should be dividable by getDoParWorkers())


## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())

for (j in 1:nrow(params)) {
	log1 <- paste0(Sys.time(), ", NEW CONDITION: computing condition ", j, "/", nrow(params))
	print(log1)

	# use %dopar% for parallel processing, or %do% for single thread
	# "batch" stores the id of the parallel process
	sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %dopar% {    
	
		# how many replications are simulated within each fork?
		b <- round(B/getDoParWorkers())
	
		# res stores the results
		res <- data.frame()
	
		for (i in 1:b) {
			
			log2 <- paste0(Sys.time(), ", batch=", batch, ": computing condition ", j, "/", nrow(params), "; rep = ", i)
			print(log2)
			
			MA1 <- simMA(k=params[j, "k"], delta=params[j, "delta"], tau=params[j, "tau"], empN=TRUE, maxN=500, minN=0, meanN=0, censorFunc=as.character(params[j, "censor"]), qrpEnv=params[j, "qrpEnv"])

							  
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

			  res <- rbind(res, res0)
		} # of b-loop

		return(res)
	} # of foreach loop
		
	sim <- sim %>% mutate(id=1000*(batch*10^(floor(log10(max(replication))+1)) + replication) + condition)	
	save(sim, file=paste0("simParts/simData_condition_", j, ".RData"), compress="gzip")
	
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
