cmpStanMA = cmpfun(stanleyMA)

detectCores()

cl = makeCluster(detectCores())

registerDoParallel(cl)

getDoParWorkers()




## ======================================================================
## SETTINGS
## Define parameter space here
## ======================================================================

# ---------------------------------------------------------------------
#  experimental factors: Pub bias, but no hacking

kPer = c(20) #he also does k = 80
BIAS = c(0,.5)
EFF = c(0,20,50)
HET = c(0, 6.25, 12.5, 25, 50)
N =  c(32,64,125,250,500) #c(20, 30, 40, 50, 150

# params stores all possible combinations of experimental factors
params = expand.grid(HET = HET,kPer = kPer,EFF = EFF,BIAS = BIAS)
rownames(params) = NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))

# other settings
B <- 10000  # number of simulation replications per condition (should be dividable by getDoParWorkers())



## ======================================================================
## THE SIMULATION
## ======================================================================

print(start <- Sys.time())


# use %dopar% for parallel processing, or %do% for single thread
# "batch" stores the id of the parallel process
sim <- foreach(batch=1:getDoParWorkers(), .combine=rbind) %dopar% {    
  
  #require(doParallel) #had to add this or it wouldn't run
  
  # how many replications are simulated within each fork?
  b <- round(B/getDoParWorkers())
  
  # res stores the results, pre-allocate memory
  # TODO: adjust ncol to final number of columns
  res <- matrix(NA, nrow=sum(params$kPer)*b, ncol=10)
  counter <- 1	# counter stores the current position in the results matrix
  
  # run b replications, over the full parameter space
  # (i.e., each fork runs all experimental conditions)
  for (j in 1:nrow(params)) {
    
    print(paste0(Sys.time(), ", batch=", batch, ": computing condition ", j, "/", nrow(params)))
    
    for (i in 1:b) {
      
      MA1 = cmpStanMA(N, 
                      eff = params[j, "EFF"], 
                      het = params[j, "HET"], 
                      k = params[j, "kPer"], 
                      biasProp = params[j, "BIAS"])
      
      #MA1 = stanleyMA(N, 
      #                eff = params[j, "EFF"], 
      #                het = params[j, "HET"], 
      #                k = params[j, "kPer"], 
      #                biasProp = params[j, "BIAS"]) 		
      
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


save(sim, file="C:/Users/ecarter/Documents/Sim MA/stanleyMA.RData")
end <- Sys.time()
print(Sys.time())
print(end - start)
