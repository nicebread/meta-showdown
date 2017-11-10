library(gtools)
library(dplyr)

# load all simulation files, grab the first ten replications from each condition, and save them as a reduced data set
# TODO: This should be computed once and saved as intermediate result.
simDatFiles <- list.files("../../simParts", pattern=".*\\.RData", full.name=TRUE)
simDatFiles <- mixedsort(simDatFiles)

for (f in simDatFiles) {
	print(f)
	load(f)	# the simulation data frame always is called "sim"
	sim0 <- sim
	sim0$unique <- as.numeric(factor(paste(sim0$batch,"_", sim0$replication)))
	sim0$replication <- NULL
	sim0$batch <- NULL
	sim0$id <- NULL
	
	sim <- sim0 %>% 
		filter(unique <= 10) %>% 
		select(-condition, -p, -t, -N, -se, pow, -n1, -n2, -D) %>% 
		mutate(tau.label = factor(tau, levels=unique(tau), labels=paste0("tau = ", unique(tau))))
	
	save(sim, file=paste0("demoDat/", basename(f)), compress="gzip")	
}

