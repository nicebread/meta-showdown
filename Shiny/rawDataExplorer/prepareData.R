library(gtools)
library(dplyr)

# load all simulation files
# TODO: This should be computed once and saved as intermediate result.
simDatFiles <- list.files("simPartsDemo", pattern=".*\\.RData", full.names=TRUE)
simDatFiles <- mixedsort(simDatFiles)

sim_list <- list()
for (f in simDatFiles) {
	load(f)	# the simulation data frame always is called "sim"
	sim$unique <- as.numeric(factor(paste(sim$batch,"_", sim$replication)))
	sim$replication <- NULL
	sim$batch <- NULL
	sim$id <- NULL
	sim_list[[f]] <- sim
}

sim <- bind_rows(sim_list)
save(sim, file="simDemo.RData")