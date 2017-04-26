library(gtools)
library(dplyr)

# load all simulation files
# TODO: This should be computed once and saved as intermediate result.
simDatFiles <- list.files("simPartsDemo", pattern=".*\\.RData")
simDatFiles <- mixedsort(simDatFiles)

sim_list <- list()
for (f in simDatFiles) {
	load(paste0("simPartsDemo/", f))	# the simulation data frame always is called "sim"
	sim0 <- sim
	sim0$unique <- as.numeric(factor(paste(sim0$batch,"_", sim0$replication)))
	sim0$replication <- NULL
	sim0$batch <- NULL
	sim0$id <- NULL
	
	sim <- sim0 %>% 
		filter(unique <= 10) %>% 
		select(-condition, -p, -t, -N, -se, pow, -n1, -n2, -D, -kFD, -sel, -qrp) %>% 
		mutate(tau.label = factor(tau, levels=unique(tau), labels=paste0("tau = ", unique(tau))))
	
	save(sim, file=paste0("simPartsDemoReduced/", f), compress="gzip")
	
	sim_list[[f]] <- sim
}

sim <- bind_rows(sim_list)

save(sim, file="simDemo.RData", compress="gzip")