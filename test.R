# install required packages
# install.packages(c("pwr", "truncnorm", "compiler"))

source("sim-studies/sim-studies.R")

# ---------------------------------------------------------------------
#  compute a simulated set of studies, no bias, no hacking
MA1 <- dataMA(	k = 1000,	# the number of studies in the MA
				QRP = 0,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 0,	# 1 if publication bias selection exists, 0 otherwise
				propB = 0,	# the proportion of the sample affected by bias
                meanD = 0.5,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 400,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 15,	# the min of the truncated normal for sample size
				meanN = 40,	# the mean of the truncated normal for sample size
				sdN = 10,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 0,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 0, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 0,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 0,	# number of times to try collecting more data
				add = 0		# number to add to each group when collecting more data
			)

hist(MA1[, 1])


# ---------------------------------------------------------------------
#  compute a simulated set of studies with bias, no hacking
MA2 <- dataMA(	k = 1000,	# the number of studies in the MA
				QRP = 0,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 1,	# 1 if publication bias selection exists, 0 otherwise
				propB = 1,	# the proportion of the sample affected by bias
                meanD = 0.5,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 400,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 15,	# the min of the truncated normal for sample size
				meanN = 40,	# the mean of the truncated normal for sample size
				sdN = 10,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 0,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 0, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 0,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 0,	# number of times to try collecting more data
				add = 0		# number to add to each group when collecting more data
			)

hist(MA2[, 1])