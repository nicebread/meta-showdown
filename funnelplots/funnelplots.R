source("../sim-studies/sim-studies.R")
source("../sim-studies/back-selection.R")
library(meta)
library(dplyr)
set.seed(0xBEEF)

# ---------------------------------------------------------------------
# A real null-effect funnel plot

H0.valid <- dataMA(	k = 250,	# the number of studies in the MA
				QRP = 0,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 0,	# 1 if publication bias selection exists, 0 otherwise
				propB = 0,	# the proportion of the sample affected by bias
                meanD = 0,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 3000,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 20,	# the min of the truncated normal for sample size
				meanN = 50,	# the mean of the truncated normal for sample size
				sdN = 100,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 0,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 0, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 0,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 0,	# number of times to try collecting more data
				add = 0		# number to add to each group when collecting more data
			)
			
meta1 <- metagen(H0.valid$d, H0.valid$se)
meta::funnel(meta1, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta1, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))


# ---------------------------------------------------------------------
# A real null-effect funnel plot

H0.pubbias <- dataMA(	k = 250,	# the number of studies in the MA
				QRP = 0,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 1,	# 1 if publication bias selection exists, 0 otherwise
				propB = 1,	# the proportion of the sample affected by bias
                meanD = 0,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 3000,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 20,	# the min of the truncated normal for sample size
				meanN = 50,	# the mean of the truncated normal for sample size
				sdN = 100,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 0,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 0, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 0,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 0,	# number of times to try collecting more data
				add = 0		# number to add to each group when collecting more data
			)
			
meta2 <- metagen(H0.pubbias$d, H0.pubbias$se)
meta::funnel(meta2, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta2, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))