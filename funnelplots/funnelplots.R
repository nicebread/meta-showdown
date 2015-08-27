source("../sim-studies/sim-studies.R")
source("../sim-studies/back-selection.R")

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
# null-effect + pub bias

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
			
meta2 <- metagen(H0.pubbias$d, H0.pubbias$se, n.e=H0.pubbias$n1, n.c=H0.pubbias$n2)
meta::funnel(meta2, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta2, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))

meta::funnel(meta2, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5), yaxis="size", log="y")

# ---------------------------------------------------------------------
# True effect + p-hacking, no pub bias

H1.hack <- dataMA(k = 250,	# the number of studies in the MA
				QRP = 1,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 0,	# 1 if publication bias selection exists, 0 otherwise
				propB = 1,	# the proportion of the sample affected by bias
                meanD = 0.5,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 80,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 15,	# the min of the truncated normal for sample size
				meanN = 20,	# the mean of the truncated normal for sample size
				sdN = 20,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 1,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 1, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 1,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 3,	# number of times to try collecting more data
				add = 10	# number to add to each group when collecting more data
			)
			
meta3 <- metagen(H1.hack$d, H1.hack$se)
meta::funnel(meta3, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta3, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))


# ---------------------------------------------------------------------
# True effect + p-hacking + pub bias

H1.hack.bias <- dataMA(k = 250,	# the number of studies in the MA
				QRP = 1,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 1,	# 1 if publication bias selection exists, 0 otherwise
				propB = 1,	# the proportion of the sample affected by bias
                meanD = 0.5,	# the true effect (or the average of the true effects if heterogeneity exists)
				sigma = 0,	# the SD around the true effect
				cbdv = 0.5,	# the correlation between the multiple DVs
				maxN = 80,	# the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
				minN = 15,	# the min of the truncated normal for sample size
				meanN = 20,	# the mean of the truncated normal for sample size
				sdN = 20,	# the SD of the truncated normal for sample size
				
				# hacking parameters
				multDV = 1,	# 1 if multiple DVs as a hack, 0 otherwise
				out = 1, 	# 1 if optional outlier removal as a hack, 0 otherwise
				mod = 1,	# 1 if optional moderator as a hack, 0 otherwise
				colLim = 3,	# number of times to try collecting more data
				add = 10	# number to add to each group when collecting more data
			)
			
meta4 <- metagen(H1.hack.bias$d, H1.hack.bias$se)
meta::funnel(meta4, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta4, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))


# ---------------------------------------------------------------------
# A real effect, no bias

H1.valid <- dataMA(	k = 250,	# the number of studies in the MA
				QRP = 0,	# 1 if QRP/p-hacks are available, 0 otherwise
				sel = 0,	# 1 if publication bias selection exists, 0 otherwise
				propB = 0,	# the proportion of the sample affected by bias
                meanD = 0.5,	# the true effect (or the average of the true effects if heterogeneity exists)
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
			
meta5 <- metagen(H1.valid$d, H1.valid$se)
meta::funnel(meta5, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta5, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
