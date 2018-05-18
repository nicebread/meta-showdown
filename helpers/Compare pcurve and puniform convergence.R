# Situation: P-curve has a much higher convergence rate than puniform. Theoretically, both should be more or less the same.

# Focus on one condition where the difference in convergence rate is quite high:

#    `{k}` `{$\\delta$}` `{QRP}` `{PB}` `{$\\tau$}` TF    `p-curve` `p-uniform` `3PSM`
#   4   10.         0.    none    med          0.    99%   78%       41%         94%

# --> this is condition 49 (see conditions.txt).

# In which unique runs does pcurve provide an estimate and puniform doesn't?

load(file="../dataFiles/res.wide.RData")
test.analysis <- res.wide.red %>% filter(condition == 49)

# only select pcurve and puniform, spread to wide format
test.analysis.red <- test.analysis %>% 
	select(1:9) %>% 
	filter(method %in% c("pcurve", "puniform")) %>% 
	spread(method, b0_estimate)

prop.table(table(is.na(test.analysis.red$pcurve)))
prop.table(table(is.na(test.analysis.red$puniform)))

critical <- test.analysis.red %>% filter(xor(is.na(pcurve), is.na(puniform)))
head(critical, 30)

load("../simPartsRev2/simData_condition_49.RData")
test.raw <- sim
# check some critical cases:

source("../0-start.R", chdir=TRUE)

test <- test.raw %>% filter(id == 1012049)

pcurveEst(t=test$t, df=test$n1 + test$n2 - 2, progress=FALSE, long=TRUE, CI=FALSE)
puniformEst(t.value=test$t, n1=test$n1, n2=test$n2, skipBarelySignificant=TRUE)