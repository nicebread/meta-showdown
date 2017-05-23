# without QRPs, sample directly from non-central t-distribution
# https://alexanderetz.com/2016/07/

# load all functions and packages
source("../0-start.R")
source("sim-studies.R")


 ncp = δ * √(1/n1 + 1/n2)
 
 
system.time({
old <-  dataMA(k=100, delta=0.4, tau=0, maxN=50, meanN=50, minN=50, selProp=0, qrpEnv="none", empN=FALSE)
})

new <- rt(10000, df=48, ncp=0.4*sqrt(1/25 + 1/25))