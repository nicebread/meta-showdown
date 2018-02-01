

# We recommend that meta-analysts and consumers of meta-analyses 
# focus on the question, "Do my conclusions depend on a meta-analytic 
# method that performs poorly in plausible conditions?" If the answer 
# is "yes," then those conclusions should clearly be revisited. This 
# script is designed to help ask this question using the simulated data
# from "Carter, Schönbrodt, Hilgard, & Gervais, 'Correcting for bias in 
# psychology: A comparison of meta-analytic methods.'"

# Set your working directory to the folder that contains both
#   1. The summ.RData file
#   2. The isPerformancePoor_Functions.R file
setwd("<the correct directory>")

# Run the following to load the necessary data, packages, and functions
source("isPerformancePoor_Functions.R")

# Run the next line and follow the prompts to define the conditions 
isPerformancePoor()

# OR run performPoorly() directly. For example, to run the example in 
# the main text, uncomment and run the following
#performsPoorly(k=30,method="RE",metric="ME",PB="any",QRP="any",performanceLB=-.1,performanceUB=.1)
#performsPoorly(k=30,method="WA",metric="ME",PB="any",QRP="any",performanceLB=-.1,performanceUB=.1)
#performsPoorly(k=30,method="TF",metric="ME",PB="any",QRP="any",performanceLB=-.1,performanceUB=.1)


