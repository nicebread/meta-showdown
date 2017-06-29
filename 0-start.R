#source("0-start.R")
#install.packages(c("devtools", "MASS", "pwr", "truncnorm", "truncdist", "dplyr", "data.table", "foreach", "doMC", "meta", "progress", "doMC", "reshape2", "metafor", "notifyR", "broom", "weightr", "doParallel", "gtools", "weightr", "rio")) 

# devtools::install_github("RobbievanAert/puniform")
# installed version 0.0.3 on 2017/06/28

# We need R >= 3.3.3 and weightr version >= 1.1.2!
if(packageVersion("weightr") < "1.1.2") stop("you need to update 'weightr'!")
if(getRversion() < "3.3.3") stop("you need to update R!")

library(MASS)
library(pwr)
library(compiler)
library(truncnorm)
library(truncdist)
library(reshape2)
library(dplyr)
library(data.table)
library(foreach)
library(doMC)
library(meta)
library(metafor)
library(progress)
library(notifyR)
library(broom)
library(puniform)
library(weightr)

source("helpers/helpers.R")
source("sim-studies/sim-studies.R")
source("MA-methods/1-RMA.R")
source("MA-methods/2-p-curve.R")
source("MA-methods/3-PET-PEESE.R")
source("MA-methods/4-p-curve skewness.R")
source("MA-methods/5-p-uniform.R")
source("MA-methods/6-WAAP.R")
source("MA-methods/7b-selection.meta.functions.R")
source("MA-methods/7-Selection Models.R")
#source("MA-methods/8-betaSM.R", chdir=TRUE)	# must chdir=TRUE to load the helper functions in /betaSM_functions

# load empirical sample sizes into workspace
# TODO: This is not quite elegant; should be in data generating function
perGrp <- read.csv("Empirical n and ES distributions/perGrp.csv")
