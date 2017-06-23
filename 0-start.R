#source("0-start.R")
#install.packages(c("MASS", "pwr", "truncnorm", "truncdist", "dplyr", "data.table", "foreach", "doMC", "meta", "progress", "doMC", "reshape2", "metafor", "notifyR", "broom", "weightr", "doParallel", "gtools")) 

# devtools::install_github("RobbievanAert/puniform")
# installed version 0.0.2 on 2017/05/12

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

source("helpers/helpers.R")
source("sim-studies/sim-studies.R")
source("MA-methods/1-RMA.R")
source("MA-methods/2-p-curve.R")
source("MA-methods/3-PET-PEESE.R")
source("MA-methods/4-p-curve skewness.R")
source("MA-methods/5-p-uniform.R")
source("MA-methods/6-WAAP.R")
source("MA-methods/7b-selection.meta.functions.R")
source("MA-methods/7-3PSM.R")
#source("MA-methods/8-betaSM.R", chdir=TRUE)	# must chdir=TRUE to load the helper functions in /betaSM_functions

# load empirical sample sizes into workspace
perGrp <- read.csv("Empirical n and ES distributions/perGrp.csv")
