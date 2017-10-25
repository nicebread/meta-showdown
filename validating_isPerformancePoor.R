



source("isPerformancePoor_Functions.R")

k_set = c("any",10,30,60,100)
delta_set = c("any",0,.2,.5,.8)
tau_set = c("any",0,.2,.4)
method_set = c("RE", "PT", "PE", "PP", "PC", "PU", "3P", "WA")
pb_set = c("any", "none", "med", "high")
qrp_set = c("any", "none", "med", "high")
metric_set = c("ME", "RMSE", "Coverage", "RejectH0")
performanceLB_set = 0.01
performanceUB_set = 0.10

cond = as.matrix(expand.grid(k=k_set,
                   delta=delta_set,
                   tau=tau_set,
                   method=method_set,
                   pb=pb_set,
                   qrp=qrp_set,
                   metric=metric_set,
                   perfLB=performanceLB_set,
                   perfUB=performanceUB_set))

check = rep(NA,dim(cond)[1])
for(i in 1:dim(cond)[1]){
  
  k = cond[i,"k"]                              
  method = cond[i,"method"]
  Delta = cond[i,"delta"]
  Tau = cond[i,"tau"]
  PB = cond[i,"pb"]
  QRP = cond[i,"qrp"]
  metric = cond[i,"metric"]
  performanceLB = cond[i,"perfLB"]
  performanceUB = cond[i,"perfUB"]
  
  test = performsPoorly(if(k!="any"){k = as.numeric(k)}else{k},
                 method,
                 if(Delta!="any"){Delta = as.numeric(Delta)}else{Delta},
                 if(Tau!="any"){Tau = as.numeric(Tau)}else{Tau},
                 PB,
                 QRP,
                 metric,
                 as.numeric(performanceLB),
                 as.numeric(performanceUB))
  
  check[i] = length(test[,1])
}
