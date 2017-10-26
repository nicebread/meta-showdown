
#if summ isn't loaded, load it
if(!exists("summ")){
  print("Loading data...")
  load("summ.RData") 
}

#get necessary packages
require(reshape2)
require(dplyr)

performsPoorly = function(k="any",method,
                          Delta="any",Tau="any",PB="any",QRP="any",
                          metric,performanceLB,performanceUB){
    
  #check that arguments are entered correctly
  if(k!="any" & k!=10 & k!=30 & k!=60 & k!=100){    
    stop("Allowable entries for k are: any, 10, 30, 60, or 100.")
  }  
  if(method!="RE" & method!="TF" & 
       method!="PT" & method!="PE" & 
       method!="PP" & method!="PC" & 
       method!="PU" & method!="3P" & 
       method!="WA"){
    stop("Allowable entries for the method argument are: RE, PT, PE, PP, PC, PU, 3P, WA.")
  }
  if(Delta!= "any" & Delta!= 0 & Delta!=.2 & Delta!=.5 & Delta!=.8){    
    stop("Allowable entries for the Delta argument are: 'any', 0, .2, .5, or .8.")    
  }
  if(Tau!= "any" & Tau!=0 & Tau!=.2 & Tau!=.4){    
    stop("Allowable entries for the Tau argument are: 'any', 0, .2, or .4.")    
  }
  if(PB!= "any" & PB!="none" & PB!="med" & PB!="high"){    
    stop("Allowable entries for the PB argument are: 'any', 'none', 'med', or 'high'.")
  }  
  if(QRP!="any" & QRP!="none" & QRP!="med" & QRP!="high"){    
    stop("Allowable entries for the PB argument are: 'any', 'none', 'med', or 'high'.")
  }  
  if(metric!="ME" & metric!="RMSE" & metric!="Coverage" & metric!="RejectH0"){    
    stop("Allowable entries for the metric argument are: ME, RMSE, Coverage, RejectH0.")
  }
  
  
  #check that performanceLB & performanceUB make sense 
  if(performanceLB>=performanceUB){
    stop("performanceLB needs to be less than performanceUB.")
  }
  if(metric=="ME"){
    if(abs(performanceUB)>=1){
      print("performanceLB or performanceUB may not match the scale of metric")
    }
  }
  if(metric=="RMSE"){
    if(performanceLB<0){
      print("performanceLB or performanceUB may not match the scale of metric")
    }
  }
  if(metric=="Coverage" | metric=="RejectH0"){
    if(performanceLB<0 | performanceUB>1){
      print("performanceLB or performanceUB may not match the scale of metric")
    }
  }
  
  #save the entered term before recoding
  methodEntered = method  
  #recode arguments to match summ.RData
  if(method=="RE"){method="reMA"}  
  if(method=="PT"){method="PET.lm"}
  if(method=="PE"){method="PEESE.lm"}
  if(method=="PP"){method="PETPEESE.lm"}
  if(method=="PC" & metric=="RejectH0"){method="pcurve.evidence"}
  if(method=="PC" & metric!="RejectH0"){method="pcurve"}
  if(method=="PU"){method="puniform"}
  if(method=="3P"){method="3PSM"}
  if(method=="WA"){method="WAAP-WLS"}
  if(PB=="high"){PB="strong"}
  if(PB=="med"){PB="medium"}
  if(metric=="Coverage"){metric="coverage"}
  if(metric=="RejectH0"){metric="H0.reject.rate"}
  
  
  #pull specific conditions by redefining check data frame
  check = summ
  #dim(check)
  if(k!="any"){check = check[check$k==k,]}
  #dim(check)
  if(Delta!="any"){check = check[check$delta==Delta,]}
  #dim(check)
  if(Tau!="any"){check = check[check$tau==Tau,]}
  #dim(check)
  if(PB!="any"){check = check[check$censor==PB,]}
  #dim(check)
  if(QRP!="any"){check = check[check$qrpEnv==QRP,]}
  #dim(check)  
  check = check[check$method==method,]
  
  
  #recode PB so that entries are {none, med, high}
  levels(check$censor)[levels(check$censor)=="medium"] = "med"
  levels(check$censor)[levels(check$censor)=="strong"] = "high"
  
  #get output
  performance = as.vector(check[,metric]<=performanceLB | check[,metric]>=performanceUB)
  out = data.frame(check$k,check$delta,check$tau,check$censor,check$qrpEnv,performance)
  #change output to yes/no
  performance[out[,"performance"]] = "Yes"
  performance[out[,"performance"]==F] = "No"
  out[,"performance"] = performance  
  
  colnames(out)=c("k","Delta","Tau","Pub Bias","QRPs","Poor performance")
  
  print(paste0(methodEntered, " performance where 'good performance' is defined by the ", metric, " interval [", 
               performanceLB, ", ", performanceUB, "]." ))
  return(out)
}



isPerformancePoor = function(){
  
  k = readline(prompt="Enter a sample size (any, 10, 30, 60, 100): ")
  Delta = readline(prompt="Enter a value for Delta (any, 0, .2, .5, .8): ")
  Tau = readline(prompt="Enter a value for Tau (any, 0, .2, .4): ")
  method = readline(prompt="Enter a method (RE, PT, PE, PP, PC, PU, 3P, WA): ")
  PB = readline(prompt="Enter a degree of publication bias (any, none, med, or high): ")
  QRP = readline(prompt="Enter a degree of QRPs (any, none, med, or high): ")
  metric = readline(prompt="Enter a performance metric (ME, RMSE, Coverage, RejectH0): ")
  performanceLB = readline(prompt="Enter a lower limit for 'good performance': ")
  performanceUB = readline(prompt="Enter an upper limit for 'good performance': ")
  
  out = performsPoorly(if(k!="any"){k = as.numeric(k)}else{k},
                       method,
                       if(Delta!="any"){Delta = as.numeric(Delta)}else{Delta},
                       if(Tau!="any"){Tau = as.numeric(Tau)}else{Tau},
                       PB,
                       QRP,
                       metric,
                       as.numeric(performanceLB),
                       as.numeric(performanceUB))
  return(out)
}

print("Run 'isPerformancePoor()'")




