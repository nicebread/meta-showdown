reEst = function(d,v){
  
  #analyzes MA data set using standard RE model estimators
  #produces estimate of true effect, CI around estimate,
  #and estimate of tau (could get CIs if wanted)
 
  
  out = matrix(NA,1,4)
  colnames(out) = c("dRE","lbRE","ubRE","tauRE")
  
  reMA = rma(d,v,method="DL")
  
  out[1,1] = as.numeric(reMA$b[,1])
  out[1,2] = reMA$ci.lb
  out[1,3] = reMA$ci.ub
  
  out[1,4] = as.numeric(sqrt(reMA$tau2))

  return(out)
  
}
