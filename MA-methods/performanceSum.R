sumSimEst = function(est,lb,ub,truth){
  
  #takes a vector of estimates and CI limits from
  #a set of replicated MAs, as well as a true value (truth)
  #return summary stats (ME, MSE, CovP) as a matrix
  
  out = matrix(NA,1,3)
  colnames(out) = c("ME","MSE","CovP")
  
  out[,1] = mean(est - truth)         
  out[,2] = mean((est - truth)^2)  
  
  n = length(lb)
  truth = matrix(truth,n)
  data = cbind(lb,ub,truth)
  cov = matrix(NA,n) 
  
  for (i in 1:n){
    cov[i] = if(truth >= lb[i] & truth <= ub[i]){1}else{0} 
  }
  
  out[,3] = mean(cov)
  
}


