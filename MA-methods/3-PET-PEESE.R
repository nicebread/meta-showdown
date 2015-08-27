lmVarEst = function(d,v){
  
  #analyzes MA data set using two lm() models
  #produces estimates of true effect (and CIs) for
  #PET, PEESE, and PET-PEESE
  #also produces FAT p-value
  
  out = matrix(NA,1,10)
  colnames(out) = c("dPT","lbPT","ubPT","dPE","lbPE","ubPE","dPP","lbPP","ubPP","fatP")
  
  PET = lm(d~sqrt(v),weights=1/v)
  PEESE = lm(d~v,weights=1/v)
  ciPET = confint(PET)
  ciPEESE = confint(PEESE)
  
  usePET = if(summary(PET)$coefficients[7]/2<.05 & as.numeric(PET$coefficients[1])>0){1}else{0}
  
  out[,1] = as.numeric(PET$coefficients[1])
  out[,4] = as.numeric(PEESE$coefficients[1])
  out[,7] = if(usePET==1){out[,1]}else{out[,4]}
  
  out[,2] = ciPET[1]
  out[,3] = ciPET[3]
  
  out[,5] = ciPEESE[1]
  out[,6] = ciPEESE[3]
  
  out[,8] = if(usePET==1){out[,2]}else{out[,5]}
  out[,9] = if(usePET==1){out[,3]}else{out[,6]}
  
  out[,10] = summary(PET)$coefficients[8]

  return(out)
  
}
