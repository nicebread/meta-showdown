stanleyMA = function(N,eff,het,k,biasProp){
  
  #set up ks for each MA
  kBias = floor(k*biasProp)
  kUnbias = k - kBias
  
  dUnbias = vUnbias = pUnbias = tUnbias = nUnbias = rep(NA,kUnbias)
  dBias = vBias = pBias = tBias = nBias = rep(NA,kBias)
  
  for(i in 1:kUnbias){
    
    #get the unbiased data for MA j
    Ni = sample(N,1)
    
    X3e=86.603*rnorm(Ni,0,1)+300  #Can toss 300, 86.603 tunes so 20 and 50 are .2 and .5 true effects     
    X3=86.603*rnorm(Ni,0,1)+300   #     
    
    Te = eff + het * rnorm(1,0,1)
    
    Ye = Te + X3e + 50*rnorm(Ni,0,1)
    Yc =      X3  + 50*rnorm(Ni,0,1)
    
    m1 = mean(Ye)
    v1 = var(Ye)
    m2 = mean(Yc)
    v2 = var(Yc)
    n1 = Ni
    n2 = Ni
    df = 2*Ni-2
    
    S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2) / df )
    
    test = t.test(Ye,Yc)
    
    dUnbias[i] = (m1 - m2)/S
    vUnbias[i] = (n1 + n2)/(n1 * n2) + (dUnbias[i]^2 / (2 *df)) * (n1 + n2) / df
    pUnbias[i] = test$p.value
    tUnbias[i] = as.numeric(test$statistic)
    nUnbias[i] = 2*Ni
  }
  
  if (biasProp>0){
    for(i in 1:kBias){
      
      #get the biased data for MA j
      posSig = 0
      while (posSig == 0){
        
        Ni = sample(N,1)
        
        X3e=86.603*rnorm(Ni,0,1)+300      
        X3=86.603*rnorm(Ni,0,1)+300       
        
        Te = eff + het * rnorm(1,0,1)
        
        Ye=Te+X3e+50*rnorm(Ni,0,1)
        Yc=X3+50*rnorm(Ni,0,1)
        
        test = t.test(Ye,Yc)
        pObs = test$p.value
        tObs = as.numeric(test$statistic)
        
        m1 = mean(Ye)
        v1 = var(Ye)
        m2 = mean(Yc)
        v2 = var(Yc)
        n1 = Ni
        n2 = Ni
        df = 2*Ni-2
        
        S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2) / df )
        
        dObs = (m1 - m2)/S
        vObs = (n1 + n2)/(n1 * n2) + (dObs^2 / (2 *df)) * (n1 + n2) / df 
        
        #test null of d being zero or less (uses different variance)
        #does so by checking if LL on ci around d is > 0
        v = (n1 + n2)/(n1 * n2) * (n1 + n2) / df 
        posSig = if ((dObs - 1.96*sqrt(v)) > 0){1}else{0}
        
      }
      
      dBias[i] = dObs
      vBias[i] = vObs
      pBias[i] = pObs
      tBias[i] = tObs
      nBias[i] = 2*Ni
      
    }
  }
  
  #put the unbiased and biased data together for MA j
  d = c(dUnbias,dBias)
  v = c(vUnbias,vBias) 
  p = c(pUnbias,pBias) #p-value from t test
  t = c(tUnbias,tBias) #t value from t test
  N = c(nUnbias,nBias) #total sample size
  
  return(cbind(d,v,p,t,N))
  
}

