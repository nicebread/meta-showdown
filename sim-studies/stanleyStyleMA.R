stanleyMA = function(N,eff,het,k,biasProp){
  
  #set up ks for each MA
  kBias = floor(k*biasProp)
  kUnbias = k - kBias
  
  dUnbias = vUnbias = pUnbias = rep(NA,kUnbias)
  dBias = vBias = pBias = rep(NA,kBias)
  
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
    n1 = Ni/2
    n2 = Ni/2
    
    S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2)/(n1 + n2 - 2) )
    
    dUnbias[i] = (m1 - m2)/S
    vUnbias[i] = (n1 + n2)/(n1 * n2) + (dUnbias[i]^2)/(2 * (n1 + n2))
    pUnbias[i] = t.test(Ye,Yc)$p.value
    
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
        
        pObs = t.test(Ye,Yc)$p.value
        
        m1 = mean(Ye)
        v1 = var(Ye)
        m2 = mean(Yc)
        v2 = var(Yc)
        n1 = Ni/2
        n2 = Ni/2
        
        S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2)/(n1 + n2 - 2) )
        
        dObs = (m1 - m2)/S
        vObs = (n1 + n2)/(n1 * n2) + (dUnbias[i]^2)/(2 * (n1 + n2))  
        
        if (dObs > 0 && pObs < .05){posSig=1}
      }
      dBias[i] = dObs
      vBias[i] = vObs
      pBias[i] = pObs
    }
  }
  
  #put the unbiased and biased data together for MA j
  d = c(dUnbias,dBias)
  v = c(vUnbias,vBias)
  p = c(pUnbias,pBias)
  
  return(cbind(d,v,p))
  
}
