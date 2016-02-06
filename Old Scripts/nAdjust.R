adjustN = function(delta,tau,empN,maxN,meanN,minN,strat,colLim,add){
  
  cbdv = .5
  
  #get data for a study using QRPs
  G = expDataB(delta,tau,cbdv,maxN)
  
  #determine the starting per-group sample size
  #using either a specified distribution OR the empirical distribition
  if (empN==T){
    s = sample(perGrp$x,1)
  }else{
    s = rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
  }
  
  #s is divided in half
  s = round(s/2)
  nInitial = s
  
  
  if(strat=='mod'){
    
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],      # the study-lvl true effect
               multDV=1,out=0,mod=0) # MODERATE 
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      if(a[1] > 0 & a[2] < .05){break}
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=0,mod=0) # MODERATE
    }
  }

  if(strat=='agg'){
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],      # the study-lvl true effect
               multDV=1,out=1,mod=1) # AGGRESIVE 
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      if(a[1] > 0 & a[2] < .05){break}
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=1,mod=1) # AGGRESSIVE
    }
  }
  
  nAdjust = c(a[8], a[8]/2, nInitial, a[8]/2-nInitial, a[1], a[2])
  return(nAdjust)
  
}
