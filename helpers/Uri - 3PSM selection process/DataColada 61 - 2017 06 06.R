##Impact on false-positive rate in meta-analysis when including n.s. result in p-curve style estimation
# www.DataColada.org/61
#
#Written by Uri Simonsohn (uws@upenn.edu)
#
#This version: June 6th, 2017
##################################################
     

#  Code by McShane et al for estimating 3 parameter model
   #Only function I use is "estimate.onestep.selection.heterogeneous()"
      source("http://datacolada.org/wp-content/uploads/2017/06/selection.meta_.functions.r") 
      #This is code posted by McShane et al, used as-is by Uri
      #Also available from http://journals.sagepub.com/doi/suppl/10.1177/1745691616662243

#Function 1. Run one meta analysis
      f=function(
            #Syntax:
               study.tot,    # of studies in to include in meta-analysis
               d.mean,       #True mean effect
               d.sd,         #SD of true effect
               nmax,nmin,    #max/min sample size (drawn with uniform)
               bias,         #Minimum % of studies that must be significat (60% used in the blog)
               q.guess=.2,   #Starting point for estimating share of p>.05 that are published
               full.res=0,   #full.res=1 show results for McShane et al., in full
               #Probability result is published, when not >0 and p<.05, conditional on being obtained:
                 w.pos.mar,  #Probability publish positive estimate with marginal significance (.05<p<.1)
                 w.neg.mar,  #Probability publish negative estimate with marginal significance (.05<p<.1)
                 w.pos.ns,   #Probability publish positive estimate with ns (p>.1)
                 w.neg.ns,   #Probability publish negative estimate with ns (p>.1)
                 w.neg.sig  #Probability publish negative estimate with p<.05
              )
    {
        
    #1) Generate Large number true effect sizes (that way i generate t-values only once per simulation)
         d.true=rnorm(study.tot*100, mean=d.mean,sd=d.sd)       #It's 100 times number of desired studies to ensure enough to drwa from for each possible outcome 
        
    #2) Set sample size
        n=round(runif(study.tot*100,min=nmin-.5, max=nmax+.5),0)
        df=2*n-2
        
    #3) Simulate studies, t-values, using noncentral distribution
        t=mapply(rt,n=1,df=2*n-2,ncp=sqrt(n/2)*d.true)   #use non central t to get random draw of studytresults 
        d=2*t/sqrt(2*n)            #convert t to d
        p=2*(1-pt(abs(t),df=df))   #two-sided p-value
        
    #4)  Make 6 bins
         #There are 6 types of results:  2 (positive/negative) 3 (significant, marginal,ns) 
         #I split the ts above into 6 bins. I then put them together again into a single bin but drawing with 
         #different probabilities from each of the 6 bins, so as to simulate the favoring of certain results over others
        #then i draw the 20 studies that are not *necessarily* p<.05 from that combined bin which has them in the right proportions
         
        #4.1 Bin indicators
          pos=t>0               #positive
          neg=t<0               #negative
          sig=p<.05             #significnat
          mar=p>.05 & p<.1      #marginal
          ns =p>.1              #ns
        
        #4.2 Bin counters (how many results of each type?)
          k.pos.mar=sum(pos & mar)
          k.neg.mar=sum(neg & mar)
          k.pos.ns=sum(pos & ns)
          k.neg.ns=sum(neg &ns)
          k.neg.sig=sum(neg & sig)
					k.pos.sig=sum(pos & sig)
					
					# FS: validity check: must sum to study.tot*100
					k.pos.mar + k.neg.mar + k.pos.ns + k.neg.ns + k.pos.sig + k.neg.sig == study.tot*100
    
        
        #4.3 Big bin 1: positive and significant  (for the share with bias%, the share that must be p<.05)
          #In blog i do as Carter et al and make bias=60%, so >=60% of results in any given meta-analysis must be p<.05
            t.pos.sig=t[sig & pos]
            n.pos.sig=n[sig & pos]
        
        #4.4 Big bin 2 all other results, in their respective proportions 
            
          t.pos.mar=t[pos & mar][1:(round(k.pos.mar*w.pos.mar,0))]  
          t.neg.mar= t[neg & mar][1:(round(k.neg.mar*w.neg.mar,0))] 
          t.pos.ns=t[pos & ns][1:(round(k.pos.ns*w.pos.ns,0))]      
          t.neg.ns=t[neg & ns][1:(round(k.neg.ns*w.neg.ns,0))]
          t.neg.sig=t[neg & sig][1:(round(k.neg.sig*w.neg.sig,0))]
          t.any=c(t.pos.mar,  t.neg.mar,  t.pos.ns, t.neg.ns,  t.neg.sig, t.pos.sig) #Note: this bin also contains pos & sig
          
          #Let's explain with the top one: t.pos.mar=t[pos & mar][1:(round(k.pos.mar*w.pos.mar,0))]  
          #    you take the full vector of t-values,                                   t
          #    you subset it to those that are marginally significant and positive     [pos & mar]
          #    you publish it, conditionaln on obtaining, with probability w.pos.mar and so i take only (k.pos.mar*w.pos.mar)
          #    from this bin back in to the full bin. So if w=50%, then only half of the drawn results that are marginal
          #    and positive are put back into teh bin from which the 20 studies are drawn
        
        #now do the same for sample size (a bit inefficient and redundant, but that's how the code came out)
          n.pos.mar=n[pos & mar][1:(round(k.pos.mar*w.pos.mar,0))]  
          n.neg.mar= n[neg & mar][1:(round(k.neg.mar*w.neg.mar,0))]
          n.pos.ns=n[pos & ns][1:(round(k.pos.ns*w.pos.ns,0))]
          n.neg.ns=n[neg & ns][1:(round(k.neg.ns*w.neg.ns,0))]
          n.neg.sig=n[neg & sig][1:(round(k.neg.sig*w.neg.sig,0))]
          n.any=c(n.pos.mar,  n.neg.mar,  n.pos.ns, n.neg.ns,  n.neg.sig, n.pos.sig)
        
        #5.1 Draw desired number of studies from each bin.
          sig.k=sample(length(t.pos.sig),size=round(study.tot*bias,0))     #Bin 1, necessarily pos and sig
          any.k=sample(length(t.any),size=round(study.tot*(1-bias),0))     #Bin 2, any, in the assumed proportions
          
        #5.2 Combine bins
          t.obs=c(t.pos.sig[sig.k],  t.any[any.k])
          n.obs=c(n.pos.sig[sig.k],  n.any[any.k])
        
          
        #5.3 Compute observed d, and sd(d) for starting parameters in MLE use observed values (as in McShane et al)
          d.obs=2*t.obs/sqrt(2*n.obs)
          d.sd.obs=sd(d.obs)
        
        #5.4 If asked by user, plot all histograms  (this is used as a dashboard thing, where the first simulation is plotted)
        if (full.res==1) 
          {
          par(mfrow=c(4,2))
          hist(t.pos.mar,main=paste0("Positive Marginal k:",length(t.pos.mar)))
          hist(t.neg.mar,main=paste0("Negative Marginal k:",length(t.neg.mar)))
          hist(t.pos.ns,main=paste0("Positive n.s. k:",length(t.pos.ns)))
          hist(t.neg.ns,main=paste0("Negative n.s. k:",length(t.neg.ns)))
          hist(t.pos.sig,main=paste0("Positive sig k:",length(t.pos.sig)))
          hist(t.neg.sig,main=paste0("Negative sig k:",length(t.neg.sig)))
          hist(d.obs,main=paste0("All observed ds:",length(d.obs)))
        }
          
              #Note: there is a bit of a bug where when there is 0% chance of obtaining a result, exactly one is selected instead
              #      i did not fix the bug because it has the nice feature of verifying that "none" are being selected
              #     since it is one of 100s of tests it will have no impact, moreover, if anything, it is conservative
              #     as it makes the simulations epsilon more similar to McShane & Carter's
          
          
    #7) MLE - procedure from McShane et al.
         #Starting values of parameters to be estimated are the observed values and .2 for q
            theta.init = c(mean(d.obs),  max(d.sd.obs,0.01), min(max(q.guess,0.01),0.99)) 
         #Estimation itself         
            mle=estimate.onestep.selection.heterogeneous(t.obs, n.obs, n.obs, alpha=.025, theta.init)
            mle.d =round(mle$est[1],3)  #first parameter estimated is the mean(d)
            mle.se=sqrt(round(mle$est.var[1,1],3))  #this is the standard error
          #Output results
            if (full.res==1) cat("\nmle d | sd(d)  | prob(publish|p>.05):\n",round(mle$est,3),"\n")
          
          c(mean(d.obs),mle.d,mle.se, mle.d/mle.se)
  }
   
  
  #Monte carlo of f(),
  montef=function(simtot,seed, ...)    {   #this runs the f() function many times, seeting a seed
    set.seed(seed)
    #Do one, and show full resultsto get a sense
      f(...,full.res = 1)
      
    #Now do all of them for the montecarlo
      r=matrix(nrow=simtot,ncol=4)           
      for (simk in 1:simtot)
      {
      r[simk,]= f(...) 
      if (simk%%100==0) cat("...",simk)
      }
      cat("\nShare t>  1.96",mean(r[,4]>1.96,na.rm=FALSE))
      cat("\nShare t< -1.96",mean(r[,4]< -1.96,na.rm=FALSE))
      list(r=r,fp.pos=mean(r[,4]>1.96,na.rm=FALSE),fp.neg=mean(r[,4]< -1.96,na.rm=FALSE))
  }
  

  
#NOTE: When the simulations run, results are reported for the very fist simulation, and several histrograms
#      are shown, the goal is to have a sense of what's being simulated

#Case 1. As assumed by p-curve critics: all p-values equally likely to be selected (Run more of this one becuase the probability of p<.05 is lower)
  r1=montef(w.pos.mar=1, w.pos.ns=1, w.neg.mar=1, w.neg.ns=1,    w.neg.sig=1,   simtot=5000,study.tot=50,seed=100,d.mean=0,d.sd=0,nmin=10,nmax=50, bias=.6,q.guess=.2)

#Case 2. Positive results twice as likely to be published
  r2=montef(w.pos.mar=1, w.pos.ns=1, w.neg.mar=.5, w.neg.ns=.5,  w.neg.sig=.5,  simtot=500,study.tot=50,seed=100,d.mean=0,d.sd=0,nmin=10,nmax=50, bias=.6,q.guess=.2)
    
#Case 3. Positive results four times as likely to be published
  r3=montef(w.pos.mar=1, w.pos.ns=1, w.neg.mar=.25, w.neg.ns=.25,w.neg.sig=.25, simtot=500,study.tot=50,seed=100,d.mean=0,d.sd=0,nmin=10,nmax=50, bias=.6,q.guess=.2)

#Case 4. Positive or n.s. negative
  r4=montef(w.pos.mar=1, w.pos.ns=1, w.neg.mar=0,   w.neg.ns=.1,  w.neg.sig=0,   simtot=500,study.tot=50,seed=100,d.mean=0,d.sd=0,nmin=10,nmax=50, bias=.6,q.guess=.2)

  
#Case 5. Only positive results 
  r5=montef(w.pos.mar=1, w.pos.ns=1, w.neg.mar=0,   w.neg.ns=0,  w.neg.sig=0,   simtot=500,study.tot=50,seed=100,d.mean=0,d.sd=0,nmin=10,nmax=50, bias=.6,q.guess=.2)
    t=r5$r
    t=t[!is.na(t)]
    mean(t>1.96)
    mean(t< -1.96)
  #It appears that the optimization fails at times, so i then do it by hand to adjust for nthe NAs  
    
    
