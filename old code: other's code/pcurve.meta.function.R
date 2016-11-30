# Code obtained on 02/10/2016 from:
# http://www.p-curve.com/Supplement/Rcode_paper2/APPENDIX%20-%20Loss%20Function%20and%20Estimation.R
# The code has been modified as per the following:
#	(i) The function originally named loss is now named pcurve.loss
#	(ii) The function originally named plotloss is now named pcurve.opt and plotting has been commented out.
#   (iii) The example has been commented out.

 

#LOSS FUNCTION
pcurve.loss=function(t_obs,df_obs,d_est) {
      #################################################################################################
      #SYNTAX:
      #1. t_obs is a vector with observed t-values, 
      #2. df_obs vector with degrees of freedom associated with each t-value
      #3. d_est is the effect size on which fitted p-curve is based and the measure of loss computed
      #################################################################################################
      
      #1.Convert all ts to the same sign (for justification see Supplement 5) 
        t_obs=abs(t_obs)
      
      #2 Compute p-values
        p_obs=2*(1-pt(t_obs,df=df_obs))
      
      #3 Keep significant t-values and corresponding df.
        t.sig=subset(t_obs,p_obs<.05)
        df.sig=subset(df_obs,p_obs<.05)
      
        
      #4.Compute non-centrality parameter implied by d_est and df_obs
      #df+2 is total N. 
      #Becuase the noncentrality parameter for the student distribution is ncp=sqrt(n/2)*d, 
      #we add 2 to d.f. to get N,  divide by 2 to get n, and by 2 again for ncp, so -->df+2/4
        ncp_est=sqrt((df.sig+2)/4)*d_est                          
      
      #5.Find critical t-value for p=.05 (two-sided)
      #this is used below to compute power, it is a vector as different tests have different dfs 
      #and hence different critical values
        tc=qt(.975,df.sig)                     
      
      #4.Find power for ncp given tc, again, this is a vector of implied power, for ncp_est,  for each test
        power_est=1-pt(tc,df.sig,ncp_est)        
      
      #5.Compute pp-values
      #5.1 First get the overall probability of a t>tobs, given ncp
        p_larger=pt(t.sig,df=df.sig,ncp=ncp_est)
      
      #5.2 Now, condition on p<.05
        ppr=(p_larger-(1-power_est))/power_est  #this is the pp-value for right-skew
    
      #6. Compute the gap between the distribution of observed pp-values and a uniform distribution 0,1 
        KSD=ks.test(ppr,punif)$statistic        #this is the D statistic outputted by the KS test against uniform
        return(KSD)          
      }

#Function 2: Estimate d and plot loss function 
pcurve.opt=function(t_obs,df_obs,dmin,dmax){
  #################################################################################################
  #SYNTAX:
  #t_obs  : vector with observed t-values 
  #df_obs : vector with degrees of freedom associated with each t-value
  #dmin   : smallest  effect size to consider 
  #dnax   : largest   effect size to consider 
  #e.g., dmin=-1, dmax=1 would look for the best fitting effect size in the d>=-1 and d<=1 range
  #################################################################################################
  
  #Results will be stored in these vectors, create them first
    loss.all=c()
    di=c()
  
  #Compute loss for effect sizes between d=c(dmin,dmax) in steps of .01    
      for (i in 0:((dmax-dmin)*100))
      {
        d=dmin+i/100                   #effect size being considered
        di=c(di,d)                     #add it to the vector (kind of silly, but kept for symmetry)
        options(warn=-1)               #turn off warning becuase R does not like its own pt() function!
        loss.all=c(loss.all,pcurve.loss(df_obs=df_obs,t_obs=t_obs,d_est=d))
                                       #apply loss function so that effect size, store result
        options(warn=0)                #turn warnings back on
      }
  
  #find the effect leading to smallest loss in that set, that becomes the starting point in the optimize command
      imin=match(min(loss.all),loss.all)       #which i tested effect size lead to the overall minimum?
      dstart=dmin+imin/100                     #convert that i into a d.
  
  #optimize around the global minimum
      dhat=optimize(pcurve.loss,c(dstart-.1,dstart+.1), df_obs=df_obs,t_obs=t_obs)
      options(warn=-0)
  
  #Plot results
      #plot(di,loss.all,xlab="Effect size\nCohen-d", ylab="Loss (D stat in KS test)",ylim=c(0,1), main="How well does each effect size fit? (lower is better)")  
      ###points(dhat$minimum,dhat$objective,pch=19,col="red",cex=2)
      ###text(dhat$minimum,dhat$objective-.08,paste0("p-curve's estimate of effect size:\nd=",round(dhat$minimum,3)),col="red")
      return(dhat$minimum)
}


#Example
  ###t_obs= c(1.7,  2.8,  -3.1,  2.4)  #the first is ignored, the third converted to a positive number
  ###df_obs=c(44,   75,   125,  200)
  ###plotloss(t_obs=t_obs,df_obs=df_obs,dmin=-.5,dmax=2)
  
  t_obs <- MAdat$t[MAdat$t > 0]
  df_obs <- MAdat$N[MAdat$t > 0] - 2
pcurve.opt(t_obs=t_obs,df_obs=df_obs,dmin=-.2,dmax=2)
