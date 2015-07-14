#R Code to compute confidence interval for power 




#QUICK DRAFT OF CODE, USE WITH CARE. WRITTEN FOR WILL GERVAIS.
#JULY 2ND, 2015
#

#
# THIS CODE WAS WRITTEN BASED ON THE POSTED R-CODE FOR ESTIMATING POWER USING P-CURVE
# IT CONSTRUCTS CONFIDENCE INTERVALS BY BOOTSTRAPPING FROM A SET OF STUDIES.
# SO FOR EXAMPLE, IF THERE ARE 10 STUDIES, IT DRAWS AT RANDOM WITH REPLACEMENT FROM THOSE 10 STUDIES, 10 STUDIES.
# COMPUTES AVERAGE POWER FOR THE SAMPLE OF 10 STUDIES
# REPEATS, SAY, 1000 TIMES
# THE 2.5TH AND 97.5TH PERCENTILE OF ESTIMATES FOR THE RANDOM SAMPLES IS THE BOOTSTRAPPED CONFIDENCE INTERVAL
#
##################################################################

#SET OF FUNCTIONS 1
#COMPUTE GAP BETWEEN POWER AND DESIRED POWER FOR A GIVEN NCP 
# (minimize these in the next step to solve for the ncp that gives the desired power)
    ncp_error.t = function(delta, power, x, df)      pt(x, df = df, ncp = delta) - (1-power)   #if this equals 0, we found the ncp.
    ncp_error.f = function(delta, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)   
    ncp_error.c = function(delta, power, x, df)      pchisq(x, df = df, ncp = delta) - (1-power)   
    ncp_error.z = function(delta, power, x)          pnorm(x, mean = delta,sd=1) - (1-power)   
  
#SET OF FUNCTIONS 2: MINIMIZE FUNCTIONS ABOVE
  #t-test
      getncp.t =function(df, power)   {      
        xc=qt(p=.975, df=df) # critical t-value
        return(uniroot(ncp_error.t, c(0, 37.62), x = xc, df =df, power=power)$root)   }  
      
  #F-test
    getncp.f =function(df1,df2, power)   {      
      xc=qf(p=.95, df1=df1,df2=df2) # critical F-value
      return(uniroot(ncp_error.f, c(0, 37.62), x = xc, df1 = df1,df2=df2, power=power)$root)  }
    
    
  #chisq-test
    getncp.c =function(df, power)   {      
        xc=qchisq(p=.95, df=df) # critical c-value
        return(uniroot(ncp_error.c, c(0, 37.62), x = xc, df = df, power=power)$root)   }
    
  #Normal
    getncp.z =function(power)   {      
      xc=qnorm(p=.975) # critical Z-value with df=1
      return(uniroot(ncp_error.z, c(0, 37.62), x = xc, power=power)$root)   }             


####################################################################################################################################
# (STEP 2) CREATE PP-VALUES FOR EACH OF THE FOUR DISTRIBUTIONS FOR HOW WELL A GIVEN POWER_EST FITS 
  powerfit.t=function(t_obs, df_obs, power_est)    {
            ncp_est=mapply(getncp.t,df=df_obs,power=power_est)  #find ncp for each  that gives each test power.k
            p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)            #prob t>tobs given ncp_est
            ppr=(p_larger-(1-power_est))/power_est              #condition on p<.05
            return(ppr)   }

  powerfit.f=function(f_obs, df1_obs, df2_obs, power_est)    {
            ncp_est=mapply(getncp.f,df1=df1_obs, df2=df2_obs,power=power_est)  #find ncp for each  that gives each test power.k
            p_larger=pf(f_obs,df1=df1_obs,df2=df2_obs, ncp=ncp_est)        #prob t>tobs given ncp_est
            ppr=(p_larger-(1-power_est))/power_est          #condition on p<.05
            return(ppr)   }

  powerfit.z=function(z_obs, power_est)    {
      ncp_est=mapply(getncp.z,power=power_est)  
      p_larger=pnorm(z_obs,mean=ncp_est)        
      ppr=(p_larger-(1-power_est))/power_est          
      return(ppr)     }
    

  powerfit.c=function(c_obs, df_obs, power_est)    {
      ncp_est=mapply(getncp.c,df=df_obs,power=power_est)  
      p_larger=pchisq(c_obs,df=df_obs,ncp=ncp_est)        
      ppr=(p_larger-(1-power_est))/power_est          
      return(ppr)   }

####################################################################################################################################
# ASSESS FIT FOR A BOOTSTRAPPED SET OF STUDIES (DRAW WITH REPLACEMENT FROM THE ORIGINAL SET)
  
  powerfit.all.boot=function(power_est)
    {
    ppr.all=c()
  #for each kind of test, create random subset of same size
      if (length(t.value.sig)>0) rand.t=sample(length(t.value.sig),replace="TRUE")
      if (length(f.value.sig)>0) rand.f=sample(length(f.value.sig),replace="TRUE")
      if (length(z.value.sig)>0) rand.z=sample(length(z.value.sig),replace="TRUE")
      if (length(c.value.sig)>0) rand.c=sample(length(c.value.sig),replace="TRUE")
  #Now apply the fit to the random subsets of each
    if (length(t.value.sig)>0) ppr.all=c(ppr.all, powerfit.t(t_obs=t.value.sig[rand.t], df_obs=t.df.sig[rand.t], power_est=power_est))
    if (length(f.value.sig)>0) ppr.all=c(ppr.all, powerfit.f(f_obs=f.value.sig[rand.f], df1_obs=f.df1.sig[rand.f], df2_obs=f.df2.sig[rand.f], power_est=power_est))
    if (length(z.value.sig)>0) ppr.all=c(ppr.all, powerfit.z(z_obs=z.value.sig[rand.z], power_est=power_est))
    if (length(c.value.sig)>0) ppr.all=c(ppr.all, powerfit.c(c_obs=c.value.sig[rand.c], df_obs=c.df.sig[rand.c], power_est=power_est))

  options(warn=-1)
  KSD=ks.test(ppr.all,punif)$statistic                #KS test on the resulting pprs
  options(warn=0)
  return(KSD)
}


        


##############################################
#EXAMPLE 1. WITH A FEW ENTERED VALUES
#ENTERING THEM
  #t-tests
    t.value.sig=c(2.5,  2.9)
    t.df.sig=c(40,84)
  #Ftest
    f.value.sig=4.8
    f.df1.sig=2
    f.df2.sig=120
  #Normal
    z.value.sig=3
  #Chisq
    c.value.sig=9.9
    c.df.sig=2




#BOOTSTRAPP IT
  boot.all=c()
  for (i in 1:1000) 
  {
  boot.all=c(boot.all, optimize(powerfit.all.boot,c(.051,.999))$minimum )
  }

  #2.5TH, 50TH, AND 97.5TH BOOTSTRAPPED SAMPLE
  quantile(boot.all, c(.025,.5,.975))
  
  #Standard Error
  sd(boot.all)
