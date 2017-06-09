


setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")

load("dataFiles/summ.RData") 
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)



#--------------------------------------------------------------

#--------------------------------------------------------------

scoreIt = function(X,metric){
  
  pts = matrix(NA,length(X),1)
  
  if(metric=='ME'){
    boundary = c(.1,.15)
  }
  if(metric=='RMSE'){
    boundary = c(.1,.15)
  }
  if(metric=='cov'){
    boundary = c(.01,.02)
  }
  if(metric=='pow'){
    boundary = c(.5,.8)
  }

  for(i in 1:length(X)){
    if(!is.na(X[i])){
      if(X[i] < boundary[1]){pts[i]=2}
      if(X[i] >= boundary[1] & X[i] < boundary[2]){pts[i]=1}
      if(X[i] >= boundary[2]){pts[i]=0}
    }else{pts[i]=0}
  }
  
  return(pts)
}






colorTable = function(metric,sel,positive=F){
  
  Delta = c(0,.2,.5,.8)
  Tau = c(0,.2,.4)
  
  finalOut = data.frame()
  
  for(i in 1:length(Delta)){
    for(j in 1:length(Tau)){
      
      k_set=c(10, 30, 60, 100)
      delta_set=Delta[i]
      qrpEnv_set=c("none", "med", "high")
      selProp_set=sel
      tau_set=Tau[j]
      
      params <- expand.grid(k=k_set, 
                            delta=delta_set, 
                            qrpEnv=qrpEnv_set, 
                            selProp=selProp_set, 
                            tau=tau_set)
      rownames(params) <- NULL
      print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))
      
      conLab = paste(params[, "k"],
                     params[, "delta"],
                     params[, "qrpEnv"],
                     params[,"tau"],
                     params[,"selProp"])
      
      performScore = data.frame(matrix(NA,nrow(params),8))
      colnames(performScore)=c("RE","TF","PT","PE","PP","PC","PU","3P")
      pb = txtProgressBar(min = 1, max = nrow(params), style = 3)
      
      for(iCon in 1:nrow(params)){
        #load a given condition
        
        if(metric=='pow'){
          check = summ %>% filter(delta==params[iCon, "delta"],
                                  tau==params[iCon, "tau"],
                                  k==params[iCon, "k"],
                                  selProp==params[iCon, "selProp"], 
                                  qrpEnv==params[iCon, "qrpEnv"],
                                  method != 'Tau',
                                  method != 'fill',
                                  method != 'pcurve', #remove pcurve, use pcurve.evidence
                                  method != 'pcurve.hack',
                                  method != 'pcurve.lack',
                                  method != 'PET.rma',
                                  method != 'PEESE.rma',
                                  method != 'PETPEESE.rma',
                                  method != 'topN.fixed')
        }else{
          check = summ %>% filter(delta==params[iCon, "delta"],
                                  tau==params[iCon, "tau"],
                                  k==params[iCon, "k"],
                                  selProp==params[iCon, "selProp"], 
                                  qrpEnv==params[iCon, "qrpEnv"],
                                  method != 'Tau',
                                  method != 'fill',
                                  method != 'pcurve.evidence', #remove pcurve.evidence, use pcurve
                                  method != 'pcurve.hack',
                                  method != 'pcurve.lack',
                                  method != 'PET.rma',
                                  method != 'PEESE.rma',
                                  method != 'PETPEESE.rma',
                                  method != 'topN.fixed')
        }
        
        
        if(positive==T){
          condition = data.frame(check$delta,
                                 check$tau,
                                 check$k,
                                 check$method,
                                 check$ME.pos,
                                 check$RMSE.pos,
                                 check$coverage.pos,
                                 check$H0.reject.pos.rate) 
        }else{
          condition = data.frame(check$delta,
                                 check$tau,
                                 check$k,
                                 check$method,
                                 check$ME,
                                 check$RMSE,
                                 check$coverage,
                                 check$H0.reject.rate) 
        }
        
        colnames(condition)=c("delta","tau","k","method","ME","RMSE","cov","pow")
        
        #recode as power
        #condition$pow = 1 - condition$cons0
        
        #Make sure each method is represented. If not, leave as NA.
        if(metric=='pow'){
          methodNames2 = c("reMA","TF","PET.lm","PEESE.lm","PETPEESE.lm","pcurve.evidence","puniform","3PSM")        
        }else{
          methodNames2 = c("reMA","TF","PET.lm","PEESE.lm","PETPEESE.lm","pcurve","puniform","3PSM")        
        }
        
        for(iMethod in 1:length(methodNames2)){
          
          if(sum(methodNames2[iMethod]==condition[,"method"])>0){
            methodInd = which(condition$method==methodNames2[iMethod])
            performScore[iCon,iMethod] = condition[methodInd,metric]
          }        

        }                        
        setTxtProgressBar(pb, iCon)              
      }
      close(pb)
      
      out = cbind(round(performScore,digit=2),id = factor(1:length(conLab)))
      outMelt = melt(out) 
      outMelt$Delta = i
      outMelt$Tau = j
      finalOut = rbind(finalOut,outMelt)
    }
  }
  
  #recode based on metric
  if(metric=='ME'){
    X = abs(finalOut$value)
  }
  if(metric=='RMSE'){
    X = abs(finalOut$value)
  }
  if(metric=='cov'){
    X = abs(finalOut$value-.95)
  }
  if(metric=='pow'){
    X = finalOut$value
  }
  
  finalOut$brks = factor(scoreIt(X,metric))
  
  #get the colors for the table
  colorCnt = 3 
  getPalette = colorRampPalette(brewer.pal(3, 'Blues'))
  brkColors = getPalette(colorCnt)
  
  #Break it up by each to-be-ploted data set
  
  plotList = list()
  plotCount = matrix(1:(length(Delta)*length(Tau)),length(Tau),length(Delta))
  
  for(i in 1:length(Delta)){
    for(j in 1:length(Tau)){
      
      toPlot = subset(finalOut,
                      finalOut$Delta==i & finalOut$Tau==j)
      
      plot = ggplot(toPlot, aes(variable, id)) + 
        geom_tile(aes(fill = brks), colour = "white") + 
        scale_fill_manual(values = c("0" = brkColors[1], "1" = brkColors[2], "2" = brkColors[3]  )) +       
        geom_text(aes(variable, label = value), 
                  color = "black", size = 3) +
        #ggtitle("test") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              legend.position = 'none',
              plot.title = element_text(lineheight=.8),
              axis.text = element_text(colour="black"))
      
      iP = plotCount[j,i]
      
      plotList[[iP]] = plot
      
    }
  }
  
  #make the legend
  params <- expand.grid(k=c(10,30,60,100), 
                        qrpEnv=c(0,1,2))
  rownames(params) <- NULL
  
  conLab = paste(params[, "k"],
                 params[, "qrpEnv"])
  leg = cbind(params,id = factor(1:length(conLab)))
  meltLeg = melt(leg)
  
  legendPlot = ggplot(meltLeg, aes(variable, id)) + 
    geom_text(aes(variable, label = value), 
              color = "black", size = 4) +
    #ggtitle("legend") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          plot.title = element_text(lineheight=.8),
          axis.text = element_text(colour="black")) 
  
  #Create and save PDF
  
  delta1 = expression(paste(delta,' = 0.0'))
  delta2 = expression(paste(delta,' = 0.2'))
  delta3 = expression(paste(delta,' = 0.5'))
  delta4 = expression(paste(delta,' = 0.8'))
  tau1 = expression(paste(tau,' = 0.0'))
  tau2 = expression(paste(tau,' = 0.2'))
  tau3 = expression(paste(tau,' = 0.4'))
  
  adjX = -.05
  adjY = -.0125
  W = .28
  
  #mark if posified
  if(positive==T){
    posif = "_Posified"
  }else{
    posif = "_NotPosified"
  }
  
  plotFileName=paste0(metric,'_sel',sel,posif,'.pdf')
  pdf(file=plotFileName,12,10)
  
  print(plotList[[1]],vp = viewport(width = W, height = 0.25, x = .38, y = .0, just = c("right","bottom")))
  print(plotList[[4]],vp = viewport(width = W, height = 0.25, x = .38, y = .25 + adjY/2, just = c("right","bottom")))
  print(plotList[[7]],vp = viewport(width = W, height = 0.25, x = .38, y = .50 + adjY, just = c("right","bottom")))
  print(plotList[[10]],vp = viewport(width = W, height = 0.25, x = .38, y = .75 + adjY*1.5, just = c("right","bottom")))
  
  print(plotList[[2]],vp = viewport(width = W, height = 0.25, x = .68 + adjX/2, y = .0, just = c("right","bottom")))
  print(plotList[[5]],vp = viewport(width = W, height = 0.25, x = .68 + adjX/2, y = .25 + adjY/2, just = c("right","bottom")))
  print(plotList[[8]],vp = viewport(width = W, height = 0.25, x = .68 + adjX/2, y = .50 + adjY, just = c("right","bottom")))
  print(plotList[[11]],vp = viewport(width = W, height = 0.25, x = .68 + adjX/2, y = .75 + adjY*1.5, just = c("right","bottom")))
  
  print(plotList[[3]],vp = viewport(width = W, height = 0.25, x = .98 + adjX, y = .0, just = c("right","bottom")))
  print(plotList[[6]],vp = viewport(width = W, height = 0.25, x = .98 + adjX, y = .25 + adjY/2, just = c("right","bottom")))
  print(plotList[[9]],vp = viewport(width = W, height = 0.25, x = .98 + adjX, y = .50 + adjY, just = c("right","bottom")))
  print(plotList[[12]],vp = viewport(width = W, height = 0.25, x = .98 + adjX, y = .75 + adjY*1.5, just = c("right","bottom")))
  
  
  print(legendPlot,vp = viewport(width = .1, height = 0.25, x = .1, y = 0, just = c("right","bottom")))
  print(legendPlot,vp = viewport(width = .1, height = 0.25, x = .1, y = .25 + adjY/2, just = c("right","bottom")))
  print(legendPlot,vp = viewport(width = .1, height = 0.25, x = .1, y = .5 + adjY, just = c("right","bottom")))
  print(legendPlot,vp = viewport(width = .1, height = 0.25, x = .1, y = .75 + adjY*1.5, just = c("right","bottom")))
  
  grid.text(delta1, x=.95, y=.25-.125,gp=gpar(fontsize=15),just=c("center","bottom"))
  grid.text(delta2, x=.95, y=.5-.125 + adjY/2,gp=gpar(fontsize=15),just=c("center","bottom"))
  grid.text(delta3, x=.95, y=.75-.125 + adjY,gp=gpar(fontsize=15),just=c("center","bottom"))
  grid.text(delta4, x=.95, y=.99-.125 + adjY*1.5,gp=gpar(fontsize=15),just=c("center","bottom"))
  
  grid.text(tau1, x=.38-.3/2, y=.99,gp=gpar(fontsize=15),just=c("center","center"))
  grid.text(tau2, x=.68 -.3/2 + adjX/2, y=.99,gp=gpar(fontsize=15),just=c("center","center"))
  grid.text(tau3, x=.98 -.3/2 + adjX, y=.99,gp=gpar(fontsize=15),just=c("center","center"))
  
  dev.off()
  
}

setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown/ColorTablePlots")

#not posified
colorTable('ME',0)
colorTable('RMSE',0)
colorTable('cov',0)
colorTable('pow',0) #p-curve and p-uniform will be NA. See note in 3-resultsFramework 

colorTable('ME',.6)
colorTable('RMSE',.6)
colorTable('cov',.6) 
colorTable('pow',.6) #p-curve and p-uniform will be NA. See note in 3-resultsFramework  


colorTable('ME',.9)
colorTable('RMSE',.9)
colorTable('cov',.9)
colorTable('pow',.9)  #p-curve and p-uniform will be NA. See note in 3-resultsFramework


#posified
colorTable('ME',0,positive=T)
colorTable('RMSE',0,positive=T)
colorTable('cov',0,positive=T)
colorTable('pow',0,positive=T)


colorTable('ME',.6,positive=T)
colorTable('RMSE',.6,positive=T)
colorTable('cov',.6,positive=T)
colorTable('pow',.6,positive=T)

colorTable('ME',.9,positive=T)
colorTable('RMSE',.9,positive=T)
colorTable('cov',.9,positive=T)
colorTable('pow',.9,positive=T)

