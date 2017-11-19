library(ggplot2)
library(grid)
library(pwr)
library(metafor)

#Needs dataMA() and perGrp
perGrp = read.csv('../Empirical n and ES distributions/perGrp.csv')

set.seed(999)

nK = 60

#fig1A
dat1=dataMA(k=nK,delta=0,tau=0,empN=T,maxN=500,minN=0,meanN=0,selProp=0, qrpEnv='none')
dat2=dataMA(k=nK,delta=.5,tau=0,empN=T,maxN=500,minN=0,meanN=0,selProp=0, qrpEnv='none')
dat3=dataMA(k=nK,delta=0,tau=.2,empN=T,maxN=500,minN=0,meanN=0,selProp=0, qrpEnv='none')
dat4=dataMA(k=nK,delta=.5,tau=.2,empN=T,maxN=500,minN=0,meanN=0,selProp=0, qrpEnv='none')

#fig1B
dat5=dataMA(k=nK,delta=0,tau=0,empN=T,maxN=500,minN=0,meanN=0,selProp=1, qrpEnv='none')
dat6=dataMA(k=nK,delta=.5,tau=0,empN=T,maxN=500,minN=0,meanN=0,selProp=1, qrpEnv='none')
dat7=dataMA(k=nK,delta=0,tau=.2,empN=T,maxN=500,minN=0,meanN=0,selProp=1, qrpEnv='none')
dat8=dataMA(k=nK,delta=.5,tau=.2,empN=T,maxN=500,minN=0,meanN=0,selProp=1, qrpEnv='none')


#---------------------------------------------------------

funnelIt = function(d,se,
                    yMax=.5,xAxis=c(-1.2,2),
                    trueD=NA,cenK=NA,
                    linSize=.2,titleFnt=4,errLabSz=3.5,axisFnt=11,ptSize=1.5,title='',
                    yLab=F,xLab=F){
  
  dat = data.frame(d,se)
  RE = rma(d,se^2, method='REML')
  reEst = as.numeric(RE$b)
  adj = 1.1
  maxSE = max(se)
  adjMax =  yMax #adj*max(se)
  
  nullContour=data.frame(y=c(0-1.96*adjMax, 0, 0+1.96*adjMax), 
                         x=c(adjMax, 0, adjMax))
  
  lineDat = data.frame(reEst=reEst,
                       adjMax=adjMax,
                       maxSE=maxSE)
  
  if(yLab==T){yLabel=expression(italic(se))}else{yLabel=''}
  if(xLab==T){xLabel=expression(italic(d))}else{xLabel=''}
  
  points = data.frame(x = c(adjMax,adjMax), y=c(reEst,trueD)) 
  
  plot = ggplot(data = dat, aes(x = se, y = d)) +
    
    #the null contour
    geom_polygon(data=nullContour,aes(x=x,y=y),fill='skyblue',alpha=.4) +
    
    #the vertical lines
    geom_segment(data=lineDat,aes(x = 0,y = reEst,xend = adjMax,yend = reEst),
                 size=linSize, linetype=1) +
    geom_segment(data=lineDat,aes(x=0,y=0,xend=adjMax,yend = 0),
                 size=linSize, linetype=5) +
    
    #the angled lines
    geom_segment(data=lineDat,aes(x=0,y=reEst,xend=adjMax,yend=reEst-1.96*adjMax), #neg
                 size=linSize, linetype=1) +
    geom_segment(data=lineDat,aes(x=0,y=reEst,xend=adjMax,yend=reEst+1.96*adjMax), #pos
                 size=linSize, linetype=1) +
    geom_segment(data=lineDat,aes(x=0,y=0,xend=adjMax,yend=0-1.96*adjMax), #neg
                 size=linSize, linetype=5) +
    geom_segment(data=lineDat,aes(x=0,y=0,xend=adjMax,yend=0+1.96*adjMax), #pos
                 size=linSize, linetype=5) +
    
    #the data
    geom_point(shape=21,size=ptSize,colour = 'black',fill='white') +
    
    #for bias
    geom_point(data=points,aes(x=x[2],y=y[2]),shape=21,colour='black',fill='red') +
    geom_point(data=points,aes(x=x[1],y=y[1]),shape=4,colour='black') +
    
    #plot formatting
    scale_y_continuous(limits = xAxis) +
    scale_x_reverse( lim=c(yMax,0)) +
    theme_classic() +
    theme(title = element_text(size = titleFnt),
          axis.text.x = element_text(size = axisFnt),
          axis.text.y = element_text(size = axisFnt),
          axis.title.x = element_text(size = axisFnt),
          axis.title.y = element_text(size = axisFnt),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.title = element_text(vjust=0)) +
    xlab(yLabel) +
    ylab(xLabel) +
    ggtitle(title) +
    coord_flip() 
  
  return(plot)
  
}

#----------------------------------------------------------

title1 = expression(paste(delta,' = 0; ',tau,' = 0'))
title2 = expression(paste(delta,' = 0.5; ',tau,' = 0'))
title3 = expression(paste(delta,' = 0; ',tau,' = 0.2'))
title4 = expression(paste(delta,' = 0.5; ',tau,' = 0.2'))

#panel A
fun1=funnelIt(dat1[,'d'],dat1[,'se'],trueD=0,title=title1,titleFnt=10,yLab=T,xLab=T)
fun2=funnelIt(dat2[,'d'],dat2[,'se'], trueD=.5,title=title2,titleFnt=10,xLab=T)
fun3=funnelIt(dat3[,'d'],dat3[,'se'], trueD=0,title=title3,titleFnt=10,xLab=T)
fun4=funnelIt(dat4[,'d'],dat4[,'se'], trueD=.5,title=title4,titleFnt=10,xLab=T)

#panel B
fun5=funnelIt(dat5[,'d'],dat5[,'se'],trueD=0,title=title1,titleFnt=10,yLab=T,xLab=T)
fun6=funnelIt(dat6[,'d'],dat6[,'se'],trueD=.5,title=title2,titleFnt=10,xLab=T)
fun7=funnelIt(dat7[,'d'],dat7[,'se'],trueD=0,title=title3,titleFnt=10,xLab=T)
fun8=funnelIt(dat8[,'d'],dat8[,'se'],trueD=.5,title=title4,titleFnt=10,xLab=T)

#------------------------------------------------------------

round(as.numeric(rma(dat1[,'d'],dat1[,'se']^2, method='REML')$b) - 0,3)

titleA = 'A: Funnel plots of data sets unaffected by publication bias'
titleB = 'B: Funnel plots of data sets where the rate of publication bias is 100%'

pdf(file='Fig1-FunnelPlots.pdf',11,6)

print(fun1,vp=viewport(width=.25,height=.46,x=.25,y=.49, just = c("right","bottom")))
print(fun2,vp=viewport(width=.25,height=.46,x=.5,y=.49, just = c("right","bottom")))
print(fun3,vp=viewport(width=.25,height=.46,x=.75,y=.49, just = c("right","bottom")))
print(fun4,vp=viewport(width=.25,height=.46,x=1,y=.49, just = c("right","bottom")))

print(fun5,vp=viewport(width=.25,height=.46,x=.25,y=0, just = c("right","bottom")))
print(fun6,vp=viewport(width=.25,height=.46,x=.5,y=0, just = c("right","bottom")))
print(fun7,vp=viewport(width=.25,height=.46,x=.75,y=0, just = c("right","bottom")))
print(fun8,vp=viewport(width=.25,height=.46,x=1,y=0, just = c("right","bottom")))

grid.text(titleA, x=.01, y=.97,gp=gpar(fontsize=15),just=c("left","bottom"))
grid.text(titleB, x=.01, y=.47,gp=gpar(fontsize=15),just=c("left","bottom"))

dev.off()

