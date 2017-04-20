library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")
load(file="RR.RData")

# ---------------------------------------------------------------------
# SETTINGS

# compare delta==0 (for false positive rate) against delta==0.5 (for power)
H1 <- 0.5

# select only some methods for displaying
RR.sel <-  RR %>% 
  filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve.evidence", "puniform", "3PSM")) %>% 
  mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve.evidence", "puniform", "3PSM"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "3PSM")))

RR.H0 <- RR.sel %>% filter(delta == 0) %>% select(condition, k, qrp.label, selProp, tau.label, method, TypeI, tau) #add tau
RR.H1 <- RR.sel %>% filter(delta == H1) %>% select(k, qrp.label, selProp, tau.label, method, Power, tau) #add tau

RR.wide <- inner_join(RR.H0, RR.H1)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI
RR.wide$errorSum <- (1-RR.wide$Power) + RR.wide$TypeI


save(RR.wide, file="RR.wide.RData")


#theme_metashowdown <- theme(
#  panel.spacing =unit(0.5, "lines"),
#  panel.background = element_rect(fill="white"),
#  panel.border = element_rect(color="grey70",fill=NA, size = 1), #element_blank(),
#  panel.grid.minor= element_blank(),
#  panel.grid.major= element_blank(),#element_line(color="grey90"),
#  strip.background = element_rect(colour="white", fill="white"), #"grey93"
#  axis.ticks = element_line(color="lightgrey"),
#  legend.position = "none", #c("bottom"),
#  axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)
#)

theme_metashowdown <- theme(
  title = element_text(size=18),
  axis.title = element_text(size=18),
  axis.text.y = element_text(size=18),
  axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1,size=15),
  strip.text = element_text(size=16),
  panel.spacing =unit(.5, "lines"),
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(color="grey70",fill=NA, size = 1), #element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major= element_blank(),#element_line(color="grey90"),
  strip.background = element_rect(colour="white", fill="white"), #"grey93"
  axis.ticks = element_line(color="lightgrey"),
  legend.position = "none"#c("bottom"),
)



buildFacet <- function(dat, title) {
  PLOT <- dat %>%
    ggplot(aes(x=factor(k), shape=qrp.label)) + 
    geom_hline(yintercept=c(.05, .50, .80), linetype="dotted", color="skyblue") +
    geom_hline(yintercept=c(0, 1), linetype="solid", color="skyblue") +
    geom_point(aes(y=TypeI), position=position_dodge(width=.7), size = 3, color="steelblue3", fill="skyblue") +	
    geom_point(aes(y=Power), position=position_dodge(width=.7), size = 1.5, color="black", fill="black") +	
    coord_flip() +
    facet_grid(tau~method,labeller = label_bquote(rows = tau == .(tau))) + 
    scale_y_continuous(labels=scales::percent, limits=c(0, 1), breaks = c(.05, .5, .8, 1)) + 
    scale_shape_manual(values=c(21, 22, 24)) + 
    ylab("False positives / Statistical Power") +
    xlab(expression(italic(k))) +
    ggtitle(title) + 
    theme_metashowdown
  return(PLOT)	
}

plotA <- buildFacet(RR.wide %>% filter(selProp==0), bquote("(A) 0% publication bias"))
plotB <- buildFacet(RR.wide %>% filter(selProp==0.6), bquote("(B) 60% publication bias"))
plotC <- buildFacet(RR.wide %>% filter(selProp==0.9), bquote("(C) 90% publication bias"))


#plotA <- buildFacet(RR.wide %>% filter(selProp==0), bquote("(A) False positive error rates and statistical power at"~delta~" = "~.(H1)~", 0% publication bias"))
#plotB <- buildFacet(RR.wide %>% filter(selProp==0.6), bquote("(B) False positive error rates and statistical power at"~delta~" = "~.(H1)~", 60% publication bias"))
#plotC <- buildFacet(RR.wide %>% filter(selProp==0.9), bquote("(C) False positive error rates and statistical power at"~delta~" = "~.(H1)~", 90% publication bias"))


# ---------------------------------------------------------------------
# Build the legend

legOnlyPlot <- data.frame(qrp.label=factor(c("none", "med", "high"), ordered=TRUE), rate=c("False positive rate", "Power", "Power")) %>% 
  ggplot(aes(x=1, y=1, shape=qrp.label, color=rate, fill=rate)) + 
  geom_point()+
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom"),
    legend.key = element_rect(fil='white')
  ) + 
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24), guide = guide_legend(title = "QRP Env.")) +
  scale_color_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates")) +
  scale_fill_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates"))

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legOnlyPlot) 


# ---------------------------------------------------------------------
# Save PDF of plot

pdf("Plots/hyptest.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()
