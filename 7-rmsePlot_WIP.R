## ======================================================================
## This is WIP code for an MSE plot
## ======================================================================

# NB, logRMSE seems to make the little differences easier to see, 
#   less likely to be swamped by big difference btwn RE and PET-PEESE

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(tidyr)

load("dataFiles/summ.RData")

# ---------------------------------------------------------------------
# Plot settings

#YLIM <- c(-0.09, 1.2)
YLIM <- c(0, 1)
DELTAS <- c(0, 0.5)

theme_metashowdown <- theme(
  title = element_text(size=18),
  axis.title = element_text(size=18),
  axis.text = element_text(size=12),
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


#summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS")) %>% 
#  mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS")))

# reduced set for revision
summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PETPEESE.lm", "pcurve", "puniform", "3PSM", "WAAP-WLS")) %>% 
  mutate(method = factor(method, levels=c("reMA", "TF", "PETPEESE.lm", "pcurve", "puniform", "3PSM", "WAAP-WLS"), labels=c("RE", "TF", "PET-PEESE", "p-curve", "p-uniform", "3PSM", "WAAP-WLS")))



# prepare extra data.frame for the number of successful computation out of 1000 simulations
summ2$just <- ifelse(summ2$delta==0, 1.8, -0.8)
summ2$symbolCol <- ifelse(summ2$delta==0, "0", "1")
summ2$n.validEstimates.label <- as.character(summ2$n.validEstimates)
summ2$n.validEstimates.label[summ2$n.validEstimates.label=="1000"] <- ""

summ2$n.validEstimates.symbol <- cut(summ2$n.validEstimates, 
                                     breaks=c(-1, 250, 500, 750, 1000), 
                                     labels=c("! ", "#", "* ", ""))

# Nudge things to make room for number position
# nPos is perc2.5 when delta == 0, and when delta > 0, it's perc97.5?
#summ2$nPos <- summ2$perc2.5.pos
#summ2$nPos[summ2$delta > 0] <- summ2$perc97.5.pos[summ2$delta > 0]
summ2$nPos <- summ2$perc2.5
summ2$nPos[summ2$delta > 0] <- summ2$perc97.5[summ2$delta > 0]

#dat = summ2 %>% filter(censor==0, delta %in% DELTAS)

buildFacet <- function(dat, title) {
  PLOT <- dat %>%
    # ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
    filter(k %in% c(10, 100)) %>% 
    ggplot(aes(x=method, 
               y=log(RMSE, 2), 
               #ymin=perc2.5, ymax=perc97.5, 
               shape=qrp.label, color=factor(k), 
               fill=factor(k))) + 
    #geom_hline(yintercept=DELTAS[1], color="skyblue") + 
    #geom_hline(yintercept=DELTAS[2], color="black") + 
    geom_point(position=position_dodge(width=.5), 
      size = 2
      ) +	
    #coord_flip(ylim=YLIM) +
    # geom_text(aes(x=factor(k), 
    #               y=nPos, 
    #               label=n.validEstimates.symbol, 
    #               hjust=just, 
    #               group=qrp.label, 
    #               color=factor(delta)), 
    #           position=position_dodge(width=0.7), 
    #           size=3, 
    #           vjust=0.9) +
    facet_grid(delta.label~tau.label
               #,labeller = label_bquote(rows = tau == .(tau))
               , scales = 'free') + 
    theme_metashowdown +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(limits = c(-6.1, 0)) + 
    scale_shape_manual(values=c(21,22,24)) + 
    scale_color_manual(values=c("steelblue3", "black", "steelblue3", "black")) +
    scale_fill_manual(values=c("skyblue", "black")) +
    ylab("log2(RMSE)") +
    xlab("method") +
    ggtitle(title)
  return(PLOT)
}

# alternative style
buildFacet2 <- function(dat, title) {
  PLOT <- dat %>%
    # ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
    filter(delta %in% c(0, 0.5)) %>% 
    ggplot(aes(x=method, 
               y=log(RMSE, 2), 
               #ymin=perc2.5, ymax=perc97.5, 
               shape=qrp.label, color=factor(delta), 
               fill=factor(delta))) + 
    #geom_hline(yintercept=DELTAS[1], color="skyblue") + 
    #geom_hline(yintercept=DELTAS[2], color="black") + 
    geom_point(position=position_dodge(width=.5), 
               size = 2
    ) +	
    #coord_flip(ylim=YLIM) +
    # geom_text(aes(x=factor(k), 
    #               y=nPos, 
    #               label=n.validEstimates.symbol, 
    #               hjust=just, 
    #               group=qrp.label, 
    #               color=factor(delta)), 
    #           position=position_dodge(width=0.7), 
    #           size=3, 
    #           vjust=0.9) +
    facet_grid(k.label~tau.label
               #,labeller = label_bquote(rows = tau == .(tau))
               #, scales = 'free'
               ) + 
    theme_metashowdown +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(limits = c(-6.1, 0)) + 
    scale_shape_manual(values=c(21,22,24)) + 
    scale_color_manual(values=c("steelblue3", "black", "steelblue3", "black")) +
    scale_fill_manual(values=c("skyblue", "black")) +
    ylab("log2(RMSE)") +
    xlab("method") +
    ggtitle(title)
  return(PLOT)
}

plotA <- buildFacet(dat = summ2 %>% filter(censor=="none", delta %in% DELTAS), 
                    bquote("(A) no publication bias"))
plotB <- buildFacet(dat = summ2 %>% filter(censor=="med", delta %in% DELTAS), 
                    bquote("(B) medium publication bias"))
plotC <- buildFacet(dat = summ2 %>% filter(censor=="high", delta %in% DELTAS), 
                    bquote("(C) strong publication bias"))

plotA1 <- buildFacet2(dat = summ2 %>% filter(censor=="none", delta %in% DELTAS), 
           bquote("(A) no publication bias"))
plotB1 <- buildFacet2(dat = summ2 %>% filter(censor=="med", delta %in% DELTAS), 
           bquote("(B) medium publication bias"))
plotC1 <- buildFacet2(dat = summ2 %>% filter(censor=="high", delta %in% DELTAS), 
           bquote("(C) strong publication bias"))
# ---------------------------------------------------------------------
# Build legend

values <- c("skyblue", "black")
names(values) <- DELTAS

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legOnlyPlot = summ2 %>% filter(censor=="none", delta %in% DELTAS, k %in% c(10, 100)) %>%
  ggplot(aes(x=factor(k), y=meanEst, shape=factor(qrpEnv),color=factor(k),fill=factor(k))) + 
  geom_point() +
  coord_flip(ylim=YLIM) +
  facet_grid(tau.label~method) +
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom"),
    legend.key = element_rect(fill='white'),
    legend.title = element_text(size=14, face="bold"),
    legend.text = element_text(size=12)
  ) + 
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.", override.aes = list(size=6))) +
  scale_color_manual(values=values, guide = guide_legend(title = bquote(k), override.aes = list(size=6))) +
  scale_fill_manual(values=values, guide = guide_legend(title = bquote(k)))


legend <- g_legend(legOnlyPlot) 


# ---------------------------------------------------------------------
# Save PDF

pdf("Plots/RMSEPlot.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, 
             layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()

pdf("Plots/RMSEPlot1.pdf", width=15, height=22)
grid.arrange(plotA1, plotB1, plotC1, legend, nrow=19, 
             layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()

# WIP WORKBENCH -------------------------------------------------------
# ungroup(summ2) %>% 
#   filter(censor == "none", delta %in% DELTAS) %>% 
#   select(k, delta, qrpEnv, censor, tau) %>% 
#   unique() %>% 
#   as.data.frame()
# 
# # First draft
# filter(summ2, censor == "none", delta %in% DELTAS) %>% 
#   ggplot(aes(x = k, y = RMSE, color = method)) +
#   geom_point(position = position_dodge(width = 1)) +
#   facet_grid(delta~tau)
# 
# # make x-axis function of k and qrp?
# filter(summ2, censor == "none", delta %in% DELTAS) %>% 
#   ggplot(aes(x = interaction(k, qrpEnv), y = RMSE, color = method)) +
#   geom_point(position = position_dodge(width = 1)) +
#   facet_grid(delta~tau)
# 
# # restrict to qrp == "none"?
# filter(summ2, censor == "none", qrpEnv == "none", delta %in% DELTAS) %>% 
#   ggplot(aes(x = k.label, y = RMSE, color = method)) +
#   geom_point(position = position_dodge(width = .5)) +
#   facet_grid(delta.label~tau.label)
# 
# # log y axis?
# filter(summ2, censor == "none", qrpEnv == "none", delta %in% DELTAS) %>% 
#   ggplot(aes(x = k.label, y = log(RMSE), color = method)) +
#   geom_point(position = position_dodge(width = .5)) +
#   facet_grid(delta.label~tau.label)
# 
# # keep playing with this...
# filter(summ2, censor == "none", tau == 0, delta %in% DELTAS) %>% 
#   ggplot(aes(x = k.label, y = log(RMSE), color = method)) +
#   geom_point(position = position_dodge(width = .75)) +
#   facet_grid(delta.label~qrp.label)
# filter(summ2, censor == "none", tau == 0.2, delta %in% DELTAS) %>% 
#   ggplot(aes(x = k.label, y = log(RMSE), color = method)) +
#   geom_point(position = position_dodge(width = .75)) +
#   facet_grid(delta.label~qrp.label)
# 
# #pcurve vs puniform, does one dominate?
# filter(summ2, method %in% c("p-curve", "p-uniform")) %>% 
#   ungroup() %>% 
#   select(condition:tau.label, method, RMSE) %>% 
#   spread(key = method, value = RMSE) %>% 
#   ggplot(aes(x = `p-curve`, y = `p-uniform`)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0)
# 
# # huck em all together and see what happens
# ggplot(summ2, aes(x = RMSE)) +
#   geom_density() +
#   facet_grid(method~.)
# ggplot(summ2, aes(x = RMSE)) +
#   geom_histogram() +
#   facet_grid(method~.)
# # split by effect size
# ggplot(summ2, aes(x = RMSE)) +
#   geom_density() +
#   facet_grid(method~delta.label)
# # split by qrp / pub bias, smooth density acroos k and tau
# filter(summ2, qrpEnv == "none", censor == "none") %>% 
#   ggplot(aes(x = log(RMSE))) +
#   geom_density() +
#   geom_rug() +
#   # geom_vline() + # would require some complicated grouping
#   facet_grid(method~delta.label)
# filter(summ2, qrpEnv == "med", censor == "medium") %>% 
#   ggplot(aes(x = log(RMSE))) +
#   geom_density() +
#   geom_rug() +
#   # geom_vline() + # would require some complicated grouping
#   facet_grid(method~delta.label)
# filter(summ2, qrpEnv == "high", censor == "strong") %>% 
#   ggplot(aes(x = log(RMSE))) +
#   geom_density() +
#   geom_rug() +
#   # geom_vline() + # would require some complicated grouping
#   facet_grid(method~delta.label) +
#   ggtitle("High QRPs and strong censoring.")
