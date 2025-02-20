## ======================================================================
## This analysis tries to find a single best method by comparing all methods pairwise:
## How often does a method dominate another method?
## --> we dismissed this analysis in favor of a sensitivity analysis
## ======================================================================

library(ggplot2)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)

load(file="dataFiles/summ.RData")
head(summ)

s2  <- summ %>% 
	ungroup() %>% 
	select(condition, method, ME.pos, RMSE.pos) %>% 
	filter(!method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack"))

# get experimental factors
conditions <- summ %>% filter(method=="reMA") %>% select(1:11) %>% arrange(condition)


# ---------------------------------------------------------------------
# pairwise dominance
	
# reshape results into a data set with binary comparisons
# This probably could be made more elegant, but I couldn't figure out how.

n.methods <- length(unique(s2$method))
methods <- unique(s2$method)

res <- data.frame()
for (C in unique(s2$condition)) {
	print(C)
	for (i in 1:n.methods) {
		for (j in 1:n.methods) {
			if (i > j)
			res <- rbind(res, data.frame(
				condition = C,
				method1 = methods[i],
				method2 = methods[j],
				ME.pos1 = as.numeric(s2[s2$condition == C & s2$method==methods[i], "ME.pos"]),
				ME.pos2 = as.numeric(s2[s2$condition == C & s2$method==methods[j], "ME.pos"]),
				RMSE.pos1 = as.numeric(s2[s2$condition == C & s2$method==methods[i], "RMSE.pos"]),
				RMSE.pos2 = as.numeric(s2[s2$condition == C & s2$method==methods[j], "RMSE.pos"])
			))
		}
	}
}

save(res, file="dataFiles/dominanceScore.RData")
#load(file="dataFiles/dominanceScore.RData")

res2 <- res %>% na.omit()

# define points, winners and draws

res2$winner <- ""
res2$draw1 <- ""
res2$draw2 <- ""

winner1 <- (abs(res2$ME.pos1) < abs(res2$ME.pos2)) & (res2$RMSE.pos1 < res2$RMSE.pos2)
res2$winner[winner1] <- as.character(res2$method1[winner1])

winner2 <- (abs(res2$ME.pos1) > abs(res2$ME.pos2)) & (res2$RMSE.pos1 > res2$RMSE.pos2)
res2$winner[winner2] <- as.character(res2$method2[winner2])

draw <- ((abs(res2$ME.pos1) > abs(res2$ME.pos2)) & (res2$RMSE.pos1 < res2$RMSE.pos2)) | ((abs(res2$ME.pos1) < abs(res2$ME.pos2)) & (res2$RMSE.pos1 > res2$RMSE.pos2))
res2$draw1[draw] <- as.character(res2$method1[draw])
res2$draw2[draw] <- as.character(res2$method2[draw])


scores <- res2 %>% group_by(condition) %>% summarise(
	RE = sum(winner=="reMA")*2 ,
	TF = sum(winner=="TF")*2 ,
	PET = sum(winner=="PET")*2,
	PEESE = sum(winner=="PEESE")*2,
	PETPEESE = sum(winner=="PETPEESE")*2,
	pcurve = sum(winner=="pcurve")*2 ,
	puniform = sum(winner=="puniform")*2 ,
	TPSM = sum(winner=="3PSM")*2
)

scores.long <- melt(scores, id.vars="condition")

scores$winner <- ""
for (i in 1:nrow(scores)) {
	scores$winner[i] <- paste0(colnames(scores)[which(scores[i, 2:9] == max(scores[i, 2:9])) + 1], collapse = ", ")
}

scores <- merge(scores, conditions, by="condition")
scores.long <- merge(scores.long, conditions, by="condition")

scores$winner <- factor(scores$winner, levels=names(sort(table(scores$winner), decreasing=TRUE)))

sort(table(scores$winner), decreasing=TRUE)

ggplot(scores, aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + geom_point(size=6) + facet_grid(censor.label~k+tau)

# That looks ugly ...
scores.long$loop <- paste0(scores.long$delta, ":", scores.long$qrpEnv, ":", scores.long$tau)
ggplot(scores.long, aes(y=value, x=loop, color=factor(variable), group=factor(variable))) + geom_line() + facet_grid(k~censor.label)

# ---------------------------------------------------------------------
# strong (absolute) dominance

s2  <- summ %>% 
	ungroup() %>% 
	select(condition, k, delta, qrpEnv, censor, tau, method, ME.pos, RMSE.pos) %>% 
	filter(!method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack"))
	
# which method is best in ME?
ME.matrix <- dcast(s2, condition ~ method, value.var="ME.pos")
ME.matrix$ME.winner <- colnames(ME.matrix)[apply(ME.matrix, 1, which.min)]

# which method is best in RMSE?
RMSE.matrix <- dcast(s2, condition ~ method, value.var="RMSE.pos")
RMSE.matrix$RMSE.winner <- colnames(RMSE.matrix)[apply(RMSE.matrix, 1, which.min)]

fullDominance <- cbind(ME.matrix[, c("condition", "ME.winner")], RMSE.winner = RMSE.matrix$RMSE.winner)
fullDominance$winner <- ""
fullDominance$winner[fullDominance$ME.winner == fullDominance$RMSE.winner] <- fullDominance$ME.winner[fullDominance$ME.winner == fullDominance$RMSE.winner]

sort(table(fullDominance$winner[fullDominance$winner != ""]), decreasing = TRUE)

s3 <- merge(fullDominance, conditions, by="condition")


ggplot(s3 %>% filter(winner != ""), aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + geom_point(size=6) + facet_grid(censor.label~k+tau)
ggplot(s3 %>% filter(winner != ""), aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + geom_point(size=6) + facet_grid(censor.label~k+tau)