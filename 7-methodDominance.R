library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(reshape2)

load(file="dataFiles/summ.RData")
head(summ)

s2  <- summ %>% 
	ungroup() %>% 
	select(condition, method, ME.pos, RMSE.pos) %>% 
	filter(!method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "PET.rma", "PEESE.rma", "PETPEESE.rma"))

# get experimental factors
conditions <- summ %>% filter(method=="reMA") %>% select(1:11)


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


## give 1 point for "draw"
# scores <- res2 %>% group_by(condition) %>% summarise(
# 	RE = sum(winner=="reMA")*2 + sum(draw1=="reMA") + sum(draw2=="reMA"),
# 	TF = sum(winner=="TF")*2 + sum(draw1=="TF") + sum(draw2=="TF"),
# 	PET = sum(winner=="PET.lm")*2 + sum(draw1=="PET.lm") + sum(draw2=="PET.lm"),
# 	PEESE = sum(winner=="PEESE.lm")*2 + sum(draw1=="PEESE.lm") + sum(draw2=="PEESE.lm"),
# 	PETPEESE = sum(winner=="PETPEESE.lm")*2 + sum(draw1=="PETPEESE.lm") + sum(draw2=="PETPEESE.lm"),
# 	pcurve = sum(winner=="pcurve")*2 + sum(draw1=="pcurve") + sum(draw2=="pcurve"),
# 	puniform = sum(winner=="puniform")*2 + sum(draw1=="puniform") + sum(draw2=="puniform"),
# 	TPSM = sum(winner=="3PSM")*2 + sum(draw1=="3PSM") + sum(draw2=="3PSM")
# )

scores <- res2 %>% group_by(condition) %>% summarise(
	RE = sum(winner=="reMA")*2 ,
	TF = sum(winner=="TF")*2 ,
	PET = sum(winner=="PET.lm")*2,
	PEESE = sum(winner=="PEESE.lm")*2,
	PETPEESE = sum(winner=="PETPEESE.lm")*2,
	pcurve = sum(winner=="pcurve")*2 ,
	puniform = sum(winner=="puniform")*2 ,
	TPSM = sum(winner=="3PSM")*2
)

scores.long <- gather(scores, key = method, value = score, RE:TPSM)

scores$winner <- ""
for (i in 1:nrow(scores)) {
	scores$winner[i] <- paste0(colnames(scores)[which(scores[i, 2:9] == max(scores[i, 2:9])) + 1], collapse = ", ")
}

scores <- merge(scores, conditions, by="condition")
scores.long <- merge(scores.long, conditions, by="condition")

scores$winner <- factor(scores$winner, levels=names(sort(table(scores$winner), decreasing=TRUE)))

sort(table(scores$winner), decreasing=TRUE)

ggplot(scores, aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + 
  geom_point(size=6) + 
  facet_grid(censor~k+tau)

# That looks ugly ...
scores.long$loop <- paste0(scores.long$delta, ":", scores.long$qrpEnv, ":", scores.long$tau)
ggplot(scores.long, aes(y=score, x=loop, color=factor(method), group=factor(method))) + 
  geom_line() + 
  facet_grid(k~censor)

# ---------------------------------------------------------------------
# strong (absolute) dominance

s2  <- summ %>% 
	ungroup() %>% 
	select(condition, k, delta, qrpEnv, censor, tau, method, ME.pos, RMSE.pos) %>% 
	filter(!method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "PET.rma", "PEESE.rma", "PETPEESE.rma"))
	
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

s3 %>% 
  filter(winner != "") %>% 
  ggplot(aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + 
  geom_point(size=6) + 
  facet_grid(censor~k+tau)


ggplot(s3 %>% filter(winner != ""), aes(y=qrpEnv, x=factor(delta), shape=factor(winner))) + geom_point(size=6) + facet_grid(selProp~k+tau)