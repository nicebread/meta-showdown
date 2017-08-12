library(tidyr)
load(file="dataFiles/res.wide.red.RData")

	x <- res.wide.red %>% filter(condition==1) %>% select(id, method, b0_estimate)

getCor <- function(x) {
	x2 <- x %>% select(id, method, b0_estimate) %>% spread(method, b0_estimate) %>% select(-id)
	x3 <- cor(x2, use="p") %>% as.data.frame()
	x3$method1 <- rownames(x3)
	x4 <- gather(x3, method1)
	colnames(x4) <- c("method1", "method2", "COR")
	return(x4)
}

# get correlation of estimators within all conditions
res <- data.frame()
for (cond in 1:432) {
	print(cond)
	x <- res.wide.red %>% filter(condition==cond)
	C <- getCor(x)
	res <- rbind(res, data.frame(
		x[1, 1:8],
		C
	))
}

# get correlation of estimators across all conditions
C2 <- getCor(res.wide.red)
C2