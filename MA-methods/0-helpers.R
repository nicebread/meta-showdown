tidyRMA <- function(RMA) {
	res <- data.frame(
			  term = if(length(RMA$b)==1) {"b0"} else {c("b0", "b1")},
			  estimate = as.vector(RMA$b),
			  std.error = RMA$se,
			  statistic = RMA$zval,
			  p.value = RMA$pval,
			  conf.low = RMA$ci.lb,
			  conf.high = RMA$ci.ub
			)
	rownames(res) <- NULL
	return(res)
}

tidyLM <- function(...) {
	res <- tidy(..., conf.int=TRUE)
	res$term[res$term == "(Intercept)"] <- "b0"
	res$term[res$term == "sqrt(v)"] <- "b1"
	res$term[res$term == "v"] <- "b1"
	return(res)
}
