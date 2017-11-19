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


# returns a result data frame either in wide or long format
returnRes <- function(res, long=TRUE, reduce=TRUE) {
	if (is.null(res)) return(NULL)
		
	# convert all factor columns to characters
	res %>% mutate_if(is.factor, as.character) -> res	
		
	if (long==FALSE) {
	  # return wide format
	  return(res)
	} else {
	  # transform to long format
	  longRes <- melt(res, id.vars=c("method", "term"))
	  if (reduce==TRUE & nrow(res) > 1) {longRes <- longRes %>% filter(!is.na(value)) %>% arrange(method, term, variable)}
	  return(longRes)
	}
}
