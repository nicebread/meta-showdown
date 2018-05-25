library(shiny)
library(dplyr)
library(ggvis)
library(reshape2)
library(htmltools)
library(meta)
library(ggplot2)
library(stringr)


# simple wrapper: formats a number in f.2 format
f2 <- function(x, digits=2, prepoint=0, skipZero=FALSE) {	
	if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x2) , fixed=TRUE)})
	} else {
		gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x) , fixed=TRUE)
	}
}


# nicely formats a p-value
p.format <- function(x, digits=3) {
	if(is.na(x)) return("NA")
	if(x >= .1^digits) return(paste0("p = ", f2(x, digits, skipZero=TRUE)))
  return(paste0("p < ", f2(.1^digits, digits, skipZero=TRUE)))
}



# returns significance of a p-value as sequence of stars
p.stars <- function(p) {
  if(p <= .0001) return('****')
  if(p <= .001)  return('***')
  if(p <= .01)   return('**')
  if(p <= .05)   return('*')
  return ('ns')
}


getTable <- function(df, cbGetClass = NULL) {
	df <- as.data.frame(df)
  thead <- paste0('<th>', htmlEscape(names(df)), '</th>', collapse='')
  
  tbody <- rep("", nrow(df))
  
  for(row in 1:nrow(df)) {
    format <- '<td>%s</td>'

    tbody[row] <- paste0(sapply(df[row,], function(x){ sprintf('<td>%s</td>', htmlEscape(as.character(x))) }), collapse='');
    
    cls <- NULL
    if( is.function(cbGetClass) ) {
      cls <- cbGetClass(df[row,])
    } 
    
    if(is.character(cls)) {
      tbody[row] <- sprintf('<tr class="%s">%s</tr>', htmlEscape(cls), tbody[row])
    } else {
      tbody[row] <- sprintf('<tr>%s</tr>', tbody[row])
    }
  }
  
  tbody2 <- paste0(tbody, collapse='')
  
  HTML(
    '<div class="table-responsive"><table style="font-size:100%;" class="data table table-condensed"><thead><tr>', 
    thead, 
    '</tr></thead><tbody>',
    tbody2,
    '</tbody></table></div>'
  )
}

relabelMethod <- function(x) {
	x <- factor(x)
	levels(x)[levels(x)=="reMA"] <- "RE"
	levels(x)[levels(x)=="PET"] <- "PET"
	#levels(x)[levels(x)=="PET.rma"] <- "PET"
	levels(x)[levels(x)=="PEESE"] <- "PEESE"
	#levels(x)[levels(x)=="PEESE.rma"] <- "PEESE"
	levels(x)[levels(x)=="PETPEESE"] <- "PET-PEESE"
	#levels(x)[levels(x)=="PETPEESE.rma"] <- "PET-PEESE"
	levels(x)[levels(x)=="pcurve"] <- "p-curve"
	levels(x)[levels(x)=="pcurve.evidence"] <- "p-curve"
	levels(x)[levels(x)=="puniform"] <- "p-uniform"	
	levels(x)[levels(x)=="WAAP-WLS"] <- "WAAP-WLS"	
	return(x)
}

# selectPETPEESEmodel <- function(x, model) {
# 	if (model == "lm") {
# 		x <- x %>%
# 			filter(!method %in% c("PET.rma", "PEESE.rma", "PETPEESE.rma")) %>%
# 			mutate(method = relabelMethod(method)) %>%
# 			filter(method %in% methodOrder)
#
# 	} else if (model == "rma") {
# 		x <- x %>%
# 			filter(!method %in% c("PET.lm", "PEESE.lm", "PETPEESE.lm")) %>%
# 			mutate(method = relabelMethod(method)) %>%
# 			filter(method %in% methodOrder)
# 	}
# 	return(x)
# }

selectModels <- function(x) {
		x <- x %>% 
			mutate(method = relabelMethod(method)) %>% 
			filter(method %in% methodOrder)
	return(x)
}



load("summ.RData")
load("hyp.wide.RData")

conditions <- read.table("conditions.txt", header=TRUE)

H1.stroke <- "black"
H1.fill <- "grey20"
H0.stroke <- "steelblue2"
H0.fill <- "skyblue"

# Prepare data for hypothesis test plot

#RR$TypeI.excess <- cut(RR$TypeI, breaks=c(0, .05, .10, 1), labels=c("skyblue", "orange", "red"))
#RR$qrpEnv <- factor(RR$qrp.label, levels=c("QRP = none", "QRP = med", "QRP = high"), labels=c("none", "med", "high"))
#RR$shape <- as.character(factor(RR$qrp.label, labels=c("circle", "square", "triangle-up")))
RR.H1 <- summ %>% ungroup() %>% filter(delta > 0, !method %in% c("pcurve", "pcurve.hack", "pcurve.lack"))  %>% select(k, delta, qrp.label, qrpEnv, censor, censor.label, tau, method, Power = H0.reject.pos.rate)
RR.H0 <- summ  %>% ungroup() %>% filter(delta == 0, !method %in% c("pcurve", "pcurve.hack", "pcurve.lack")) %>% select(k, delta, qrp.label, qrpEnv, censor, censor.label, tau, method, TypeI = H0.reject.pos.rate)

# add Type I error rate to the delta > 0 conditions
RR.H0b <- RR.H0
RR.H0b$method[RR.H0b$method == "pcurve.evidence"] <- "pcurve"
summ <- inner_join(summ, RR.H0b %>% select(-delta))

# Prepare data for estimation plot

summ$stroke <- ifelse(summ$delta == 0, H0.stroke, H1.stroke)
summ$fill <- ifelse(summ$delta == 0, H0.fill, H1.fill)

summ2 <- summ %>% 
		filter(method %in% c("reMA", "TF", "WAAP-WLS", "PET", "PEESE", "PETPEESE", "3PSM", "4PSM", "pcurve", "puniform")) %>% 
		ungroup()

# store estimation quantiles in long format		
summLong <- summ2  %>% 
		select(k, delta, qrp.label, censor, censor.label, tau, qrpEnv, method, stroke, fill, meanEst.pos, perc2.5.pos, perc97.5.pos, meanEst, perc2.5, perc97.5) %>% 
		melt(id.vars=c("k", "delta", "qrp.label", "censor", "censor.label", "tau", "qrpEnv", "method", "stroke", "fill"), na.rm=FALSE)


#methodOrder <- c("RE", "TF", "WAAP-WLS", "p-curve", "p-uniform", "PET-PEESE", "3PSM")
methodOrder <- c("RE", "TF", "WAAP-WLS", "p-curve", "p-uniform", "PET", "PEESE", "PET-PEESE", "3PSM", "4PSM")
