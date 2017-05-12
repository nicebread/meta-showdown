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

