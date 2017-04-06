
#2) Calculate the gradient.

SEFE.gradient <- function(SEFEadjustedest, y, v, p) {
   a2 <- SEFEadjustedest[1]
   b2 <- SEFEadjustedest[2]
   vc2 <- 0
   mu2 <- SEFEadjustedest[3]

   A <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      A[i] <- integrate(fxnA,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   da <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      da[i] <- integrate(fxnda,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   db <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      db[i]<- integrate(fxndb,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dvc <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dvc[i] <- integrate(fxndvc,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dmu <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dmu[i] <- integrate(fxndmu,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dlda <- sum( log(p) - da/A )
   dldb <- sum( log(1-p) - db/A )
#   dldvc <- sum( ((y-mu2)^2)/(2*(v+vc2)^2) - dvc/A )
   dldmu <- sum( (y-mu2)/(v+vc2) - dmu/A )

   return( matrix(c(dlda,dldb,dldmu),1,3) ) 
}


#3) Calculate the hessian.

SEFE.hessian <- function(SEFEadjustedest, y, v, p) {
   a2 <- SEFEadjustedest[1]
   b2 <- SEFEadjustedest[2]
   vc2 <- 0
   mu2 <- SEFEadjustedest[3]

   A <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      A[i] <- integrate(fxnA,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   da <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      da[i] <- integrate(fxnda,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   db <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      db[i]<- integrate(fxndb,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dvc <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dvc[i] <- integrate(fxndvc,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dmu <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dmu[i] <- integrate(fxndmu,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   da2 <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      da2[i] <- integrate(fxnda2,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   db2 <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      db2[i]<- integrate(fxndb2,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dvc2 <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dvc2[i] <- integrate(fxndvc2,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dmu2 <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dmu2[i] <- integrate(fxndmu2,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dadb <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dadb[i] <- integrate(fxndadb,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dadvc <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dadvc[i] <- integrate(fxndadvc,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dadmu <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dadmu[i] <- integrate(fxndadmu,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dbdvc <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dbdvc[i]<- integrate(fxndbdvc,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   dbdmu <- rep(0,length(y))
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      dbdmu[i]<- integrate(fxndbdmu,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

#   dvcdmu <- rep(0,length(y))
#   for(i in 1:length(y)) {
#      vv = v[i]
#      yy = y[i]
#      dvcdmu[i] <- integrate(fxndvcdmu,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,mu2=mu2)$value }

   d2lda2 <- sum ( da^2/A^2 - da2/A )
   d2ldadb <- sum ( da*db/A^2 - dadb/A )
#   d2ldadvc <- sum ( da*dvc/A^2 - dadvc/A )
   d2ldadmu <- sum ( da*dmu/A^2 - dadmu/A )

   d2ldb2 <- sum ( db^2/A^2 - db2/A )
#   d2ldbdvc <- sum ( db*dvc/A^2 - dbdvc/A )
   d2ldbdmu <- sum ( db*dmu/A^2 - dbdmu/A )

#   d2ldvc2 <- sum ( (-1*((y-mu2)^2)/((v+vc2)^3)) + dvc^2/A^2 - dvc2/A )
#   d2ldvcdmu <- sum ( (-1*(y-mu2)/((v+vc2)^2)) + dvc*dmu/A^2 - dvcdmu/A )

   d2ldmu2 <- sum ( -1/(v+vc2) + dmu^2/A^2 - dmu2/A )

   return( matrix(c(d2lda2,d2ldadb,d2ldadmu,
                    d2ldadb,d2ldb2,d2ldbdmu,
                    d2ldadmu,d2ldbdmu,d2ldmu2),3,3) ) 
}

