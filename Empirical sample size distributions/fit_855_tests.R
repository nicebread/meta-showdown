dat855 <- read.table("855_t_tests_twosample.tab", skip=2, header=TRUE)

# sample size distribution: N1 and N2 are nearly identical
plot(dat855$N1, dat855$N2)

N <- c(dat855$N1, dat855$N2)

myhist <- hist(N, breaks=40)
multiplier <- myhist$counts / myhist$density
mydensity <- density(N)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist, ylim=c(0, 80), main="Sample sizes from 855 t-tests")
lines(mydensity, col="blue")

