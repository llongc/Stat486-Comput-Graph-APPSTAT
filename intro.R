myvec <- rnorm(10)
myvec1 <- myvec + .5 * rnorm(10)
boxplot(myvec1)
boxplot(myvec, myvec1)
plot(myvec, myvec1)
mean(myvec)
mean(myvec, trim=.1)
rank(myvec)
o1 <- order(myvec)
myvec[o1]
sort(myvec)
myvec[-c(1:5)]
sample(10,5)
v1 <- sample(10,5)
myvec[v1]
myvec[-v1]
NOAA <- read.csv("NOAA+GISS.csv")
NOAA
plot(NOAA[[3]],NOAA[[2]],xlab="Temperature",ylab="Number of disasters",main="Billion dollar disasters per year")
par(mfrow=c(2,1))
plot(NOAA[[3]],NOAA[[2]])
abline(lsfit(NOAA[[3]],NOAA[[2]]))
plot(NOAA[[3]],lsfit(NOAA[[3]],NOAA[[2]])$resid)
lines(lowess(NOAA[[3]],lsfit(NOAA[[3]],NOAA[[2]])$resid))
lines(lowess(NOAA[[3]],NOAA[[2]]))
NOAA.mat<-as.matrix(NOAA)
NOAA1<-NOAA.mat
NOAA1[,2] <- sqrt(NOAA.mat[,2])
plot(NOAA1[,3],NOAA1[,3]^2)
xmat <- cbind(NOAA1[,3],NOAA1[,3]^2)
ls.print(lsfit(xmat,NOAA1[,2]))
