assign2 <- function(data, X, Y){
  par(mfrow=c(3,1))
  plot(X,Y)
  lines(smooth.spline(X,Y))
  lines(smooth.spline(X,Y,df=2),col=2)
  smsp.strcv<-smooth.spline(X,Y)
  smspcv.resid<-Y-approx(smsp.strcv$x,smsp.strcv$y,X)$y
  smsp.str2<-smooth.spline(X,Y,df=2)
  smsp2.resid<-Y-approx(smsp.str2$x,smsp.str2$y,X)$y
  SSF<-sum(smspcv.resid^2)
  SSN<-sum(smsp2.resid^2)
  pf(((SSN-SSF)/(smsp.strcv$df-2))/(SSF/(nrow(NOAA)-3)),(smsp.strcv$df-2),(nrow(data)-3))
  qqnorm(smspcv.resid)
  qqnorm(smsp2.resid)
}

NOAA <- read.csv("NOAA+GISS.csv")
assign2(NOAA,NOAA[[3]],sqrt(NOAA[[2]]))
