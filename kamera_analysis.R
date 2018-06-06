
H1 <- read.csv("C:/Users/aaron.hung/Documents/shiny_test/MyData3_dateRefine_0301_v4_h1.csv")
library(forecast)
H1data_A07 <- msts(H1['A07'],start=c(1,1) ,seasonal.periods=c(1))
H1data_A08 <- msts(H1['A10'],start=c(1,1) ,seasonal.periods=c(1))

H1data_A07 <- ts(H1['A07'],start=c(1,1) ,frequency = 6)

acf(ts.union(H1data_A07, H1data_A08))
a<- ccf(H1data_A07[1:2190,],H1data_A08[1:2190,],lag.max=20, ylab = "cross-correlation")

Find_Max_CCF(H1data_A07[1:2190,],H1data_A08[1:2190,])
Find_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res_max)
} 





H1data
plot(H1data_A07)
a<-acf(H1data_A07[1:300])
pacf(H1data_A07[1:300])

H1data_A07diff1 <- diff(H1data_A07,differences=1)
plot(H1data_A07diff1)
a<-acf(H1data_A07diff1[1:300])
pacf(H1data_A07diff1[1:300])


fit <- tbats(H1data_A07[1:300], use.parallel=FALSE)
plot(forecast(fit,165))


H1data_A07arma <- arima(H1data_A07, order = c(5,1,4))
H1data_A07arma <- auto.arima(H1data_A07)

plot(decompose(H1data_A07))

plot(forecast(H1data_A07arma,h=100))

acf(H1data_A07arma$residuals, lag.max = 20)
Box.test(H1data_A07arma$residuals, lag = 1, type = "Ljung")
plot.ts(H1data_A07arma$residuals)
plotForecastErrors(H1data_A07arma$residuals)
H1data_A07armaforecast <- forecast(H1data_A07arma,h=690)

plot(H1data_A07armaforecast)
lines(H1data_A07[1:2190])

accuracy(H1data_A07arma)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
