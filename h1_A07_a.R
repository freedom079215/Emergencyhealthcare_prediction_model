s1 <- "C:/Users/Aaron/Documents/MyData3_dateRefine_0301_v4_h1."
s2 <- "csv" 
H1 <- read.csv(paste("C:/Users/aaron.hung/Documents/shiny_test/MyData3_dateRefine_0301_v4_h1.csv", sep = ""))
library(forecast)
H1data_A07 <- ts(H1['A07'],start=1 , frequency = 42)
H1data_A07 <- msts(H1['A07'],start=c(2013,1) ,seasonal.periods=c(6,84))

H1data
plot(H1data_A07)
a<-acf(H1data_A07,lag.max=500)
pacf(H1data_A07,lag.max=10000)

H1data_A07arma <- arima(H1data_A07, order = c(7,1,5))
H1data_A07arma <- auto.arima(H1data_A07)
H1data_A07ets <- ets(H1data_A07)
H1data_A07stl <-stlf(H1data_A07[,1],allow.multiplicative.trend=TRUE)


plot(forecast(H1data_A07arma,h=500))

acf(H1data_A07arma$residuals, lag.max = 20)
Box.test(H1data_A07arma$residuals, lag=20, type="Ljung-Box")
plot.ts(H1data_A07arma$residuals)
plotForecastErrors(H1data_A07arma$residuals)
accuracy(H1data_A07arma)



acf(H1data_A07stl$residuals, lag.max = 20)
Box.test(H1data_A07stl$residuals, lag=20, type="Ljung-Box")
plot.ts(H1data_A07stl$residuals)
plotForecastErrors(H1data_A07stl$residuals)
plot(forecast(H1data_A07stl,h=500))

accuracy(H1data_A07stl)

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
