library(forecast)

Data <- read.csv(paste("C:/Users/aaron.hung/Documents/shiny_test/MyData3_dateRefine_0301_v4.csv", sep = ""))
hospital <- list()
for( x in c(1:11)) 
{
  hospital[[x]] <- Data[Data["Hospital_PK"]==x,]
  E_vol <- list()
  for( y in c(15:19))
  {     
    model1 <- auto.arima(ts(hospital[[x]][y],start=1 , frequency = 42))
    model2 <-stlf(ts(hospital[[x]][y][,1],start=1 , frequency = 42),allow.multiplicative.trend=TRUE)
    measure1 <- accuracy(model1)
    measure2 <- accuracy(model2)
    if(measure1[,'RMSE'] > measure2[,'RMSE'])
    {
      hospital[[x]][y][1] <- measure2
    }
    else
    {
      hospital[[x]][y][1] <- measure1
    }
  }
}
par(mfrow=c(2,3))        
plot(forecast(hospital[[1]][15][1],h=500))
plot(forecast(hospital[[1]][16][1],h=500)) 
plot(forecast(hospital[[1]][17][1],h=500))
plot(forecast(hospital[[1]][18][1],h=500))
plot(forecast(hospital[[1]][19][1],h=500))

H1data_A07 <- ts(H1['A07'],start=1 , frequency = 42)
H1data_A07arma <- auto.arima(H1data_A07)
H1data_A07stl <-stlf(H1data_A07[,1],allow.multiplicative.trend=TRUE)
accuracy(H1data_A07arma)



plot(forecast(H1data_A07stl,h=500))


