## Clean any local/environment variables
rm(list = ls())

## Get this script present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

print(LOC_CODE)
## Set it to working direcotry
setwd(LOC_CODE)

library(xts)
library(lmtest)
library(tidyverse)
library(forecast)
options(scipen = 999)

source("../code/functions/testdf.R")

TS <- read.csv("../data/TSA_2024_project_data_1.csv")
TS$date <- as.Date(TS$date, format = "%Y-%m-%d")


TS <- xts(TS[, -1], order.by = TS$date)
DF <- TS
TS$dy1 <- diff.xts(TS$y1)
TS$dy2 <- diff.xts(TS$y2)
TS$dy3 <- diff.xts(TS$y3)
TS$dy4 <- diff.xts(TS$y4)
TS$dy5 <- diff.xts(TS$y5)
TS$dy6 <- diff.xts(TS$y6)
TS$dy7 <- diff.xts(TS$y7)
TS$dy8 <- diff.xts(TS$y8)
TS$dy9 <- diff.xts(TS$y9)
TS$dy10 <- diff.xts(TS$y10)
#index(TS)

TS_SHORT <- TS[index(TS) < as.Date("2022-03-08"),]

TS_TEST <- TS[index(TS) > as.Date("2022-03-07"),]

plot(DF, main = "Stock prices")



Y <- c('y1', 'y2', 'y3', 'y4', 'y5', 'y6', 'y7', 'y8', 'y9', 'y10')
df_res <- data.frame(pair=NULL, p_adf=NULL,p_bg=NULL, result=NULL, cointegration_vector=NULL)
cointvec <- "NULL"
for (i in 1:9) {
  for (j in (i + 1):10) {
    print(paste("(y=",i,",y=",j,")"))
    model.coint <- lm(formula = paste(Y[i], "~", Y[j]), data = TS)
    redf <- testdf(variable = residuals(model.coint), max.augmentations = 3)
    print(redf)
    if (redf$p_adf[1] < .05) {
      result <- "Conintegrated"
      cointvec = paste("[1,",unname(coef(model.coint))[1], ",",unname(coef(model.coint))[2],"]")
    } else {
      result <- "Not Conintegrated"
      cointvec <- ""
    }
    new_row <- data.frame(pair=paste("(y=",i,",y=",j,")"), p_adf=redf$p_adf[1], p_bg=redf$p_bg[1], result=result, cointegration_vector=cointvec)
    df_res <- rbind(df_res, new_row)
    }
}

print(TS_SHORT$dy4[-1,])
par(mar = rep(2, 4))
par(mfrow = c(2, 1)) # c(rows, columns)
acf(TS_SHORT$y4[-1,])
pacf(TS_SHORT$y4[-1,])
par(mfrow = c(1, 1))

par(mfrow = c(2, 1)) # c(rows, columns)
acf(TS_SHORT$dy4[-1,], lag.max = 100)
pacf(TS_SHORT$dy4[-1,], lag.max = 100)
par(mfrow = c(1, 1))

plot(TS_SHORT$y4[-1,])
auto.arima(TS_SHORT$dy4[-1,]) #Result is ARIMA(0,0,4)
auto.arima(TS_SHORT$dy8[-1,]) #Result is ARIMA(0,0,1)
par(mfrow = c(1, 1))


par(mfrow = c(2, 1)) 
acf(TS_SHORT$dy4,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.1, 0.1),   # limits for the y axis - we give c(min, max)
    lwd = 5,               # line width
    col = "dark green",
    na.action = na.pass)   # do not stop if there are missing values in the data
pacf(TS_SHORT$dy4, 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1)) # we restore the original single panel
# it gives 4,1,4
arima111 <- Arima(TS_SHORT$y4,order = c(1, 1, 1))
arima111
coeftest(arima111)
summary(arima111)
arima111_2 <- Arima(TS_SHORT$y4,order = c(1, 1, 1), include.constant = TRUE)
coeftest(arima111_2)
############### Y4 ################
###################################
par(mfrow = c(2, 1)) # c(rows, columns)
acf(TS_SHORT$dy4,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.1, 0.1),   # limits for the y axis - we give c(min, max)
    lwd = 5,               # line width
    col = "dark green",
    na.action = na.pass)   # do not stop if there are missing values in the data
pacf(TS_SHORT$dy4, 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))

arima111 <- Arima(TS_SHORT$y4,order = c(1, 1, 1))
coeftest(arima111)
summary(arima111)
par(mfrow = c(2, 1)) # c(rows, columns)
acf(resid(arima111), 
    lag.max = 36,
    ylim = c(-0.1, 0.1), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(arima111), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima111),type = "Ljung-Box", lag = 10)


arima414 <- Arima(TS_SHORT$y4,order = c(4, 1, 4))
coeftest(arima414)
summary(arima414)
par(mfrow = c(2, 1)) # c(rows, columns)
acf(resid(arima414), 
    lag.max = 36,
    ylim = c(-0.1, 0.1), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(arima414), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima414),type = "Ljung-Box", lag = 10)



arima014 <- Arima(TS_SHORT$y4,order = c(0, 1, 4))
coeftest(arima014)
summary(arima014)
Box.test(resid(arima014),type = "Ljung-Box", lag = 10)

AIC(arima111, arima414, arima014)
BIC(arima111, arima414, arima014)

################## Y8 #############
###################################

auto.arima(TS_SHORT$y8) #Result is ARIMA(0,0,4)

par(mfrow = c(2, 1)) # c(rows, columns)
acf(TS_SHORT$dy8,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.1, 0.1),   # limits for the y axis - we give c(min, max)
    lwd = 5,               # line width
    col = "dark green",
    na.action = na.pass)   # do not stop if there are missing values in the data
pacf(TS_SHORT$dy8, 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))

arima8_111 <- Arima(TS_SHORT$y8,order = c(1, 1, 1))
coeftest(arima8_111)
summary(arima8_111)
par(mfrow = c(2, 1)) # c(rows, columns)
acf(resid(arima8_111), 
    lag.max = 36,
    ylim = c(-0.1, 0.1), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(arima8_111), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima8_111),type = "Ljung-Box", lag = 10)


arima8_810 <- Arima(TS_SHORT$y8,order = c(8, 1, 0))
coeftest(arima8_810)
summary(arima8_810)
par(mfrow = c(2, 1)) # c(rows, columns)
acf(resid(arima8_810), 
    lag.max = 36,
    ylim = c(-0.1, 0.1), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(arima8_810), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima8_810),type = "Ljung-Box", lag = 10)

arima8_810_Reduced_ars <- Arima(TS_SHORT$y8,order = c(8, 1, 0),
                                fixed=c(NA, 0, 0, NA,NA, 0, 0, NA))



coeftest(arima8_810_Reduced_ars)
summary(arima8_810_Reduced_ars)
Box.test(resid(arima8_810_Reduced_ars),type = "Ljung-Box", lag = 10)

AIC(arima8_111, arima8_810, arima8_810_Reduced_ars)
BIC(arima8_111, arima8_810, arima8_810_Reduced_ars)


####### forecasting
TS_TEST

forecasts_y4 <- forecast(arima014, h = 20)
length(TS_TEST)
length(forecasts_y4)
print(forecasts_y4)

print(TS_TEST$y4)

plot(forecasts_y4, main = "Forecast vs Actuals", col = "blue")
lines(TS_TEST$y4, col = "red")

accuracy_measures_y4 <- accuracy(forecasts_y4, TS_TEST$y4)
print(accuracy_measures_y4)



forecasts_y8 <- forecast(arima8_810_Reduced_ars, h = 20)

plot(forecasts_y8, main = "Forecast vs Actuals", col = "blue")
lines(TS_TEST$y8, col = "red")

accuracy_measures_y8 <- accuracy(forecasts_y8, TS_TEST$y8)
print(accuracy_measures_y8)
