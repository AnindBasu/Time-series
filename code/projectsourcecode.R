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

library(xts)
library(quantmod)
library(ggthemes)
library(dygraphs)
library(tidyverse)
library(urca)
library(tseries)
library(forecast)
library(dplyr)
options(scipen = 999)

source("../code/functions/testdf.R")
source("../code/functions/utils.R")
source("../code/config.R")

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



############### Y4 ################
###################################
#Identifictin p=?, d=1 and q=?
par(mfrow = c(2, 1))
par(mfrow = c(1, 1))

# c(rows, columns)
## Plotting Actual Bitcoin Price
tibble(df = TS$y4) %>%
  ggplot(aes(zoo::index(TS), df)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "Actual Bitcoin Price",
    subtitle = paste0("Number of observations: ", length(TS$y4)),
    caption = "source: RR 2024",
    x="",
    y=""
  )+
  theme_gdocs() +
  theme(panel.background = element_rect(fill = "transparent",color = 4,size = 2))


## Plotting Log Transformed Bitcoin Price
tibble(df = TS$y4) %>%
  ggplot(aes(zoo::index(TS), log(TS$y4))) +
  geom_line() +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#67c9ff"))
scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "Log Transformed Bitcoin Price",
    subtitle = paste0("Number of observations: ", length(TS$y4)),
    caption = "source: RR 2024",
    x="",
    y=""
  )

## Plotting 1st Difference Log Operator
tibble(df = TS$y4) %>%
  ggplot(aes(zoo::index(TS), periodReturn(TS$y4, period="daily", type="log"))) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "1st Difference Log Operator",
    subtitle = paste0("Number of observations: ", length(TS$y4)),
    caption = "source: RR 2024",
    x="",
    y=""
  )

selectIndex500 <- (zoo::index(TS) <= as.Date('2022-03-07',YYY_MM_DD))

#original_500 <- window(ausbeer, start=1995)

original_500 <- TS[selectIndex500,]$y4
original_20_test <- tail(TS,20)$y4
first_diff_original_20_test <- periodReturn(original_20_test, period="daily", type="log")

log_transformed <- log(TS$y4)
log_transformed_500 <- log_transformed[selectIndex500,]
first_diff_log_operator_500 <- periodReturn(original_500, period="daily", type="log")

########
#Table 1. Stationary test of data start here
########

adf.original_500 <- adf.test(original_500)
pp.original_500 <- pp.test(original_500)
bg.original_500 <- bgtest(adfTest(original_500, lags = 0, type = "c")@test$lm$residuals~1, order = 1)

results_500 <- data.frame()
results_500 <- stationary_test_data(results_500, "Original data", "01/01/2012~14/05/2013", adf.original_500, pp.original_500, bg.original_500)

adf.log_transformed_500 <- adf.test(log_transformed_500)  
pp.log_transformed_500 <- pp.test(log_transformed_500)  
bg.log_transformed_500 <- bgtest(adfTest(log_transformed_500, lags = 0, type = "c")@test$lm$residuals~1, order = 1)

results_500 <- stationary_test_data(results_500, "Log transformed data", "01/01/2012~14/05/2013", adf.log_transformed_500, pp.log_transformed_500, bg.log_transformed_500)

adf.first_diff_log_operator_500 <- adf.test(first_diff_log_operator_500)  
pp.first_diff_log_operator_500 <- pp.test(first_diff_log_operator_500)  
bg.first_diff_log_operator_500 <- bgtest(adfTest(first_diff_log_operator_500, lags = 0, type = "c")@test$lm$residuals~1, order = 1)

results_500 <- stationary_test_data(results_500, "Log return (1st difference)", "01/01/2012~14/05/2013", adf.first_diff_log_operator_500, pp.first_diff_log_operator_500, bg.first_diff_log_operator_500)

print(results_500)

plot_acf_pacf(first_diff_log_operator_500)
models_detail <- data.frame()
for (p in 1:4) {
  for (q in 1:4) {
    cur_model <- Arima(first_diff_log_operator_500, order=c(p,1,q))
    models_detail <- model_summary(models_detail, paste("c(",p,",1,",q,")",sep=""), cur_model)
  }
}
print(models_detail)
#select the best model where AIC and BIC are minimum (max negative value)
#7  c( 2 ,1, 3 )      0.0239271216 -2280.516 -2258.708
#15 c( 4 ,1, 3 )      0.2033242976 -2282.713 -2253.635

#Analyzing (2,1,3)
y4_model_213 <- Arima(first_diff_log_operator_500, order=c(2,1,3))

# Diagnostics
y4_residuals_213 = resid(y4_model_213)
#plot acf and pacf for residuals
plot_acf_pacf(y4_residuals_213)
y4_residuals_acf_test_213 <- data.frame()
adf.y4_residuals_213 <- adf.test(y4_residuals_213)  
pp.y4_residuals_213 <- pp.test(y4_residuals_213)
bg.y4_residuals_213 <- bgtest(adfTest(y4_residuals_213, lags = 0, type = "c")@test$lm$residuals~1, order = 1)

y4_residuals_acf_test_213 <- stationary_test_data(y4_residuals_acf_test_213, "Log transformed data", "01/01/2012~14/05/2013", adf.y4_residuals_213, pp.y4_residuals_213, bg.y4_residuals_213)

print(y4_residuals_acf_test_213)
# Ljung-Box test for autocorrelation in residuals
Box.test(y4_residuals_213, type = "Ljung-Box", lag = 10)

coeftest(y4_model_213)

#Analyzing (4,1,3)
y4_model_413 <- Arima(first_diff_log_operator_500, order=c(4,1,3))

# Diagnostics
y4_residuals_413 = resid(y4_model_413)
#plot acf and pacf for residuals
plot_acf_pacf(y4_residuals_413)
y4_residuals_acf_test_413 <- data.frame()
adf.y4_residuals_413 <- adf.test(y4_residuals_413)  
pp.y4_residuals_413 <- pp.test(y4_residuals_413)  
bg.y4_residuals_413 <- bgtest(adfTest(y4_residuals_413, lags = 0, type = "c")@test$lm$residuals~1, order = 1)

y4_residuals_acf_test_413 <- stationary_test_data(y4_residuals_acf_test_413, "Log transformed data", "01/01/2012~14/05/2013", adf.y4_residuals_413, pp.y4_residuals_413, bg.y4_residuals_413)

print(y4_residuals_acf_test_413)
# Ljung-Box test for autocorrelation in residuals
# as https://robjhyndman.com/hyndsight/ljung-box-test/ suitable value for lag is 10
Box.test(y4_residuals_413, type = "Ljung-Box", lag = 10)

coeftest(y4_model_413)

# As ar1, ar2 and ar3 are not significant and ma2 as well, we can remove them from model

y4_model_413_modified <- Arima(first_diff_log_operator_500, order=c(4,1,3),
                      fixed = c(0, 0, 0, NA, NA,0, NA))

y4_residuals_413_modified = resid(y4_model_413_modified)
plot_acf_pacf(y4_residuals_413_modified)
Box.test(y4_residuals_413_modified, type = "Ljung-Box", lag = 10)
coeftest(y4_model_413_modified)
# Ljung-Box p value is 0.04991 < .05 reject the null hypothesis that residuals are non auto
# correlated, earlier model shows p value 0.2033 > .05 failed to reject the null hypothesis therefore 
# residuals are white noise and model 413 is best accepted model


y4_model_413.forecast <- forecast(y4_model_413, h = length(first_diff_original_20_test))
plot(y4_model_413.forecast, main = "Graph with forecasting", )
lines(first_diff_original_20_test, col = "red")
legend("topright", legend = c("Forecasted Values", "Actual Values"), 
       col = c("blue", "red"), lty = 1)

accuracy(y4_model_413.forecast, first_diff_original_20_test)



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

#• For comparing VECM model’s forecasts with ARIMAs alternatives – 5 pts.




