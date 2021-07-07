library(fpp2)
library(forecast)
library(ggplot2)
library(ggfortify)

#Read in data
getwd()
setwd('C:/Users/Steve/dev/Grad School/BANA6630/Homework/Homework3/')
install.packages("gdata")
library(readxl)
data = readxl::read_xls('bicup2006.xls')

#format date
data$DATE <- format(data$DATE, "%Y-%m-%d") # change to Date variable
#create weekend dummy var:
data$weekend = 0 
data$weekend[which(data$DATE == "2005-03-05" | data$DATE == '2005-03-06' | data$DATE == '2005-03-12' | data$DATE == '2005-03-13' | data$DATE == '2005-03-19' | data$DATE == '2005-03-20')] = 1


#Split series into weekday and weekend data.
weekdays = data[data$weekend == 0,]
weekends = data[data$weekend == 1,]

#Create full series
series = ts(data[,3], start = c(1,1), end = c(21, 63), freq = 63)
autoplot(series)
#Create weekday ts
weekday_ts = ts(weekdays[,3], start = c(1,1), end = c(15, 63), freq = 63)
autoplot(weekday_ts)
#partition weekday data
weekday_train = window(weekday_ts, start = c(1,1), end = c(10, 63))
weekday_valid = window(weekday_ts, start = c(11, 1), end = c(15, 63))

#Create weekday msts
weekday_msts = msts(weekday_ts, seasonal.periods = c(24, 63))
weekday_msts_train = msts(weekday_train, seasonal.periods = c(24, 63))
weekday_msts_valid = msts(weekday_valid, seasonal.periods = c(24, 63))

plot(mstl(weekday_ts))

#Create weekend ts
weekend_ts = ts(weekends[,3], start = c(1,1), end = c(6, 63), freq = 63)
autoplot(weekend_ts)
#partition weekend data
weekend_train = window(weekend_ts, start = c(1,1), end = c(4, 63))
weekend_valid = window(weekend_ts, start = c(5, 1), end = c(6, 63))

#Create weekend msts
weekend_msts = msts(weekend_ts, seasonal.periods = c(24, 63*2))
weekend_msts_train = msts(weekend_train, seasonal.periods = c(24, 63*2))
weekend_msts_valid = msts(weekend_valid, seasonal.periods = c(24, 63*2))

plot(mstl(weekend_ts))

#########################################################
#Create weekday AR and ARIMA
weekday_ar=arima(weekday_ts,order=c(1,0,0))
weekday_ar

plot(weekday_ar$residual,type='l')## checks residuals
Box.test(weekday_ar$residual,lag=10,type='Ljung') 

weekday_arima1=arima(weekday_ts,order=c(2,0,0))
weekday_arima1
tsdiag(weekday_arima1)
plot(weekday_arima1$residuals,type='l')
Box.test(weekday_arima1$residuals,lag=10,type='Ljung')

weekday_arima2 = arima(weekday_ts, order = c(2,0,2))
weekday_arima2
tsdiag(weekday_arima2)
Box.test(weekday_arima2$residuals,lag=10,type='Ljung')
plot(weekday_arima2$residuals,type='l')


##############################################################################
#Create weekend AR and ARIMA
weekend_ar=arima(weekend_ts,order=c(1,0,0))
weekend_ar

plot(weekend_ar$residual,type='l')## checks residuals
Box.test(weekend_ar$residual,lag=10,type='Ljung') 

weekend_arima1=arima(weekend_ts,order=c(2,0,0))
weekend_arima1
tsdiag(weekend_arima1)
plot(weekend_arima1$residuals,type='l')
Box.test(weekend_arima1$residuals,lag=10,type='Ljung')

weekend_arima2 = arima(weekend_ts, order = c(2,0,2))
weekend_arima2
tsdiag(weekend_arima2)
plot(weekend_arima2$residuals,type='l')
Box.test(weekend_arima2$residuals,lag=10,type='Ljung')


########################################################
#Create tbats model on weekday train series.
m1_tbats = tbats(weekday_msts_train)

#Check components of model.
components_m1 = tbats.components(m1_tbats)
plot(components_m1)


#plot weekday series and predictions.
plot(weekday_msts, col = 'blue')
lines(m1_tbats$fitted.values, col = 'red')
m1_tbats_preds = forecast(m1_tbats, h = 63*5)
lines(m1_tbats_preds$mean, col = 'green')

#calculate m1_rmse
accuracy(m1_tbats)
accuracy(m1_tbats_preds$mean, weekday_msts_valid)
weekday_msts_valid

###################################################
#Create tbats model on weekend_train series
m2_tbats = tbats(weekend_msts_train)

#check components of model.
components_m2 = tbats.components(m1_tbats)
plot(components_m2)


#plot weekend series and predictions.
plot(weekend_msts, col = 'blue')
lines(m2_tbats$fitted.values, col = 'red')
m2_tbats_preds = forecast(m2_tbats, h = 63*2)
lines(m2_tbats_preds$mean, col = 'green')
#AIC with 24, 63*2: 1653.17

#calculate m2_rmse
accuracy(m2_tbats)
accuracy(m2_tbats_preds$mean, weekend_msts_valid)


###################################################
#Predict 3 days into the future using only weekday series
m3_tbats = tbats(weekday_msts)

#Check components of model.
components_m3 = tbats.components(m3_tbats)
plot(components_m3)

#Calculate RMSE
accuracy(m3_tbats)

#plot weekday series and predictions.
plot(series, col = 'blue', xlim = c(0, 26), main = 'Predict Three Days Out')
m3_tbats_preds = forecast(m3_tbats, h = 63*3)
predictions = as.numeric(m3_tbats_preds$mean)
predictions = ts(predictions, start = c(22,1), end = c(24, 63), freq = 63)
lines(predictions, col = 'red')


##########################################################################
# Exponetial Smoothing

#Create weekday ts
weekday_ts = ts(weekdays[,3], start = c(1,1), end = c(15, 63), freq = 63)
weekday_ts1 <- diff(diff(weekday_ts, lag = 315), lag = 1)
length(weekday_ts1)
# partition
train.ts <- window(weekday_ts1, start = c(6,2), end = c(12,63))
valid.ts <- window(weekday_ts1, start = c(13,1), end = c(15,63))
# Simple Exponential Smoothing on twice-differenced data
smooth <- ets(train.ts, model = "ANN")
smooth.pred <- forecast(smooth,  h = (3*63), level = 0 )
plot(smooth)
plot(smooth.pred)
accuracy(smooth)
# RMSE = 12.3
# surprisingly good result for simple exponential smoothing


##############################################################################
# Holt-Winters Exponential Smoothing
# holt winters cannot accept 0 or negative values. 2 zeros exists, replace with 1s
weekday_msts_train[weekday_msts_train < 1] = 1
holt_winters.pred <- dshw(weekday_msts_train[weekday_msts_train > 0], period1 = 63, period2 = 315, h = length(weekday_msts_valid)) # 315 = 63 periods in a day * 5 days in a week
holt.winters.pred.mean <- msts(holt_winters.pred$mean, seasonal.periods = c(63,315), start = c(11,1))
accuracy(holt.winters.pred.mean, weekday_msts_valid)
plot(holt_winters.pred)
# RMSE = 9.1


#HW with full dataset
weekday_msts[weekday_msts < 1] = 1
holt_winters.pred_full <- dshw(weekday_msts[weekday_msts > 0], period1 = 63, period2 = 315, h = 63*3) # 315 = 63 periods in a day * 5 days in a week
accuracy(holt_winters.pred_full)
plot(holt_winters.pred_full)


#plot full series with predictions
plot(series, col = 'blue', xlim = c(0, 26), main = 'HW Predict Three Days Out')
hwpred = forecast(holt_winters.pred_full, h = 63*3)
predictions = as.numeric(hwpred$mean)
predictions = ts(predictions, start = c(22,1), end = c(24, 63), freq = 63)
lines(predictions, col = 'red')
