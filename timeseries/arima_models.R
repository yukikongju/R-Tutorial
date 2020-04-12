# -------------------- Dependencies ------------

library(ggplot2)
library(dyplr)
library(fpp2)


# --------------------- 8.5 non seasonal arima model-----------

fit <- auto.arima(uschange[,'Consumption'], seasonal = FALSE)
fit %>% forecast(h=10) %>% autoplot(include=80)
ggAcf(uschange[,'Consumption'])
ggPacf(uschange[,'Consumption'])
checkresiduals(fit)

# --------------------- 8.7 Seasonally adjusted -----------

elecequip %>% stl(s.window = 'periodic') %>% seasadj()-> eeadj
autoplot(eeadj)

eeadj %>% diff() %>% ggtsdisplay()

checkresiduals(eeadj)

fit <- Arima(eeadj, order = c(3,1,1))
checkresiduals(fit)
autoplot(fit)
autoplot(forecast(fit))

# ----------------------- 8.9 Arima model with seasonal data : retail ------------

euretail %>% diff(lag=4) %>% ggtsdisplay()
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()
# note: the plot isnt stationarry so we differencing again

euretail %>% 
  Arima(order = c(0,1,1), seasonal = c(0,1,1)) %>% 
  residuals() %>% 
  ggtsdisplay()

fit <- Arima(euretail, order = c(0,1,1), seasonal = c(0,1,1))
checkresiduals(fit)
  
fit %>% forecast(h=12) %>% autoplot()

# ----------------------- 8.9 Arima model with seasonal data : drugs sales ------------

# transform data
autoplot(log_h02)
log_h02 <- log(h02)

# differenciate data
log_h02 %>% diff(h=12) %>% ggtsdisplay()

# fit arima check residuals
fit_h02 <- Arima(h02, order = c(3,0,1), seasonal = c(0,1,2), lambda = 0)
checkresiduals(fit_h02, lag=36)

# forecast
fit_h02 %>% forecast() %>% autoplot()

# ------- 8.10 comparing auto.arima() and ets() on non-seasonal data ------

fcast_ets <- function(ts, h){
  forecast(ets(ts), h=h)
} 

fcast_arima <- function(ts, h){
  forecast(auto.arima(ts), h=h)
}

# compute error
e1 <- tsCV(air, fcast_ets, h=1)
e2 <- tsCV(air, fcast_arima, h=1)

mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# plot best model vs mean: ets
air %>% ets() %>% forecast() %>% autoplot()

# ------- 8.10 comparing auto.arima() and ets() on seasonal data ------

# separate train
win_cement <- window(qcement, start=1980)
train <- window(qcement, end = c(2007,4))

# fit models
fit_arima <- auto.arima(train)
checkresiduals(fit_arima)

fit_ets <- ets(train)
checkresiduals(fit_ets)

# check accuracy
a_arima <- fit_arima %>% forecast(h= 4*(2013-2007)+1) %>% 
  accuracy(qcement)

a_est <- fit_ets %>% forecast(h=4*(2013-2007)+1) %>% 
  accuracy(qcement)

# plot highest accuracy
qcement %>% auto.arima() %>% forecast(h=12) %>% autoplot()
