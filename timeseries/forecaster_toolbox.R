# -------------------- Dependencies ------------

library(ggplot2)
library(dyplr)
library(fpp2)

# ----------- 3.1 Forecasting methods with seasonal data --------------

d_beers <- window(ausbeer, start = 2000)

autoplot(d_beers)+
  autolayer(meanf(d_beers, h=11),
            series = "Mean", PI = FALSE) +
  autolayer(naive(d_beers, h=11),
            series = "Naive", PI = FALSE) +
  autolayer(snaive(d_beers, h=11),
            series = "Seasonal Naive", PI = FALSE)

# ----------- 3.1 Forecasting methods with non seasonal data --------------

autoplot(goog200)+
  autolayer(meanf(goog200, h=40),
            series = "Mean", PI = FALSE)+
  autolayer(rwf(goog200, h=40),
            series = "Naive", PI = FALSE)+
  autolayer(rwf(goog200, drift = TRUE, h=40),
            series = "Drift", PI = FALSE)

# ------ 3.2 Calendar Adjustments --------------

d_milk <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(d_milk, facets = TRUE)

# ------ 3.2 Population Adjustments --------------
# ------ 3.2 Inflation Adjustments --------------
# ------ 3.2 Maths Transformations --------------

(elec.lambda <- BoxCox.lambda(elec))
autoplot(BoxCox(elec, elec.lambda))

# ------ 3.2 Bias Adjustments --------------

fc <- rwf(eggs, drift = TRUE, lambda = 0, h=50, level = 80)
fc2 <- rwf(eggs, drift = TRUE, lambda = 0, h=50, level = 80, biasadj = TRUE)

autoplot(eggs)+
  autolayer(fc, series = "Simple back Transformation", PI = TRUE)+
  autolayer(fc2, series = "Bias adjusted", PI = FALSE)

# ------ 3.3 Residual diagnostis with google stock ----------

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))

autoplot(res)+
  xlab("day")+ ylab("")+
  ggtitle("Residuals from naive method")

gghistogram(res)+ ggtitle("Histogram of Naive Method residuals")

ggAcf(res) + ggtitle("ACF of residuals")

Box.test(res, lag=10, fitdf = 0)
Box.test(res, lag=10, fitdf = 0, type = "Lj")

checkresiduals(naive(goog200))

# --------- 3.4 Evaluating forecast accuracy with seasonal data -------------

d_ausbeer <- window(ausbeer, start = 1992, end= c(2007, 4))

beerfit1 <- meanf(d_ausbeer, h=10)
beerfit2 <- naive(d_ausbeer, h=10)
beerfit3 <- snaive(d_ausbeer, h=10)
  
autoplot(window(ausbeer, start = 1992)) + 
  autolayer(beerfit1,
            series = "Mean", PI = FALSE) +
  autolayer(beerfit2,
            series = "Naive", PI = FALSE) +
  autolayer(beerfit3,
            series = "Seasonal Naive", PI = FALSE) 

d_ausbeer2 <- window(ausbeer, start= 2008)
accuracy(beerfit1, d_ausbeer2)
accuracy(beerfit2, d_ausbeer2)
accuracy(beerfit3, d_ausbeer2)


# --------- 3.4 Evaluating forecast accuracy with non seasonal data -------------

googfit1 <- meanf(goog200, h=40)
googfit2 <- rwf(goog200, h=40)
googfit3 <- rwf(goog200, h=40, drift = TRUE)

autoplot(subset(goog, end = 240)) +
  autolayer(googfit1, series = "Mean", PI = FALSE) +
  autolayer(googfit2, series = "Naive", PI = FALSE) +
  autolayer(googfit3, series = "Drift", PI = FALSE)

googtest <- window(goog, start=201, end=240)
accuracy(googfit1, googtest)

# --------- 3.4 Making forecast with cross validation -------------

e <- tsCV(goog200, forecastfunction = naive, h=8)

mse <- colMeans(e^2, na.rm = TRUE)

data.frame(h=1:8, MSE = mse) %>% 
  ggplot(aes(x=h, y=MSE)) +geom_point()

# --------- 3.4 Evaluating forecast accuracy with cross validation -------------

sub <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(sub^2, na.rm = TRUE))
sqrt(mean(residuals(rwf(goog200, drift = TRUE))^2, na.rm = TRUE))

# --------------- Exercice 1 ------------------



# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------
# --------------- Exercice 1 ------------------