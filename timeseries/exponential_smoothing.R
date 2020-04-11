# -------------------- Dependencies ------------

library(ggplot2)
library(dyplr)
library(fpp2)

# ----------------- 7.1 Oil production --------------

oildata <- window(oil, start=1999)
fc <- ses(oildata, h=5)
round(accuracy(fc), 2)

autoplot(fc)+
  autolayer(fitted(fc), series = "Fitted")

# ------------- 7.2 Holt's method ---------------

air <- window(ausair, start= 1990)
fc_air <- holt(air) 
fc_air2 <- holt(air, damped = TRUE, phi = 0.9, PI=FALSE)

autoplot(air)+
  autolayer(fitted(fc_air), series = "Holt's method")+
  autolayer(fitted(fc_air2), series = "Holt's dampened")

# -------------- 7.3 holt-winter's method -----------

tourists <- window(austourists, start=2005)
fit_add <- hw(tourists, seasonal = "additive")
fit_mul <- hw(tourists, seasonal = "multiplicative")
autoplot(tourists) + 
  autolayer((fit_add), series = "Additive HW", PI=FALSE)+
  autolayer((fit_mul), series = "Multiplicative HW", PI=FALSE)+
  guides(colour=guide_legend(title="Forecast"))

tourists %>% decompose(type = "multiplicative") %>% autoplot()
tourists %>% decompose(type = "additive") %>% autoplot()


# -------------- 7.3 holt-winter's method : daily data -----------

fc <- hw(subset(hyndsight, end = length(hyndsight)-35),
         damped = TRUE, h=35, seasonal = "multiplicative")
autoplot(hyndsight)+
  autolayer(fc, series = "HW multiplicative damp")
