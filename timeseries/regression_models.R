# -------------------- Dependencies ------------

library(ggplot2)
library(dplyr)
library(fpp2)

# --------------- 5.1 single regression: US consumption expenditure -------------

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>% 
  as.data.frame() %>% 
  ggplot(aes(Consumption, Income)) +
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)

tslm(data = uschange, Consumption~Income)

# --------------- 5.1 multiple linear regression: US consumption expenditure -------------

uschange %>% 
  as.data.frame() %>% 
  GGally:: ggpairs()

# ----------------- 5.2 estimating the coefficients with least squares -----------------

fit.uschange <- tslm(data = uschange, Consumption ~ Income + Production + Unemployment + Savings)
summary(fit.uschange)

autoplot(uschange[,'Consumption'], series = "Data") +
  autolayer(fitted(fit.uschange), series = "Fitted") + 
  guides(color= guide_legend(title = ""))+
  ggtitle("Percent Change in US Consumption")+
  xlab("Year")+ ylab("")

cbind(Data = uschange[,'Consumption'],
      Fitted = fitted(fit.uschange)) %>% 
  as.data.frame() %>% 
  ggplot(aes(Data, Fitted))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)
  
# ---------- 5.3 Evaluating the regression model ----------------

# make acf and histogram
checkresiduals(fit.uschange)

# make scatterplot of residuals
df <- as.data.frame(uschange)

df[, "Residuals"] <- as.numeric(residuals(fit.uschange))

p1 <- df %>% ggplot(aes(Income, Residuals)) + geom_point()
p2 <- df %>% ggplot(aes(Production, Residuals)) + geom_point()
p3 <- df %>% ggplot(aes(Savings, Residuals)) + geom_point()
p4 <- df %>% ggplot(aes(Unemployment, Residuals)) + geom_point()

gridExtra:: grid.arrange(p1,p2,p3,p4, nrow=2)

# other way to make residuals scatterplot

cbind(Fitted=fitted(fit.uschange),
      Residuals = residuals(fit.uschange)) %>% 
  as.data.frame() %>% 
  ggplot(aes(Fitted, Residuals))+geom_point()

# ------------------ 5.4 Australian beer production example --------------

d_beers <- window(ausbeer, start=1992)

fit.beer <- tslm(d_beers~ trend + season)
summary(fit.beer)

autoplot(d_beers, series = "Data") + 
  autolayer(fitted(fit.beer), series = "Fitted")+
  xlab("Year") + ylab("Mega Litres")

cbind(Data = d_beers, Fitted = fitted(fit.beer)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Data, y=Fitted, colour= as.factor(cycle(d_beers)))) +
  geom_point()+
  scale_color_brewer(palette = "Dark2", name= "Quarter")+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "lm", se=FALSE)

# --------------- 5.6 Forecasting with regression : beer example ---------------

d_beers <- window(ausbeer, start=1992)
fit.beer <- tslm(d_beers ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forcast of beer production using a regression")+
  xlab("Year")+ylab("Mega Litres")

# --------------- 5.6 Forecasting with regression : uschange example ---------------

fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange
)

h <- 4

newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))

fcast.up <- forecast(fit.consBest, newdata= newdata)

newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))

fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[, 'Consumption']) +
  autolayer(fcast.up, PI=TRUE, series ="increase")+
  autolayer(fcast.down, PI = TRUE, series = "decrease")+
  guides(color= guide_legend(title = "Scenario"))+
  ylab("% change in Us Consumption")

# -------------- 5.8 Non linear regresson : marathon ds ------------------

# testing all transformations

h <- 10 
fit.lin <- tslm(marathon~trend)
fcast.lin <- forecast(fit.lin, h=h)
fit.exp <- tslm(marathon~trend, lambda = 0)
fcast.exp <- forecast(fit.exp, h=h)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t -t.break1), start = 1897)
tb2 <- ts(pmax(0, t -t.break2), start = 1897)

fit.pw <- tslm(marathon~ t+tb1+tb2) 
t.new <- t[length(t)] +seq(h)
tb1.new <- tb1[length(tb1)] +seq(h)
tb2.new <- tb2[length(tb2)] +seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>% 
  as.data.frame()
fcast.pw <- forecast(fit.pw, newdata=newdata)

fit.spline <- tslm(marathon~ t + I(t^2)+I(t^3)+I(tb1^3)+I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata=newdata)

autoplot(marathon)+
  autolayer(fitted(fit.lin), series = "Linear")+
  autolayer(fitted(fit.exp), series = "Exponential")+
  autolayer(fitted(fit.pw), series = "Power")+
  autolayer(fitted(fit.spline), series = "Spline")+
  autolayer(fcast.lin, series = "Linear", PI=FALSE)+
  autolayer(fcast.exp, series = "Exponential", PI=FALSE)+
  autolayer(fcast.pw, series = "Piecewise", PI=FALSE)+
  autolayer(fcasts.spl, series = "Cubic Spline", PI=FALSE)


# -------------- 5.8 Non linear regresson : marathon ds pt 2------------------

# before transformation
autoplot(marathon)

# after transformation
marathon %>%
  splinef(lambda=0) %>%
  autoplot()

# check residuals after transformation
marathon %>% 
  splinef(lambda = 0) %>% 
  checkresiduals()

# ---------------------- Exercices 1 ----------------------------

# a
daily20 <- head(elecdaily, 20)
fit.daily20 <- tslm(data= daily20, Demand~Temperature)
summary(fit.daily20)

autoplot(elecdaily[,c('Demand')], series = "Data")+
  autolayer(fitted(fit.daily20), series="Fitted")

daily20 %>% 
  as.data.frame() %>% 
  ggplot(aes(Temperature, Demand))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE)

#b
checkresiduals(fit.daily20$residuals)

#c
new_obs <- data.frame(Temperature=c(15,35))
fcast_temp <- forecast(fit.daily20, newdata = new_obs) 

#e
elecdaily %>% 
  as.data.frame() %>% 
  ggplot(aes(Temperature, Demand))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE)

# -------------------- Exercice 2 ----------------

# a. plot winning time against year
autoplot(mens400)

# b. fit regression line to data

fit.mens400 <- tslm(data= mens400, mens400~trend)
fit.mens <- tslm(data = mens400, mens400~ time(mens400))

autoplot(mens400)+
  autolayer(fitted(fit.mens400), series="Fitted 1")+
  autolayer(fitted(fit.mens), series="Fitted 2")

# note: the two fitted line are the same

fit.mens400$coefficients[2]
# The winning times have been decreasing at average rate of 0.2582954 second per year.

fit.mens$coefficients[2]
# The winning times have been decreasing at average rate of 0.06457385 second per year.

# c. plot resudal agaisnt the years

time <- time(mens400)
cbind(Time= time,
      Residuals= fit.mens$residuals) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Time, y=Residuals))+
  geom_point()

checkresiduals(fit.mens)

# d. forecast winning time in 2020

lm_mens400 <- lm(mens400~time, data = mens400, na.action = na.exclude)

fcast_mens <- forecast(lm_mens400, newdata = data.frame(time=2020))

# ------------------ Exercice 3 ------------------

easter_beer <- easter(ausbeer)

# ------------------ Exercice 5 ------------------

autoplot(fancy)
# sales increses around december

fit_log_fancy <- tslm(BoxCox(fancy, 0)~ trend+season)

autoplot(fit_log_fancy$residuals)
checkresiduals(fit_log_fancy$residuals)

# d. plot time and residuals

cbind(Time= time(fancy),
      Residuals= fit_log_fancy$residuals) %>% 
  as.data.frame() %>% 
  ggplot(aes(Time, Residuals))+
  geom_point()

# e. boxplot of the residuals for each month

cbind(Values=fancy, Residuals= fit_log_fancy$residuals) %>% 
  as.data.frame() %>% 
  ggplot(aes(y=Residuals))+
  geom_boxplot()

ggsubseriesplot(fit_log_fancy$residuals)

# f. coefficients values

fit_log_fancy$coefficients

# g. bresuch godfrey

checkresiduals(fit_log_fancy)

# h. forecast for 1994-1996

