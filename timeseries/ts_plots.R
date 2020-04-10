# -------------------- Dependencies -----------------

library(ggplot2)
library(dyplr)
library(fpp2)

# -------------- Time Plot -------------------

autoplot(AirPassengers) + 
  ggtitle("Airline Passengers") + 
  xlab("Year") +
  ylab("Passengers")


# -------------- Time Plot with facets -------------------

autoplot(elecdaily[,c("Demand", "Temperature")], facets = TRUE)+
  ylab("")+
  ggtitle("Daily Demand in Victoria, Australia")

# ------------------ Seasonal Plot -------------

ggseasonplot(AirPassengers, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Passengers") +
  ggtitle("Air Passengers by seasons")

# ------------------ Polar Seasonal Plot -------------

ggseasonplot(AirPassengers, polar = TRUE)+
  ylab("Passengers") +
  ggtitle("Polar plot: Air Passengers by seasons")

# --------- Seasonal Subseries Plots ------------------

ggsubseriesplot(AirPassengers) + 
  ylab("Passengers") +
  ggtitle("Subseries Plot: Air Passengers by seasons")

# ------------- Scatter plot matrices -----------------------

autoplot(visnights[,1:4], facets=TRUE)+
  ylab("Number of visitors each quarter (millions)")

# ------------------- qplot ---------------------

qplot(data = as.data.frame(elecdemand), Temperature, Demand )

# -------------- Corrrelation plot --------------

GGally:: ggpairs(as.data.frame(visnights[,1:5]))

# ------------------ Lag plots -------------------

d_beer <- window(ausbeer, start = 1985)
gglagplot(d_beer)

# ------------ autocorrelation plot ---------------

ggAcf(d_beer, lag.max = 40)

# ---------- Trend and seasonality in ACF plots ---

d_elec <- window(elec, start = 1975)
autoplot(d_elec) + xlab("Year") + ylab("Demand")

# ----------- white noise plot -----------------

set.seed(420)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("white noise")

# ----------- Exercices 1 -------------------------

autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

frequency(gold)
frequency(woolyrnq)
frequency(gas)

# find outliers
which.max(gold)

# ----------- Exercice 4 --------------------

autoplot(bicoal)
autoplot(chicken)
autoplot(usdeaths)
autoplot(goog) + ggtitle("Google stock price ")

# ----------- Exercice 5 --------------------

ggseasonplot(writing)
ggsubseriesplot(writing)

ggseasonplot(fancy)
ggsubseriesplot(fancy)

# ----------- Exercice 6 --------------------

autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)

# ----------- Exercice 7 --------------------

autoplot(arrivals, facets = TRUE)

ggseasonplot(arrivals[, "Japan"])
ggseasonplot(arrivals[, "NZ"])
ggseasonplot(arrivals[, "UK"])
ggseasonplot(arrivals[, "US"])

ggsubseriesplot(arrivals[, "Japan"])
ggsubseriesplot(arrivals[, "NZ"])
ggsubseriesplot(arrivals[, "UK"])
ggsubseriesplot(arrivals[, "US"])

# ----------- Exercice 9 --------------------

d_pigs <- window(pigs, start=1990)

autoplot(d_pigs)
ggseasonplot(d_pigs)
ggsubseriesplot(d_pigs)
gglagplot(d_pigs)
ggAcf(d_pigs)

# ---------- Exercice 10 ---------------

djj <- diff(dj)
autoplot(djj)
ggAcf(djj)

# Note: probably white noise
