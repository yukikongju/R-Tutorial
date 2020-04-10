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













