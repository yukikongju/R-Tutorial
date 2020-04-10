# -------------------- Dependencies -----------------

library(ggplot2)
library(dyplr)
library(fpp2)

# -------------- Time Plot -------------------

autoplot(AirPassengers) + 
  ggtitle("Airline Passengers") + 
  xlab("Year") +
  ylab("Passengers")

# ------------------ Seasonal Plot -------------
