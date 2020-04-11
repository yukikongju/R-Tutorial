# -------------------- Dependencies ------------

library(ggplot2)
library(dplyr)
library(fpp2)

# ---------------- 6.2 Moving average -------------

autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")

ma(elecsales, 5)

autoplot(elecsales, series = "Data")+
  autolayer(ma(elecsales,5), series = "5-MA")+
  xlab("Year")+ ylab("GWh")+
  ggtitle("Annual electricity sales: South Australia")

# ------------- 6.2 Moving averages of moving averages -----------

d_beers <- window(ausbeer, start=1992)
ma4 <- ma(d_beers, order = 4, centre = FALSE)
ma2x4 <- ma(d_beers, order = 4, centre = TRUE)

autoplot(elecequip, series = "Data")+
  autolayer(ma(elecequip, 12), series = "12-MA")+
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

# ----- 6.3 Classical decomposition : Multiplicative decomposition ----------

elecequip %>% 
  decompose(type = "multiplicative") %>% 
  autoplot()

# ------------ 6.4 X11 decomposition ---------------

library(seasonal)
elecequip %>%  seas(x11="") -> fit
autoplot(fit)
  
autoplot(elecequip, series = "Data")+
  autolayer(trendcycle(fit), series = "Trend")+
  autolayer(seasadj(fit), series = "Seasonally ADjusted")+
  scale_colour_manual(values = c("gray", "blue", "red"))

fit %>% seasonal() %>% ggsubseriesplot()+ylab("Seasonal")

# ------------ 6.5 SEATS decomposition --------------

elecequip %>% 
  seas() %>% 
  autoplot()

# ------------ 6.5 STLdecomposition --------------

elecequip %>% 
  stl(s.window = "periodic", t.window = 13 ) %>% 
  autoplot()

# ---------- 6.8 Forecasting with decomposition ----------

fit <- stl(elecequip, t.window = 13, s.window = "periodic", robust = TRUE)

fit %>% seasadj() %>% naive() %>% 
  autoplot() 

fit %>% forecast(method="naive") %>% autoplot()

fcast <- stlf(elecequip, method="naive")

# --------------- Exercice 2 ----------

# a. plot ts
autoplot(plastics)
# multiplicative decomposition bc mean increase over time. increase mid year

# b. multiplicative decomp 
plastics %>% 
  decompose(type = "multiplicative") %>% 
  autoplot()

# d. seasonally adjusted data

plastics_decomposed <- plastics %>% decompose(type = "multiplicative")

autoplot(plastics, series = "Data")+
  autolayer(trendcycle(plastics_decomposed), series="Trend")+
  autolayer(seasadj(plastics_decomposed), series="Seasonally adjusted")+
  scale_color_manual(values = c("gray","blue", "red"))
  
# ------------------- Exercice 5 --------------

# a. plot ts
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)

# b. stl decomp
stl_cangas <- stl(cangas, s.window = "periodic", t.window = 13, robust = TRUE)
autoplot(stl_cangas)

# c. x11 and seats decomp

# seats
cangas %>% seas() %>% autoplot()
cangas %>% seas(x11="") %>% autoplot()
# note: the seasonal and trend remain the same, but it seems that the remainder component has changed (increased)

# -------------- Exercice 6 ---------------

# a. stl decomp
stl_bricksq_fixed <- stl(bricksq,s.window = "periodic", t.window = 13, robust = TRUE)
autoplot(stl_bricksq_fixed)

stl_bricksq_changing <- stl(bricksq,s.window = 5, robust = TRUE)
autoplot(stl_bricksq_changing)


# b. season adj data
autoplot(bricksq, series = "Data")+
  autolayer(trendcycle(stl_bricksq_fixed), series = "Trend")+
  autolayer(seasadj(stl_bricksq_fixed), series = "Season Adj")

# c.naive method and season adj

stl_bricksq_fixed %>% seasadj() %>% naive() %>% autoplot()
stl_bricksq_changing %>% seasadj() %>% naive() %>% autoplot()








