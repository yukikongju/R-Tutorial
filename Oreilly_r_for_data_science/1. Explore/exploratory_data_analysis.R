library(tidyverse)
library(ggstance)
library(hexbin)




# ----------------- Visualising distributions ---------------------

# categorical data: geom_bar()
diamonds %>% ggplot() +
  geom_bar(aes(cut))

diamonds %>% count(cut)

# continuous data: geom_hist()
diamonds %>% ggplot() +
  geom_histogram(aes(price), binwidth = 1000)

diamonds %>% count(cut_width(price, 1000))

# different bindwith and smaller dataset
diamonds %>% filter(price > 15000) %>%
  ggplot() + geom_histogram(aes(price), binwidth = 300)

# overlay multiple histograms in the same plot : geom_freqpoly()
diamonds %>% filter(carat < 3) %>%
  ggplot() + geom_freqpoly(aes(carat, color = cut), binwidth = 0.1)

# ---------------------- Typical values --------------------

# which are most common? why?
# which are rare? why?
# unusual pattern? Explantion for them?
# how the observations within the same cluster similar to each other?
# how the observations in different cluster different from each other?
# describe and explain each cluster
# what appearance of clusters be misleading

# --------------------- Unusual values ---------------------------

# zoom in on plot to see unusual values in histogram: coord_cartesian()

diamonds %>%  ggplot()+
  geom_histogram(aes(y), binwidth = 0.5)+
  coord_cartesian(ylim = c(0,50))

diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)

# which x,y,z is length, width and depth?
diamonds %>% ggplot()+
  geom_histogram(aes(x), binwidth = 0.4)

diamonds %>% ggplot()+
  geom_histogram(aes(y), binwidth = 0.5)+
  coord_cartesian(xlim = c(0,20))

diamonds %>% ggplot()+
  geom_histogram(aes(z), binwidth = 0.2)+
  coord_cartesian(xlim = c(0,10))

# price distribution
diamonds %>% ggplot()+
  geom_histogram(aes(price), binwidth = 800)

# -------------------- missing values ------------------

# replacing unusual values with NA. not calclated in model
outliers <- diamonds %>% mutate(y= ifelse(y<3|y>20, NA, y))

outliers %>% ggplot()+geom_point(aes(x,y), na.rm = TRUE)

# highlight outliers in the plot
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# ----------------------- Variation : behavior WITHIN a variable -------------------

# ------------------------- covariation: behavior BETWEEN variables ----------------------------

# --------------------------- covariation:categorical and continous variable ----------------
diamonds %>% ggplot(aes(price))+geom_freqpoly(aes(color=cut), binwidth=500)
diamonds %>% ggplot(aes(price))+geom_freqpoly(aes(color=cut, y=..density..), binwidth=500)

# use boxplot to compare
diamonds %>%  ggplot(aes(cut, price))+ geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))+
  coord_flip()

# exercices 7.5.1.1
diamonds %>% ggplot()+
  geom_violin(aes(cut, price))

diamonds %>% ggplot()+
  geom_histogram(aes(price, fill=cut), binwidth = 1500)


# -------------- covariation : two categorical variables -------------------

# geom_count(): how many observations at each combination. bigger == more correlation
diamonds %>%  ggplot()+
  geom_count(aes(cut, color))

# geom_tile(): d3heatmap or heatmaply packages for bigger dataset
diamonds %>% 
  count(color, cut) %>% 
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill=n))

# -------------- covariation : two continuous variables -------------------

# to avoid overplotting when dataset grows, use alpha
diamonds %>% ggplot()+
  geom_point(aes(carat, price), alpha=1/100)

# other solution: use bins. 1D: geomhist and geomfreqpoly. 2D geombin2d and geomhex

diamonds %>% ggplot()+
  geom_bin2d(aes(carat, price))

diamonds %>% ggplot()+
  geom_hex(aes(carat, price))

# other solution : bin one continuous variable so it acts like categorical var
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# exercices 7.5.3.1
diamonds %>% ggplot()+
  geom_freqpoly(aes(carat), binwidth=0.5)

diamonds %>% ggplot()+
  geom_point(aes(carat, price), alpha=1/100)

diamonds %>% ggplot()+
  geom_histogram(aes(price, fill=cut), bins=25)

diamonds %>% ggplot()+
  geom_point(aes(carat, price, color=cut), alpha=1/100)

# --------------------- patterns and models -----------------------------

# could this pattern be due to coincidence>
# how is the relationshio implied by the pattern? how strong is it?
# what other variables may affect the relationship?
# how does the relationship change is we look at supgroups od the data?

# If two variables covary, you can use the values of one variable to make better predictions about the values of the second

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))
