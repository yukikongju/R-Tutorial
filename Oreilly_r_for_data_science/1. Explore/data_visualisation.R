
# url: https://r4ds.had.co.nz/

library(tidyverse)



# ------ Do cars with big engines use more fuel than cars with small engines? ------------

mpg %>% ggplot()+geom_point(mapping=aes(x=displ, y=hwy))

# ----------------- 3.3 : Scaling : Adding a third component to scatter plot ----------------------

mpg %>%  ggplot(aes(x=displ, y=hwy, color=class))+geom_point()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

mpg %>% ggplot()+geom_point(aes(cyl, displ), color="red")

mpg %>%  ggplot(aes(x=displ, y=hwy, color=displ<5))+geom_point()

# ------------------ 3.5 : Adding more variables using Facets --------------------

mpg %>% ggplot() +
  geom_point(aes(x=displ, y=hwy)) +
  facet_wrap(~class, nrow = 2)

mpg %>% ggplot() +
  geom_point(aes(x=displ, y=hwy)) +
  facet_grid(drv~cyl)

mpg %>% ggplot() +
  geom_point(aes(x=displ, y=hwy)) +
  facet_grid(.~cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# --------------- Geom : describing the same data with different visuals  ---------------------

# scatterplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# line chart
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# grouping the data in the plot

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# ------------ 3.6 : Displaying multiple geoms ---------------------

mpg %>% ggplot(aes(displ, hwy))+ 
  geom_point()+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth()

# ------------------------ 3.6: Exercices Recreating Graphs --------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = FALSE)

mpg %>% ggplot(aes(displ, hwy, color=drv)) +
  geom_point()+
  geom_smooth(se=FALSE)

mpg %>%  ggplot(aes(displ, hwy))+
  geom_point(aes(color=drv))+
  geom_smooth(se=FALSE)

mpg %>% ggplot(aes(displ, hwy))+
  geom_point(aes(color=drv)) +
  geom_smooth(aes(linetype=drv), se=FALSE)


# ---------------------- 3.7 : Statistical transformations ------------------

diamonds %>%  ggplot()+
  geom_bar(aes(cut))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# ----------------- 3.8 : Position adjustements with bar charts -----------------------------

diamonds %>% ggplot() +
  geom_bar(aes(cut, fill=cut))

diamonds %>% ggplot() +
  geom_bar(aes(cut, fill=clarity))

diamonds %>% ggplot(aes(cut, fill=clarity))+
  geom_bar(alpha=1/4, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# fill: comparing proportion percentages
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# dodge: make variables besides each other
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# jitter: add random noise to each point to spread out value. not one point for 109 abservations
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position="jitter")

# ---------------- 3.9 : Coordinate systems ------------------------------

# coord_flip: make bar plot horizontal

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# quick_map() : set ratio for maps

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar : make bar chart as polar coordinates

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()






