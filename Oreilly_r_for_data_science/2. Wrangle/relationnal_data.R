

library(tidyverse)
library(nycflights13)
library(maps)
library(map)

# ------------------------ Keys : primary vs foreign keys ----------------------

# verify how to identidy each observation : unique value and draw relationship
planes %>% count(tailnum)
weather %>% count(year, month, day, hour, origin)

# surrogate key: add a key to dataset with mutate() and row_number() if ther is no primary key

# exerices 0
nycflights13::flights
nycflights13::airports
nycflights13::weather
nycflights13::planes
nycflights13::airlines

# exercices 1
Lahman::Batting
Lahman::BattingPost
Lahman::battingLabels

# exercices 2
babynames::babynames
babynames::applicants
babynames::births
babynames::lifetables

babynames <- babynames::babynames %>% pivot_wider(names_from = year, values_from = n)

baby_applicants <- babynames::applicants %>% as.data.frame() %>% 
  spread(sex, n_all)
baby_applicants <- babynames::applicants %>% 
  pivot_wider(names_from = sex, values_from = n_all)

babynames_lifetable <- babynames::lifetables %>% 
  rename(age="x", death_pct="qx", survivors_1M="lx", deaths="dx" )

# exercices 3
nasaweather::atmos
nasaweather::borders
nasaweather::elev
nasaweather::glaciers
nasaweather::storms

# exercices 4
fueleconomy::vehicles
fueleconomy::common

# exercices 5
ggplot2::diamonds

# ------------------ mutating joins: combine variables from 2 ds --------------------------------

# left_join(): add columns missing from a dataset by a common key. keeps all obs from x
flights2 <- flights %>% 
  select(year:day, hour, tailnum, carrier) %>% 
  left_join(airlines)

# right_join(): keeps all observations from y. 


# full_join(): keeps all observations from both ds. union


# inner_join(): returns ds with columns that have similar rows key value. intersection
flights4 <- flights %>% inner_join(planes)

# mutating join: the joined column adds another column
flights3 <- flights %>% 
  select(year:day, hour, tailnum, carrier) %>% 
  mutate(name=airlines$name[match(carrier, airlines$carrier)])

# exercices 13.4.6
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()




# ---------------------- filtering joins -----------------------

# semi_join(): keeps all rows in x that match in y. Match filtered sumary table into original rows
top_dest <- flights %>%
  count(dest, sort = TRUE) 

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% semi_join(top_dest)

# anti_join(): drops all rows in x that match in y
flights %>% anti_join(planes, by="tailnum") %>% 
  count(tailnum, sort = TRUE)

# exerice 13.5.1

flights %>% filter(is.na(tailnum))

planes %>% select(year, manufacturer, model, tailnum)

#flights100 <- flights %>% count(n=planes$tailnum[match(tailnum, planes$tailnum)] )

airports %>% count(alt, lon) %>% filter(n > 1)


# ------------------------- set operations -------------------


# intersect(x, y): return only observations in both x and y.
# union(x, y): return unique observations in x and y.
# setdiff(x, y): return observations in x, but not in y.