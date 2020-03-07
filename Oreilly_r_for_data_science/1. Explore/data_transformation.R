# urk: https://suzan.rbind.io/2018/01/dplyr-tutorial-1/


library(nycflights13)
library(tidyverse)

# -------------- filter() : select specifict rows-------------------------

flights %>% filter(month == 1, day == 1)

# with logical operator
flights %>% filter(month == 11 | month == 12)
flights %>% filter(month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# handling missing value
flights %>% filter(is.na(month))

# exercices

flights %>% filter(arr_delay >= 2 * 60)
flights %>% filter(dest %in% c("IAH", "HOU"))
flights %>%  filter(carrier %in% c("UA", "AA", "DA"))
flights %>%  filter(month %in% c(7, 8, 9))
flights %>% filter(arr_delay >= 2 * 60 & dep_delay == 0)
flights %>% filter(dep_delay >= 60 & (arr_delay - dep_delay) >= 30)
flights %>% filter(between(dep_time, 0, 60 * 6))
flights %>% filter(is.na(dep_time)) %>% count()

# --------------------- arrange() : change order of rows ------------------------------

flights %>% arrange(year, month, day)
flights %>% arrange(desc(dep_delay))
flights %>% arrange(dep_delay)

# sort NA values to the top
flights %>%
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)),
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))
flights %>%
  arrange(desc(rowSums(is.na(.))))

# --------------- select() : select columns ----------------

flights %>% select(year, month, day)
flights %>% select(year:day)
flights %>%  select(-(year:day))

# with regex
flights %>% filter(str_detect(tailnum, "J"))
flights %>% select(tailnum, starts_with("A"))
flights %>% select(tailnum, ends_with("A"))
flights %>% select(tailnum, contains("A"))

# rename variables
flights %>% rename(tail_num = "tailnum")

# reorder columns
flights %>% select(time_hour, air_time, everything())

# exercices
select(flights, contains("TIME"))
flights %>%  select(one_of(c(
  "year", "month", "day", "dep_delay", "arr_delay"
)))

# ------------------ mutate() : add another column ------------------

flights %>% select(year:day,
                   ends_with("delay"),
                   distance,
                   air_time) %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = distance / air_time ,
    speed = hours * 60
  )

# calculate proportion of a total : x/sum(x)
#flights %>% transmute(dep_time_pct=dep_time/sum(dep_time) )

# calculate difference from mean : y- mean(y)
#flights %>% transmute(dep_delay_residual= dep_delay-mean(dep_delay))

# modulo for division: %/% , for addition : %%
flights %>% transmute(dep_time, hour = dep_time %/% 100, minute = dep_time %%
                        100)

# logs : log(), log2(), log10():
log(1816516)

# cumsum: sum all the column
cumsum(1:10)

# ----------------- Offsets : refer to leading or lagging value -------------------------------

# compute running differences with lead(): x-lag(x)

# find when values change with lead(): x!= lag(x) 

# Excercises

flights %>% transmute(dep_time, hour= dep_time%/%60, minute= hour%%60)
flights %>% transmute(air_time, air_time2=(arr_time-dep_time))
flights %>%  transmute(dep_time, sched_dep_time, dep_delay, dep_time2=sched_dep_time+dep_delay)

# ---------------------- summarise(): collapse data frame into a single row ---------------

flights %>% summarise(delay=mean(dep_delay, na.rm = TRUE))

# combining summarise() with group_by() to create grouped summaries
flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay=mean(dep_delay, na.rm = TRUE))

# ------------ Using pipe command : coding without using intermediate values... then ...-------------------

# without pipe:
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# with pipe
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# include a non missing calue count to not draw conclusion from small sample 

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# without
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# with
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)





