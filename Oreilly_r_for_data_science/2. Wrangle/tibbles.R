library(tidyverse)

# tibbles never changes the type of the inputs (e.g. it never converts strings to factors!)
# tibbles always return another tibble

# convert a dataset as tibble
as_tibble(iris)

# convert to tribble: column names, separated by comma
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

# printing 
nycflights13::flights %>% 
  print(n = 10, width = Inf)

# ------------------------------------- exercices

# checking if dataset is a tibble
class(diamonds)
class(as_tibble(diamonds))







