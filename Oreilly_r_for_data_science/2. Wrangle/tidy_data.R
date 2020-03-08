library(tidyverse)

# clean up datset with tidyr by 1. put each dataset in a tibble, 2. put each variable in its own column

# ---------------------------- Tidy data --------------------------------------

# rate per 10,000
table1 %>% mutate(rate = cases / population * 10000)

# compute cases by year
table1 %>% count(year, wt = cases)

# visualize change overtime
table1 %>% ggplot(aes(year, cases)) +
  geom_line(aes(group = country)) +
  geom_point(aes(color = country))

# exercices


# -- Pivoting : variable spread across multiples columns or observation scattered across multiple rows --

# pivot_longer(): pivot columns into new variable
tidy4a <-
  table4a %>% pivot_longer(c('1999', '2000'), names_to = "year", values_to = "cases")

tidy4b <-
  table4b %>%  pivot_longer(c('1999', '2000'), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b)

# pivot_wider(): make observation scatter across multiples rows

table2 %>% pivot_wider(names_from = type, values_from = count)

# exercices

# ------------------- Separating and uniting -----------------------

# separate(): pull appart a column. true makes it convert to good type
table3 %>%
  separate(
    rate,
    into = c("cases", "population"),
    sep = "/",
    convert = TRUE
  )

# separate year column at the 2 position
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

# unite(): combine two columns or more into a single one
table5 %>% unite(new, century, year, sep = "")

# exercices 12.4.5 : extra(), fill(), separate(), remove()

# exercices: separate() vs extract()
table5 %>%
  separate(
    rate,
    into = c("cases", "population"),
    sep = "/",
    convert = TRUE
  )

# -------------- Missing values ------------------

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# explicit: flagged with NA
stocks %>% pivot_wider(names_from = year, values_from = return)

stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )

stocks %>%  complete(year, qtr)

# implicit: NAs not in the data
stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )

# fill(): make NAs have the same value as the one above it
treatment <- tribble(
  ~ person,
  ~ treatment,
  ~ response,
  "Derrick Whitmore",
  1,
  7,
  NA,
  2,
  10,
  NA,
  3,
  9,
  "Katherine Burke",
  1,
  4
)

treatment %>% fill(person)

# ------------------ Case study -----------------

# drop redundant columns
who1 <- who %>% select(-c(iso2, iso3))

# pivot longer
who2 <- who1 %>%  pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = "key",
  values_to = "cases",
  values_drop_na = TRUE
)

# rename unformated columns
who3 <- who2 %>%
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))

# separate columns key column 
who4 <- who3 %>% 
  separate(key, c("new","type", "sexage"), sep="_")

# separate sexage column
who5 <- who4 %>% separate(sexage, c("sex", "age"), sep = 1)
  
  
  
  
  
  
