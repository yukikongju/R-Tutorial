library(tidyverse)

# with stringr

# -------------- basics -----------------

# str_length(): nb of character in a string
str_length(c("a", "I love paatatat", "", NA))

# str_c(): combining strings
str_c("name", "LastName", sep = " ")

# str_replace_na(): show NAs values when printing 
x <- c("abs", NA)
str_c("123", x, "456")
str_c("123", str_replace_na(x), "456")

# str_sub(): subsetting a string
x <- c("Apple", "Banana", "Pear")

str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)

# str_to_lower(): lower case
str_to_lower(x)
str_to_upper(x)

# str_sort(): order strings
str_sort(x, locale = "fra")

# exercice 14.2.5

paste("abc", "def")
paste0("abc", "def")

str_wrap("abc",width = 85)

# ---------------- matching patterns -----------------









