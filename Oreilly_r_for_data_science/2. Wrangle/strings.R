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
str_sub(x,-3,-1)
str_sub("a", 1, 5)

# str_to_lower(): lower case
str_to_lower(x)
str_to_upper(x)

# str_sort(): order strings
str_sort(x, locale = "fra")

# exercice 14.2.5

paste("abc", "def")
paste0("abc", "def")

str_wrap("abc", width = 85)

# ---------------- matching patterns : str_views()-----------------

x <- c("apple", "banana", "pear")
#  The point replaces any char. Use \\ to escape and 4 for point symbol
x %>% str_view("an")
x %>% str_view(".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.b", "der"), "a\\.c")

# str_view_all():


# ^: match the beginning of a string
x <- c("apple", "banana", "pear")
x %>%  str_view("^ap")

# $: match the end of the string
x %>% str_view("ear$")

# exercices 14.3.2.1

str_view("$^$", "\\$\\^\\$")

stringr::words %>% str_view("^y")
stringr::words %>% str_view("x$")
stringr::words %>% str_view("^...$")
stringr::words %>% str_view("^.......")

# -------- Character classes and alternatives ---------------

# \. : matches any char
# \d : matches any digits
# \s : matches any space, tab , newline
# [abc] : matches any a,b,c
# [^abc] : matches anything except a,b,c

str_view(c("grey", "gray"), "gr(e|a)y")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

# exercices 14.3.3.1

# only begin with vowel
stringr::words %>% str_view("^[aeiou]")

# only contains consonant
stringr::words %>% str_view("[^aeiou]")

# ends with ed , but not eed (failed)
str_view(c("ed", "eed"), "[^e]ed$")

# end with ing or ise
stringr::words %>% str_view("(ing|ise)$")

# i before e except after c

# q followed by a u (failed)
# stringr::words %>% filter(str_view("qu"))

# telephones matchs (failed)
str_view(c("514-855"), "514\\-")

# -------------- Repetitions: how many times a pattern matches ------------------

# ?: 0 or 1 
# +: 1 or more
# *: 0 or more
# {n} : exactly n times
#  {n,} : n or more
#  {, m} : at most m
#  {n,m} : between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

x %>% str_view("CC?")
x %>%  str_view("CC+")
x %>%  str_view("CC+L[X]+")
x %>% str_view("C{2}")

# exercices 14.3.4.1




