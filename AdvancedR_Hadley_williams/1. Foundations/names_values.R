# https://adv-r.hadley.nz/

library(lobstr)

# ------------------- Binding Basics: the name is the reference to a value. use syntaxic name ----------------------

# each value has its own adress
x <- c(1, 2, 3)
y <- x
obj_addr(x)
obj_addr(y)

# exercices

obj_addr(mean)
obj_addr(base::mean)
obj_addr(evalq(mean))

# when we import csv files, data might have non syntaxic name (ex: "_abc"), which means we have to use different syntax to retrieve the value. Name variables must not begin with . or _, nor be reserved words like if, else, or ...



