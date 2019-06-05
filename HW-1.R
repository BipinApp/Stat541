# Exercise 1
# load data into R
x <- read.csv("C:\\data\\DATATAB_5_12.csv")
x
str(x)

##split into groups

y <- split(x,x$class)
str(y)

A <- y$'a'
B <-y$'b'

#perform a t test

t.test(A$output,B$output)


## first 2 terms of the equation

v1 <- var(A$output)/length(A$output)
v1
v2 <- var(B$output)/length(B$output)
v2

top <- (v1+v2)^2
top

##Terms on the bottom + deg of freedom

denom1 <- ((var(A$output)/length(A$output))^2) / (length(A$output)-1)
denom2 <- ((var(B$output)/length(B$output))^2) / (length(B$output)-1)

#This gives the value for the degrees of freedom, round it up to next integer.

welch.satterthwaite  <- top / (denom1+denom2)
welch.satterthwaite 






# Exercise 5
# load data into R
x <- read.csv("C:\\data\\DATATAB_5_14.csv")
x
str(x)

##split into groups

y <- split(x,x$diet)
str(y)

r <- y$'reg'
n <-y$'new'

#perform a t test

t.test(n$weight,r$weight,alternative = 'greater')

## first 2 terms of the equation

v1 <- var(r$output)/length(r$output)
v1
v2 <- var(n$output)/length(n$output)
v2

top <- (v1+v2)^2
top

##Terms on the bottom + deg of freedom

denom1 <- ((var(r$output)/length(r$output))^2) / (length(r$output)-1)
denom2 <- ((var(n$output)/length(n$output))^2) / (length(n$output)-1)

#This gives the value for the degrees of freedom, round it up to next integer.

welch.satterthwaite  <- top / (denom1+denom2)
welch.satterthwaite 




