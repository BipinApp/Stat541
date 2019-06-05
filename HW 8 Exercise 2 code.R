rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\table.9.21.csv",header=TRUE)
x
str(x)
summary(x)
colnames(x)

#### side-by side box plots of each factor
par(mfrow = c(1, 2))  # set plotting window for one graph
boxplot(y ~ a , data=x, xlab="a",
        ylab="y", main="Boxplots of y for Factor A")
boxplot(y ~ c , data=x, xlab="c",
        ylab="y", main="Boxplots of y for Factor C")
## Fit a model with up to third order interactions.
### ANOVA
aov <-aov(y~a*c,data=x)
aov
summary(aov)
replications(y ~ a * c, data=x)
###### fit a model
model <-lm(y~a*c,data=x)
summary(model)
anova(model)
###### Construct an interaction plot using 
###### Factor C on the x-axis and Factor A as the plots in the window
interaction.plot(x$c,x$a,x$y)

