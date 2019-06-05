rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\DATATAB_9_30.csv",header=TRUE)
x
str(x)
summary(x)
colnames(x)
#### side-by side box plots of each factor
par(mfrow = c(1, 2))  # set plotting window for one graph
boxplot(bioval ~ grain , data=x, xlab="grain",
        ylab="Biological Value", main="Boxplots of Biological value for grain")
boxplot(bioval ~ prep , data=x, xlab="prep",
        ylab="Biological Value", main="Boxplots of Biological value for prep")
## Fit a model with up to third order interactions.
### ANOVA
aov <-aov(bioval~grain*prep,data=x)
aov
summary(aov)
###### fit a model
model <-lm(bioval~grain*prep,data=x)
summary(model)
anova(model)
###### Construct an interaction plot using 
###### Factor C on the x-axis and Factor A as the plots in the window
interaction.plot(x$grain,x$prep,x$bioval,type="l")
##reroder making trt #10 the control first
x$trt <- as.factor(x$trt)
x$trt <- relevel(x$trt, "10")
summary(lm(bioval~trt,data=x))

