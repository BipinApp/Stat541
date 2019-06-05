# HW 9 R code for example 9.3 of table 9.5
rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\table.9.51.csv",header=TRUE)
x
str(x)
summary(x)
colnames(x)
#  fit a linear model 
model <- lm(mpg~cylinder*oil, data=x)
summary(model)
anova(model)

# ANOVA
Exp9.3.aov <- aov(mpg ~ cylinder*oil, data = x)
summary(Exp9.3.aov)
## Using L1-L5 multiple regression

model <-lm(mpg~L1+L2+L3+L4+L5, data=x)
summary(model)
anova(model)

