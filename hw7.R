#clear the memory in R
rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
# calling file from the c drive
x <- read.csv("c:\\data\\DATATAB_LAKES.csv")
str(x)
summary(x)
pairs(~wtrCHLO+wtrTP+wtrTN, main="Scatterplots: Winter Data ",data=x, labels=c("Cholorophyll","Total Phosphorous","Total Nitrogen"))
pairs(~smrCHLO+smrTP+smrTN, main="Scatterplots: Summer Data",data=x, labels=c("Cholorophyll","Total Phosphorous","Total Nitrogen"))
# if coefficient in negative then it would be limiting factor.
model1 <- lm(wtrCHLO~wtrTN+wtrTP, data = x)
summary(model1)
plot(model1)
plot(model2)
model2 <- lm(smrCHLO~smrTN+smrTP, data = x)
summary(model2)
anova(model1)
anova(model2)
par(mfrow= c(1,2))
qqnorm(model1$residuals,main = "Winter")
qqline(model1$residuals)
qqnorm(model2$residuals,main = "Summer")
qqline(model2$residuals)
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
