x <-read.csv("c:\\data\\datatab_hw_6.csv", header = TRUE)
x
str(x)
summary(x)
### plot scatterplot with regression line
plot(WATER~STIME,data=x)
lines(sort(x$STIME),fitted(model)[order(x$STIME)],col='red', type='l')
# or use abline to plot line
abline(model)


##step1 fit SLR 

model <- lm(WATER~STIME,data=x)
summary(model)
anova(model)

qqnorm(model$residuals)
qqline(model$residuals)

shapiro.test(model$residuals)

##step2 fit SLR with Log
hist(log(x$WATER))
hist(log(x$STIME))
model <- lm(log(WATER)~STIME,data=x)
summary(model)
anova(model)

##To interpret the coeffcients of the log scale use exp() on the fitted
##coefficients

exp(model$coefficients)
qqnorm(model$residuals)
qqline(model$residuals)
shapiro.test(model$residuals)
predict(model)

