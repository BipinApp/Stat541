#
# Multiple Regression for Example 8.2 on Home Prices
#  using data in Table 8.2
#

# Set directory to data folder
setwd("C:\\data")
getwd()

# Read data from csv file
data <- read.csv("DATATAB_8_2.csv",header=TRUE, sep=",")
data 
str(data)
summary(data)
colnames(data)
 
par(mfrow = c(1, 1))  # set plotting window for one graph

# examine scatterplot matrix for relationships amongs pairs of variables

library(car)
scatterplotMatrix(data,spread=FALSE,smoother.args=list(lty=2),diagonal="histogram",
  main="Dependent Variable is Price")


# Part (a) Linear regression for estimating sales price

model2 <- lm(price ~ age + bed + bath + size + lot, data=data)
summary(model2)
coef(model2)
anova(model2)
confint(model2,level=0.95)

# examine residuals
par(mfrow = c(2, 2))  # set the plotting window for four graphs
plot(model2)

# predicted values
pred.values2 <- fitted(model2)
pred.values2

# residuals
resid.values2 <- residuals(model2)
resid.values2

par(mfrow = c(1, 1))

# scatter plot of residuals versus predicted values
plot(pred.values2,resid.values2)



# Part (b) Linear regression for estimating ln(sales price)

# Now analyze natural logarithm of price
data$ln_price <- log(data$price)
data$ln_price
data$price

scatterplotMatrix(~ln_price + age + bed + bath + size + lot ,data=data,
  spread=FALSE, smoother.args=list(lty=2),diagonal="histogram",
  main="Dependent Variable is ln(Price)")

model3 <- lm(ln_price ~ age + bed + bath + size + lot, data=data)
summary(model3)
anova(model3)
confint(model3,level=0.95)

# examine residuals
par(mfrow = c(2, 2))  # set the plotting window for four graphs
plot(model3)

# predicted values
pred.values3 <- fitted(model3)
pred.values3

# residuals
resid.values3 <- residuals(model3)
resid.values3

par(mfrow = c(1, 1))

# scatter plot of residuals versus predicted values
plot(pred.values3,resid.values3)










