x <- read.csv("C:\\data\\datatab_6_28.csv")
x
summary(x)
str(x)
summary(x)
colnames(x)
# Create factor variable to use in analysis
x$group.factor <- as.factor(x$group)
x$group.factor
class(x$group.factor)
str(x)
# Use lm function to analyze the data
result <- lm(los~group.factor,data=x)
anova(result)  
in# Check assumptions using residuals
x$model.residuals <- resid(result)
x$model.residuals
x
# Fun with plots
plot(x$model.residuals)
plot(x$group.factor,x$model.residuals)
boxplot(model.residuals~group.factor,data=x,
         main="Boxplot of Residuals")
boxplot(los~group,data=x,main="Boxplot of Observed Data")
# Normal Q-Q Plot
qqnorm(x$model.residuals, main='Normal Q-Q Plot of Residuals')
qqline(x$model.residuals, main='Normal Q-Q Plot of Residuals')
# Shapiro-Wilk Normality Test of residuals
shapiro.test(x$model.residuals)

# Levene's Test for Homogeneity of Variances
library(car)
leveneTest(los~group.factor,data=x)









