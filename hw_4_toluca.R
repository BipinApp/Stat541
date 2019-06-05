hours <- c(399,121,221,376,361,224,546,352,353,157,160,252,389,113,435,420,212,268,377,421,273,468,244,342,323)
sizelot <- c(80,30,50,90,70,60,120,80,100,50,40,70,90,20,110,100,30,50,90,110,30,90,40,80,70)

hours
sizelot

y <- c(399,121,221,376,361,224,546,352,353,157,160,252,389,113,435,420,212,268,377,421,273,468,244,342,323)
x <- c(80,30,50,90,70,60,120,80,100,50,40,70,90,20,110,100,30,50,90,110,30,90,40,80,70)
y
x

mean(x)
mean(y)


#TSS

t1 <- sum(y^2)
t1

t2 <- (sum(y)^2)/length(y)
t2

#TSS= t1-t2

tss <- t1-t2
tss

#Calculate Sxx

sxx <- sum((x-mean(x))^2)
sxx

#Calculate Sxy

sxy <-sum((x-mean(x))*(y-mean(y)))
sxy

sxy^2


#Calculate SSE

sse <- t1 - t2 - (sxy^2/sxx)
sse


# Fit regression to fit the model

model <- lm(y~x)

anova(model)


#Calculate the Beta coeeficients

b1 <- sxy/sxx
b1

b0 <- mean(y)-b1*mean(x)
b0

Check them against the fits of the model

summary(model)



