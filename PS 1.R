setwd("/Users/jpayson/Documents/Stats209")



############################################
## 1. Yule 

data <- read.table("yuledoc.dat", header=T)

attach(data)

paup <- paup - 100
outrelief <- outrelief - 100
old <- old - 100
pop <- pop - 100

model1 <- lm(paup ~ outrelief)
summary(model1)

model2 <- lm(paup ~ outrelief + old + pop)
summary(model2)


## Restricted model: paup ~ outrelief
## Unrestricted model: paup ~ outrelief + old + pop


## F = (Rss1 - Rss2 / p2 - p1) / (Rss2 / n - p2)
## Rssi = the residual sum of squares of model i
## Statistic will have an F distribution with (p2 - p1, n - p2) degrees of freedom
## Null hypothesis rejected if the F statistic is greater than the critical value of an F distribution


## A few different ways to do this:

n = length(paup)

Rss1 <- sum(residuals(model1)^2)
Rss2 <- sum(residuals(model2)^2)

p2 <- 4
p1 <- 2

F <- ((Rss1 - Rss2) / (p2 - p1)) / (Rss2 / (n - p2))
F

## OR:

df.1 <- df.residual(model1)
df.2 <- df.residual(model2)

F <- ((Rss1 - Rss2) / (df.1 - df.2)) / (Rss2/df.2)
F


## Easy way:

anova(model2, model1)


## We can reject the null hypothesis that b3 = b4 = 0




############################################
## 2. Coleman


data <- read.table("coleman320.dat", header=T)
attach(data)



## Predict verbal achievement based on mom's education and other variables

model <- lm(vach ~ ssal + whcol + ses + tverb + momed)
summary(model)


## What is going on with momed variable?!
## momed extremeley correlated with whcol and closely correlated with ses


## Regress mom's education on other variables

model1 <- lm(momed ~ ssal + whcol + ses + tverb)
summary(model1)

## tverb associated with momed?!


## Plot residuals of momed regression

plot(residuals(model1))




## Take residuals from previous regression and predict verbal achievement


model1adj <- lm(vach ~ residuals(model1))
summary(model1adj)


## Plot outcome vs adjusted predictor (residuals from mom education regression)

plot(residuals(model1), vach)




############################################
## 3. Regression Recursion

names(data)
## vach - var 1
## momed - var 2
## ses - var 3


## B12 = coef. for variable 2 predicting variable 1
lm(vach ~ momed)
B12 <- coefficients(lm(vach ~ momed))[2]


## B12*3 = coef. for variable 2 predicting variable 1 with 3 in equation 
lm(vach ~momed + ses)
B12_3 <- coefficients(lm(vach ~momed + ses))[2]


## B3*2 = coef. for variable 2 predicting variable 3
lm(ses ~ momed)
B3_2 <- coefficients(lm(ses ~ momed))[2]


## B13*2 = coef. for variable 3 predicting variable 1 with 3 in equation
lm(vach ~ momed + ses)
B13_2 <- coefficients(lm(vach ~ momed + ses))[3]

## "The coefficient of variable 2 predicting variable 1 is equal to the coefficient of variable 2 predicting 1 with 3 in the equation plus the coefficient of variable 2 predicting variable 3 multiplied by the coefficient of variable 3 predicting variable 1 with 2 in the equation"

B12
B12_3 + B3_2*B13_2





############################################
## 4. Errors in variables


install.packages("DAAG")
library(DAAG)

?errorsINx

## Reliability coefficient for predictor variable: .8
## Slope for the perfectly measured predictor: 1.5
## Compare slope for perfectly meausred predictor with fallible predictor

errorsINx(mu=10, n=100, a=20, b=1.5, SD=4, SDyerr=.5,
timesSDx=1.5, gpfactor=F, layout=NULL,
parset = simpleTheme(col ="black",
col.line ="black", lwd=1, pch=20, lty=1), print.summary=TRUE, plotit=TRUE)




