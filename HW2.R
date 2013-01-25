## Stats209 HW2
## Jan 22, 2013


setwd("/Users/jpayson/Documents/Stats209")
set.seed(100)


#################################
## 1. Neyman-Holland-Rubin formulation

## U: population units
## K: set of potential causes/ treatments
## S: treatment to which U is actually exposed
## Y(u, s) = value of the response observed if U is exposed to S

## Potential outcomes: causal effect T_tc(u) ~ N(2,1)
## Y(u, c) ~N(10, 1)

## Y(u, t) - Y(u, c) = T_tc(u)

#################################
## Create some fake data:

## Control outcomes: 
Y_u.c <- rnorm(100, mean=10, sd=1)

## T_tc(u)
T <- rnorm(100, mean=2, sd=1) 

## Treatment counterfactual
Y_u.t <- Y_u.c + T

## Estimate the causal effect:

mean(Y_u.t) - mean(Y_u.c)

## Estaimted T_tc(u) = 2.01 (not bad)


#################################
## Create an indicator variable:

G <- rbinom(100, 1, .5)


## If G=1 observe Y(u,t), if G=0 observe Y(u,c)

sample <- rep(NA, 100)

for(i in 1:100){
	sample[i] <- ifelse(G[i]==1, Y_u.t[i], Y_u.c[i])
}

## Estimate the causal effect: 

obs <- data.frame(G, sample)

mean(obs$sample[which(obs$G==1)]) - mean(obs$sample[which(obs$G==0)])

## 2.2 estimate is slightly further off but still pretty good!


#################################
## Create a non-random indicator variable

phi <- (Y_u.c - 10)
phi[phi < 0] <- 0
phi[phi > 1] <- 1

Gneq <- rbinom(100, 1, phi)

## If Gneq=1 observe Y(u,t), if Gneq=0 observe Y(u,c)

sample1 <- rep(NA, 100)

for(i in 1:100){
	sample1[i] <- ifelse(Gneq[i]==1, Y_u.t[i], Y_u.c[i])
}

## Estimate the causal effect: 

obs1 <- data.frame(Gneq, sample1)

mean(obs1$sample1[which(obs1$Gneq==1)]) - mean(obs1$sample1[which(obs1$Gneq==0)])

## 3.59 estimate is biased



#################################
## Compare bias using Gneq assignment with result for bias in class handout

FACE <- mean(obs1$sample1[which(obs1$Gneq==1)]) - mean(obs1$sample1[which(obs1$Gneq==0)])


## BIAS: E[Y_c | S = t] - E[Y_c | S = c]
## Because we know T, we pretty much know the BIAS:

BIAS <- FACE - T
mean(BIAS)


## However, we can also estimate the BIAS

tmp <- data.frame(Gneq, sample1, T)

Exp.Ycc <- mean(tmp$sample1[which(tmp$Gneq==0)])

## The counterfactual E[Y_c | S = t]  is "the average value of Y_c among all the units exposed to t"
Exp.Yct <- mean(tmp$sample1[which(tmp$Gneq==1)] - tmp$T[which(tmp$Gneq==1)])


BIAS1 <- Exp.Yct - Exp.Ycc
BIAS1

## Estimate of bias is 1.65 (not bad)




#################################
## 2. In the data below is the association between X and Y a consequent of common cause Z? Give a point estimate, corresponding scatterplot, and 95% confidence interval for the appropriate partial correlation. Does the partial correlation coefficient settle the causal question?


install.packages("ppcor")
library(ppcor)

data <- read.table("data.txt", header=T)
data$Row <- NULL
attach(data)

pcor.test(Y, X, Z)

## partial correlation point estimate: -0.1198
## Do it by hand to check: (Yup it works!)

(cor(Y,X) - cor(Y,Z)*cor(X,Z)) / sqrt((1 - cor(Y,Z)^2)*(1 - cor(X,Z)^2))


## Scatterplot of residuals (association goes away):

cor.test(residuals(lm(Y ~ Z)), residuals(lm(X ~ Z)))


plot(residuals(lm(Y ~ Z)), residuals(lm(X ~ Z)))





#################################
## 3. Spurious Correlation

## Use sobel function to look at effect of momed on vach with tverb as a mediating variable.


data <- read.table("coleman.dat")

install.packages("multilevel")
library(multilevel)

attach(data)
names(data)

?sobel

sobel(momed, tverb, vach)

