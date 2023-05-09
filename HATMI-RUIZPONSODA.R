# setwd ...
# setwd("C:/Users/diego/OneDriveUPM/MII/CentraleSupelec/StatAvan/dm")


# install.packages("AppliedPredictiveModeling")

library(AppliedPredictiveModeling)

data(FuelEconomy)


?cars2010



head(cars2010)

attach(cars2010)



##  Models

Mod1=lm(FE~EngDispl,data=cars2010)

summary(Mod1)

Mod2=lm(FE~EngDispl + I(EngDispl^2),data=cars2010)

summary(Mod2)


## Plots

plot(EngDispl,FE, main="Engine Volume (L)/Fuel consumption (MPG)")

lines(EngDispl, Mod1$fitted, lty = 1, col = "red")

lines(EngDispl[order(EngDispl)], Mod2$fitted[order(EngDispl)], lty = 2, col = "blue")


# Residuals

par(mfrow=c(1,2)) 

plot(Mod1$fitted,Mod1$residuals,main="differents residus")
abline(h=0,lty=2)

plot(Mod2$fitted,Mod2$residuals,main="differents residus")
abline(h=0,lty=2)


# Model comparison (ANOVA)

anova(Mod1, Mod2)

## Exercise 2

# Calculate training errors 

sum(Mod1$residuals^2)/length(Mod1$residuals)

sum(Mod2$residuals^2)/length(Mod2$residuals)

## Explain code


# Leave-one-out cross-validation (Prediction Residual Sum of squares)
n = nrow(cars2010)
loo1=0
for(j in 1:n){
  #  LM omitting the j-th row 
  r1 = lm(FE ~ EngDispl, data=cars2010[-j,])
  
  
  loo1=loo1+sum((cars2010$FE[j]-predict(r1,cars2010[j,]) )^2 )
}

# Forecast package
library(forecast)

CV(Mod1)

CV(Mod2)

#Error calculation (Mod2 is still better)

sum((cars2011$FE - predict(Mod1, cars2011))^2)/nrow(cars2011)

sum((cars2011$FE - predict(Mod2, cars2011))^2)/nrow(cars2011)

## Ex 3. Modele total

Mtot=lm(FE~.,data=cars2010)

summary(Mtot)

par(mfrow=c(1,3)) 

plot(Mtot$fitted,Mtot$residuals,main="differents residus")
abline(h=0,lty=2)

library(MASS)

qqnorm(studres(Mtot),main="qqplot")
qqline(stdres(Mtot))

hist(Mtot$residuals)

library(timeDate)
hist(Mtot$residuals)

# Kurtosis!!

## Ex 4. 

stepAIC(Mtot)

BIC(Mtot)

## Ex. 5

library(leaps)

recherche=regsubsets(FE~.,int=TRUE, nbest=1, nvmax=50,method="exhaustive",data=cars2010)

par(mfrow=c(1,1)) 

plot(recherche,scale="bic")
plot(summary(recherche)$bic)



