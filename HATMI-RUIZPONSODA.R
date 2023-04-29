# setwd ...
# setwd("C:/Users/diego/OneDriveUPM/MII/CentraleSupelec/StatAvan")


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










