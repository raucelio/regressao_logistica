library(ISLR)

str(Default)

attach(Default)

table(default)

table (student)

simbolo <- ifelse (default == "Yes" , 3 , 1)

cores   <- ifelse (default == "Yes" , "orange", "skyblue" )

default2 <- ifelse (default == "Yes" , 1, 0 )

plot(balance, income,
     pch = simbolo, 
     col=cores    , 
     cex=0.7      ,
     xlab="Balance",
     ylab="Income")

par(mfrow=c(1,2))
boxplot(balance~default, col= c("skyblue", "orange"),
        xlab="Default",
        ylab="Balance")
boxplot(income~default, col= c("skyblue", "orange"),
        xlab="Default",
        ylab="Income")
par(mfrow=c(1,1))



## Graficos da regressão


default2 <- ifelse (default == "Yes" , 1, 0 )

par(mfrow=c(1,2))
plot (balance, default2 , 
      pch = 20, 
      cex = 0.3,
      xlab="Balance",
      ylab="Probabilidade de Default", col="orange4")

fit1 <- lm (default2 ~ balance)

abline (fit1, col="skyblue")
abline(h = c(0,1), lty = 3)

##

plot (balance, default2 , 
      pch = 20, 
      cex = 0.7,
      xlab="Balance",
      ylab="Probabilidade de Default", col="orange4")

fit2 <- glm (default2 ~ balance, family = "binomial")
points (balance,fit2$fitted.values, pch = 20 , cex = 0.3, col="skyblue")
abline(h = c(0,1), lty = 3)
par(mfrow=c(1,1))


summary(fit1)
