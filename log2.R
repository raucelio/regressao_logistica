# figura 1

x <- seq(0,50,len=40)
y <- 17 + 2*x + rnorm(40,0,8)

plot (x,y, pch=16, col="red")

fit <- lm(y ~x)
abline(fit$coefficients)

summary(fit)


# exercicio 1

#Faça um box plot para cada variável

boxplot(cars,col=c("skyblue","orange4"))

#Faça o gráfico de dispersão: distância versus velocidade.

plot(cars$speed, cars$dist, pch=20, col="skyblue")

#Calcule a correlação entre a distância e a velocidade.

cor (cars$speed, cars$dist)

#Ajuste o modelo de regressão linear onde a distância é a variável dependente
#e a velocidade é independente.

fit <- lm (cars$dist ~cars$speed)
summary(fit)


# dados tempo de estudo e aprovação

x <- c(0.0, 0.5, 0.4  ,0.8 ,0.8 ,1.0 , 1.4,  1.3, 1.4,  2.0, 2.5,  2.3,  2.5, 2.7,
       3.0, 3.5, 3.1  ,3.3 ,3.7 ,4.0 ,4.11,  4.1, 4.3,  4.2, 4.5,  4.8,  4.7, 4.9,
       4.3, 4.1, 5.0  ,5.5 ,5.2 ,5.7 , 5.3,  6.0, 6.9,  6.1, 6.8,  6.3,  6.4, 6.8,
       6.9, 7.0, 7.5  ,7.7 ,7.2 ,7.9 , 8.0,  8.9, 8.3,  8.5, 8.4,  8.6,  8.7, 8.1,
       8.6, 9.0, 9.8  ,9.1 ,9.5 ,9.7 , 9.2,  9.3, 9.4, 10.0, 10.9,10.2, 10.9,10.3,
       10.7,10.5, 10.7, 10.4)

y <-  c(rep(0,4), rep(0,3),rep(1,1), rep(0,4), rep(1,1), rep(0,3), rep(1,2), 
        rep(0,6), rep(1,5),rep(0,3), rep(1,2), rep(0,5), rep(1,4), rep(0,2), 
        rep(1,3), rep(0,3), rep(1,6),rep(0,2), rep(1,6), rep(0,1), rep(1,8))


classe <- cut(x,0:11, right = F)

# figura 2

plot(x, y, xlab ="Horas de Estudo",
     ylab = "Probabilidade de Passar",pch=16, col="red")

abline(a=0, b=0.8/10)


# figura 3 discretizando as hora de estudo

x <- seq(0,10)

y <- c(0,.25,.20,.40,.45,.40,.44,.60,.67,.75,.89)

plot(x, y, xlab ="Horas de Estudo", xlim=c(0,10),
     ylab = "Probabilidade de Passar",pch=16, col="red")
abline(lm(y~x))

# fun??o logistica


l <- function(x) 1/(1+exp(-x))

curve(l,-6,6,ylab = "",xlab = "")

abline(h=c(0,0.5,1),col="gray")
abline(v=seq(-6,6,by=2),col="gray")


exemplo_aula <- data.frame(x,y)

reg_exemplo <- glm (y~x, family=binomial,exemplo_aula)


plot(x,y,xlab="horas de estudo",ylab = "Probabilidade de passar",pch=16, col="red")


curve(predict(reg_exemplo,data.frame(x=x), type="resp"), add=T)


summary(reg_exemplo)

#Anâlise de vari?ncia

anova(reg_exemplo,test="Chisq")

#Pseudo R2

pR2(re_exemplo)


# Obtendo a acuracia
## Criando vetor com os valores preditos pelo modelo

resultados_acur <- predict(reg_exemplo,exemplo_aula,type="response")

## Transformando as probabilidade em 0 ou 1

resultados_acur <- ifelse(resultados_acur > 0.5 ,1 ,0)

## Calculando a acuracia

erro <- mean(resultados_acur != exemplo_aula$y)

print(paste("Acuracia",1-erro))

# Obtendo o ROC

library(ROCR)

## Criando vetor com os valores preditos pelo modelo

p <- predict (reg_exemplo, exemplo_aula, type = "response")

## Acrescentando o valor original das respostas

py <- prediction(p, exemplo_aula$y)

## Calculando Sensibilidade e Especificidade para a curva ROC

pyr <- performance(py,measure="tpr",x.measure="fpr")

## Plotando a Curva ROC

plot(pyr)

## valor da AUROC

auc <- performance(py, measure = "auc")
auc <- auc@y.values[[1]]
auc
