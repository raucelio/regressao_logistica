library(MASS)
library(ggplot2)
 
treino <- Pima.tr
teste  <- Pima.te
treino$type <- ifelse(treino$type=="Yes",1,0)
teste$type  <- ifelse(teste$type=="No",0,1)

# Missing values?
cat("# valores missing?",'\n')
sapply(treino, function(x) sum(is.na(x)))

# Scatterplot matrix 
pairs(subset(treino, select = -c(type)), col = as.factor(treino$type))

# Dispersão da Idade versus a Ocorrência de Diabete
ggplot(treino, aes(x = age, y = type)) +
geom_jitter(width = 0.5, height = 0.07, alpha = .2) +
geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial")) +
labs(y = expression(hat(P)(Diabetica)))

modelo1 <- glm(type ~ age + bmi, data = treino, family = binomial)
summary(modelo1)

predict(modelo1, type = "response", newdata = data.frame(bmi = c(32, 22), age = 35))

# Através da função logística
lgs_fun <- function(par, x) 
{
  1 / (1 + exp(-x %*% par))
  # x \%*\% par é  equivalente a formula b\_0 + b\_1*age + b2*bmi
}

lgs_fun(modelo1$coefficients, c(1, 35, 32))
lgs_fun(modelo1$coefficients, c(1, 35, 22))

# Usando a definicão de Logit 
lgs_fun(modelo1$coefficients, c(1, 55, 37)) /
  (1 - lgs_fun(modelo1$coefficients, c(1, 55, 37)))

# ou usando um forma mais simples, com a notações de algebra linear 
exp(c(1, 55, 37) %*% modelo1$coefficients)

# Or atravês da função definida do R 
exp(predict(modelo1, response = "link",
            newdata = data.frame(age = 55, bmi = 37))) 

mc1 <- table(treino$type[!is.na(treino$bmi)],
             predict(modelo1, type = "response") >= 0.5)
mc1 

# Verificar se o conjunto de teste esta com missing values
sapply(teste, function(x) sum(is.na(x))) 

# Fazer as predições 
teste_pred <- predict(modelo1, type = "response", newdata = teste)

# ou manualmente (através das definuções)
teste_predm <- lgs_fun(modelo1$coefficients, as.matrix(cbind(1, teste[, c("age", "bmi")])))

mc1_teste <- table(teste$type,teste_pred >= 0.5)
mc1_teste
