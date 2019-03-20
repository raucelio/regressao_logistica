library(xlsx)
condicional <- read.xlsx("condicional.xls",1)
save(condicional,file="condicional.RData")

load("condicional.RData")


# funcao chance 

chance <- function (p) y <- p/(1-p)

# funcao logit

logit <- function (x) y <- exp(x)/(1+exp(x))

# grafico da função logit

x <- seq(-5,5,length.out = 1000 )

plot (x , logit(x), pch=20 , cex= 0.2)

# regressão logistica


summary (glm (violoucondicional ~., data= condicional))


fidelidade <- read.csv("fidelidade.csv")
save(fidelidade, file="fidelidade.RData")

summary (glm (tevecaso ~., data= fidelidade))
