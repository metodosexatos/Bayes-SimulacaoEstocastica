#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Re-Amostragem por Import�ncia                          #
# Data: 07/12/2019                                       #
#********************************************************#

# SIR::Sample Importance Resampling (Re-Amostragem por Import�ncia)

# Exemplo RU 486

# Tamanho da amostra
n <- 100000

# Amostra de teta
teta <- runif(n, 0, 1)
hist(teta)

# Verossimilhan�a para cada valor de teta (n=4 e x = 0)
ver <- dbinom(0, 4, prob = teta)
hist(ver)

# Densidade da priori para os valores de teta
priori <- dunif(teta, 0, 1)

# Fun��o de import�ncia
imp <- dunif(teta, 0, 1)

# Pesos
pesos <- (ver*priori)/imp
pesos

# Pesos normalizados
pesos.norm <- pesos/sum(pesos)
pesos.norm

# Amostra com reposi��o
teta.sir <- sample(teta, size = 1000, replace = TRUE, prob = pesos.norm)
hist(teta.sir, main = "Histograma do Andrey")

# Resultados
summary(teta.sir)
quantile(teta.sir, c(0.025,0.25,0.5,0.75,0.975))
sd(teta.sir)
mean(teta.sir)

# Par�metros

#- op��o 1:
install.packages("fitdistrplus")
library(fitdistrplus)
set.seed(123)
x <- teta.sir # teste: rbeta(95, 3, 10)
fit1 <- fitdist(x, "beta")
print(fit1) #shape 1= alfa e shape 2 = beta

qbeta(0.025, alfa, beta)
qbeta(0.025, 1, 5)

parametros <- coef(fit1)
parametros
alfa <- parametros[1]
beta <- parametros[2]

#- op��o 2:
install.packages("MASS")
library("MASS")
fit2 <- fitdistr(x, "beta", start = list(shape1 = 2, shape2 = 9))
print(fit2)

# ERU

eru <- -sum(pesos.norm*log(pesos.norm))/log(n)
eru
