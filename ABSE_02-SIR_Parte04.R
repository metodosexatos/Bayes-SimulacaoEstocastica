#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Re-Amostragem por Import�ncia                          #
# Data: 07/12/2019                                       #
#********************************************************#

# SIR::Sample Importance Resampling (Re-Amostragem por Import�ncia)

#************************* Exemplo RU 486 **********************************#

#::::::::::::::: Parte 1 - C�lculo da Amostra com Reposi��o ::::::::::::::::#

# Tamanho da amostra
n <- 10^5

# Amostra de teta
set.seed(42)
teta <- runif(n, 0, 1)
#hist(teta)

# Verossimilhan�a para cada valor de teta (n = 4 e x = 0)
ver <- dbinom(0, 4, prob = teta)
#hist(ver)

# Densidade da priori para os valores de teta
priori <- dunif(teta, 0, 1)

# Fun��o de import�ncia
imp <- dunif(teta, 0, 1)

# Pesos
pesos <- (ver*priori)/imp

# Pesos normalizados
pesos.norm <- pesos/sum(pesos)

# Amostra com reposi��o
set.seed(123)
teta.sir <- sample(teta, size = 1000, replace = TRUE, prob = pesos.norm)
hist(teta.sir, main = "Distribui��o Posterior")
#hist(teta.sir, main = "Re-Amostragem por Import�ncia")

#::::::::::::::::::: Parte 2 - Ajuste do Modelo (Par�metros) :::::::::::::::::::#

#- op��o 1:
#install.packages("fitdistrplus")
#library("fitdistrplus")

# Ajuste
fit1 <- fitdist(teta.sir, "beta")
print(fit1)                # par�metros: shape 1 = alfa; shape 2 = beta
param.fit1 <- coef(fit1)   # objeto com os valores dos coeficientes
alfa1 <- param.fit1[1]     # Coeficientes alfa
beta1 <- param.fit1[2]     # coeficiente beta

#- op��o 2:
#install.packages("MASS")
#library("MASS")

# Ajuste
fit2 <- fitdistr(teta.sir, "beta", start = list(shape1 = alfa1, shape2 = beta1))
print(fit2)
param.fit2 <- coef(fit2)   # objeto com os valores dos coeficientes
alfa2 <- param.fit2[1]     # Coeficientes alfa
beta2 <- param.fit2[2]     # coeficiente beta

# Matrix de coeficientes
Coeficientes <- matrix(data = c(alfa1[[1]], alfa2[[1]], beta1[[1]], beta2[[1]]),
                       nrow = 2, ncol = 2, byrow = TRUE,
                       dimnames = list(c("Alfa", "Beta"), c("Fit 1", "Fit 2")))
print(Coeficientes)

#::::::::::::::::: Parte 3 - Entropia Relativa � Uniformidade :::::::::::::::::#

eru <- -sum(pesos.norm*log(pesos.norm))/log(n)
print(eru)

#:::::::::::::::::::::::::::: Parte 4 - Resultados ::::::::::::::::::::::::::::#

# Resumo estat�sticos
summary(teta.sir)
quantile(teta.sir, c(0.025, 0.25, 0.5, 0.75, 0.975))
mean(teta.sir)
sd(teta.sir)


# C�lculo das probabilidades da distribui��o posterior Beta
qbeta(0.5, alfa1, beta1)
qbeta(0.5, alfa2, beta2)
qbeta(0.5, 1, 5)

# Matriz de compara��o para mediana

percentil <- quantile(teta.sir, 0.50)
prob.fit1 <- qbeta(0.50, alfa1, beta1)
prob.fit2 <- qbeta(0.50, alfa2, beta2)
analitico <- qbeta(0.50, 1, 5)

perc <- percentil/analitico
p.f1 <- prob.fit1/analitico
p.f2 <- prob.fit2/analitico
anal <- analitico/analitico

Resultados <- matrix(data = c(percentil, prob.fit1, prob.fit2, analitico, perc, p.f1, p.f2, anal),
                     nrow = 4, ncol = 2,
                     dimnames = list(c("Percentil", "Ajuste 1", "Ajuste 2", "Anal�tico"),
                                     c("Resultados", "Aproxima��o")))
print(Resultados)




















