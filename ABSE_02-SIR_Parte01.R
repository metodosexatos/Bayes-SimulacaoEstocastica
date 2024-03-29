#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Re-Amostragem por Import�ncia                          #
# Data: 07/12/2019                                       #
#********************************************************#

# SIR::Sample Importance Resampling (Re-Amostragem por Import�ncia)

#************************* Exemplo RU 486 **********************************#

#** Parte 1 - C�lculo da Amostra com Reposi��o **#
# Tamanho da amostra
n <- 10^5

# Amostra de teta
set.seed(42)
teta <- runif(n, 0, 1)
hist(teta)

# Verossimilhan�a para cada valor de teta (n = 4 e x = 0)
ver <- dbinom(0, 4, prob = teta)
hist(ver)

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
hist(teta.sir, main = "Re-Amostragem por Import�ncia")
