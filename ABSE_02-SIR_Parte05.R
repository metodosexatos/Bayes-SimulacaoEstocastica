#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Re-Amostragem por Import�ncia                          #
# Data: 08/12/2019                                       #
#********************************************************#

# SIR::Sample Importance Resampling (Re-Amostragem por Import�ncia)

#************************** Exemplo RU 486 ******************************#

#::::::::::::::::::::::: Processos essenciais :::::::::::::::::::::::::::#

# Amostra de teta:
n <- 10^5
set.seed(42)
teta <- runif(n, 0, 1)

# Verossimilhan�a para cada valor de teta:
ver <- dbinom(0, 4, prob = teta)

# Densidade da priori para os valores de teta
priori <- dunif(teta, 0, 1)

# Fun��o de import�ncia
imp <- dunif(teta, 0, 1)

# Pesos normalizados
pesos <- (ver*priori)/imp
pesos.norm <- pesos/sum(pesos)

# Amostra com reposi��o
set.seed(123)
teta.sir <- sample(teta, size = 1000, replace = TRUE, prob = pesos.norm)

# Entropia Relativa � Uniformidade
eru <- -sum(pesos.norm*log(pesos.norm))/log(n)
print(eru)

# Resultados
quantile(teta.sir, c(0.025, 0.25, 0.5, 0.75, 0.975))
mean(teta.sir)
sd(teta.sir)

# Modelo te�rico (anal�tico)
set.seed(123)
beta <- rbeta(n, 1, 5)
qbeta(c(0.025, 0.25, 0.5, 0.75, 0.975), 1, 5)
mean(beta)
sd(beta)

# Histogramas
hist(teta, main = "Distribui��o Priori")
hist(ver, main = "Verossimilhan�a")
hist(teta.sir, main = "Distribui��o Posterior")
hist(beta, main = "Modelo Te�rico (Anal�tico)")

     