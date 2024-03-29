#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Atividade                                              #
# Data: 12/12/2019                                       #
#********************************************************#

#::::::::::::::::::::: Exerc�cio 07 :::::::::::::::::::::#

# Quest�o:
#- Desenvolver o m�todo de Re-Amostragem por Import�ncia (SIR):
#  a) Determinar as distribui��es posteriores para as zonas A e B.
#  b) Comparar os resultados da simula��o computacional com o m�todo anal�tico.

#*********************************#
#************* SIR ***************#
#*********************************#

# Amostras para cada zona:
nA <- 396
nB <- 460
xA <- 195
xB <- 198

# P(A) = P(B), ent�o os par�metros s�o os mesmos para as zonas A e B:
alfa.AB.priori <- 1.5
beta.AB.priori <- 1.5

# Tamanho da amostra para simula��o
n <- 10^5

# Amostra de teta
set.seed(42)
teta <- rbeta(n, alfa.AB.priori, beta.AB.priori)

# Verossimilhan�a para cada valor de teta (n = 396 e x = 195)
ver.A <- dbinom(xA, nA, prob = teta)
ver.B <- dbinom(xB, nB, prob = teta)

# Densidade da priori para os valores de teta
priori <- dbeta(teta, alfa.AB.priori, beta.AB.priori)

# Fun��o de import�ncia
imp <- dbeta(teta, alfa.AB.priori, beta.AB.priori)

# Pesos
pesos.A <- (ver.A*priori)/imp
pesos.B <- (ver.B*priori)/imp

# Pesos normalizados
pesos.norm.A <- pesos.A/sum(pesos.A)
pesos.norm.B <- pesos.B/sum(pesos.B)

# Amostra com reposi��o
set.seed(123)
teta.sir.A <- sample(teta, size = 1000, replace = TRUE, prob = pesos.norm.A)
teta.sir.B <- sample(teta, size = 1000, replace = TRUE, prob = pesos.norm.B)

#- op��o 1:
#install.packages("fitdistrplus")
library("fitdistrplus")

# Ajuste
fit1.A <- fitdist(teta.sir.A, "beta")
fit1.B <- fitdist(teta.sir.B, "beta")
rbind(fit1.A$estimate,fit1.B$estimate)

param.fit1.A <- coef(fit1.A)
param.fit1.B <- coef(fit1.B)
alfa.sir1.A <- param.fit1.A[1]
beta.sir1.A <- param.fit1.A[2]
alfa.sir1.B <- param.fit1.B[1]
beta.sir1.B <- param.fit1.B[2]

#- op��o 2:
#install.packages("MASS")
library("MASS")

# Ajuste
fit2.A <- fitdistr(teta.sir.A, "beta", start = list(shape1 = alfa.sir1.A, shape2 = beta.sir1.A))
fit2.B <- fitdistr(teta.sir.B, "beta", start = list(shape1 = alfa.sir1.B, shape2 = beta.sir1.B))
rbind(fit2.A$estimate,fit2.B$estimate)

param.fit2.A <- coef(fit2.A)
param.fit2.B <- coef(fit2.B)
alfa.sir2.A <- param.fit2.A[1]
beta.sir2.A <- param.fit2.A[2]
alfa.sir2.B <- param.fit2.B[1]
beta.sir2.B <- param.fit2.B[2]

#*********************************#
#********** Anal�tico ************#
#*********************************#

# Se a priori � Beta e a verossimilhan�a � Binomial, ent�o a posterior ser�:
alfa.A.posterior <- alfa.AB.priori + xA
beta.A.posterior <- beta.AB.priori + nA - xA
alfa.B.posterior <- alfa.AB.priori + xB
beta.B.posterior <- beta.AB.priori + nB - xB

# Distribui��es posteriores
dist.AB.posterior <- matrix(data = c(alfa.A.posterior,beta.A.posterior,alfa.B.posterior,beta.B.posterior),
                            byrow = TRUE, nrow = 2, dimnames = list(c("Zona-A:", "Zona-B:"), c("Alfa", "Beta")))
print(dist.AB.posterior)

#******************************************#
#**** Tabela de Compara��o dos Modelos ****#
#******************************************#

dt <- data.frame(c(alfa.A.posterior,beta.A.posterior),c(alfa.B.posterior,beta.B.posterior),
                 param.fit1.A, param.fit1.B, param.fit2.A,param.fit2.B)
names(dt) <- c("Zona A","Zona B","Zona A", "Zona B", "Zona A", "Zona B")
row.names(dt) <- c("Alfa:", "Beta:")
dt <- round(dt, 4)

# Formata��o e Visualiza��o da tabela
#install.packages("kableExtra")
library(knitr)
library(kableExtra)

kable(dt, format = "html", caption = "Par�metros dos Modelos") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  add_header_above(c(" ","Anal�tico" = 2 , "SIR - fitdistrplus" = 2, "SIR - MASS" = 2))
