#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br     #
# Curso An�lise Bayesiana: Simula��o Estoc�stico com R   #
# Atividade                                              #
# Data: 13/12/2019                                       #
#********************************************************#

#::::::::::::::::::::: Exerc�cio 06 :::::::::::::::::::::#

# Quest�o:
#- Desenvolver o m�todo anal�tico:
#  a) Determinar as distribui��es posteriores para as zonas A e B.
#  b) Avaliar qual zona � mais prop�cia � presen�a de f�meas: P(teta > 0.5)

# Amostras para cada zona:
nA <- 396
nB <- 460
xA <- 195
xB <- 198

# P(A) = P(B), ent�o os par�metros s�o os mesmos para as zonas A e B:
alfa.AB.priori <- 1.5
beta.AB.priori <- 1.5

# Se a priori � Beta e a verossimilhan�a � Binomial, ent�o a posterior ser�:
alfa.A.posterior <- alfa.AB.priori + xA
beta.A.posterior <- beta.AB.priori + nA - xA
alfa.B.posterior <- alfa.AB.priori + xB
beta.B.posterior <- beta.AB.priori + nB - xB

# Distribui��es posteriores
dist.AB.posterior <- matrix(data = c(alfa.A.posterior,beta.A.posterior,alfa.B.posterior,beta.B.posterior),
                            byrow = TRUE, nrow = 2, dimnames = list(c("Zona-A:", "Zona-B:"), c("Alfa", "Beta")))
print(dist.AB.posterior)

# P(teta>0.5):
p.zona.A <- 1 - pbeta(0.5,alfa.A.posterior,beta.A.posterior)
p.zona.B <- 1 - pbeta(0.5,alfa.B.posterior,beta.B.posterior)

p.zona.AB <- matrix(data = c(p.zona.A, p.zona.B),
                    byrow = TRUE, nrow = 2, dimnames = list(c("Zona-A:", "Zona-B:"), "p(+50% Fem.)"))
print(p.zona.AB)
