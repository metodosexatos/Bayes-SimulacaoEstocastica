#********************************************************#
# www.metodosexatos.com                                  #
# Prof.Ms. André Santos | andre@metodosexatos.com.br     #
# Curso Análise Bayesiana: Simulação Estocástico com R   #
# Atividade                                              #
# Data: 13/12/2019                                       #
#********************************************************#

#::::::::::::::::::::: Exercício 06 :::::::::::::::::::::#

# Questão:
#- Desenvolver o método analítico:
#  a) Determinar as distribuições posteriores para as zonas A e B.
#  b) Avaliar qual zona é mais propícia à presença de fêmeas: P(teta > 0.5)

# Amostras para cada zona:
nA <- 396
nB <- 460
xA <- 195
xB <- 198

# P(A) = P(B), então os parâmetros são os mesmos para as zonas A e B:
alfa.AB.priori <- 1.5
beta.AB.priori <- 1.5

# Se a priori é Beta e a verossimilhança é Binomial, então a posterior será:
alfa.A.posterior <- alfa.AB.priori + xA
beta.A.posterior <- beta.AB.priori + nA - xA
alfa.B.posterior <- alfa.AB.priori + xB
beta.B.posterior <- beta.AB.priori + nB - xB

# Distribuições posteriores
dist.AB.posterior <- matrix(data = c(alfa.A.posterior,beta.A.posterior,alfa.B.posterior,beta.B.posterior),
                            byrow = TRUE, nrow = 2, dimnames = list(c("Zona-A:", "Zona-B:"), c("Alfa", "Beta")))
print(dist.AB.posterior)

# P(teta>0.5):
p.zona.A <- 1 - pbeta(0.5,alfa.A.posterior,beta.A.posterior)
p.zona.B <- 1 - pbeta(0.5,alfa.B.posterior,beta.B.posterior)

p.zona.AB <- matrix(data = c(p.zona.A, p.zona.B),
                    byrow = TRUE, nrow = 2, dimnames = list(c("Zona-A:", "Zona-B:"), "p(+50% Fem.)"))
print(p.zona.AB)
