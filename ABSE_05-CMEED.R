#***************************************************************#
# www.metodosexatos.com                                         #
# Prof.Ms. Andr� Santos | andre@metodosexatos.com.br            #
# Curso An�lise Bayesiana: Monte Carlo com Cadeias de Markov    #
# Cadeias de Markov                                             #
# Data: 11/12/2019                                              #
#***************************************************************#

# Exemplo - Manejo de Aves (Distribui��o estacion�ria)

# Matriz de transi��o
nomes <- c("Ilha 1","Ilha 2","Ilha 3", "Ilha 4")
byrow <- TRUE
M <- matrix(data = c(0.5, 0.3, 0.2, 0.0,
                     0.2, 0.4, 0.3, 0.1,
                     0.1, 0.1, 0.6, 0.2,
                     0.1, 0.2, 0.3, 0.4), byrow = byrow, nrow = 4,
            dimnames = list(nomes, nomes))
M
# Cadeia de Markov com espa�o de estado discreto
#- Sintaxe para a simula��o de uma Cadeia de Markov com 70000 ciclos, partindo da ilha 2 e com as 
#  ... frequ�ncia relativas de visitas �s ilhas calculadas ap�s 1000, 10000, 30000, 50000 e 70000 ciclos.

# Vetor os valores (1, 2, 3, 4) que indicar�o a posi��o do p�ssaro no decorrer de 70000 ciclos.
pos <- numeric()

# Ilha 2 como o ponto inicial
pos[1] <- 2

# Simula��o do posicionamento do p�ssaro nos momentos subsequentes
for(i in 2:70000){
  pos[i] <- sample(1:4, size = 1, prob = M[pos[i-1],])
}

# Posi��o do p�ssaro nos 10 primeiros momentos
pos[1:10]

# Distribui��es de frequ�ncia relativa de ocupa��o das ilhas, para os 1000, 10000, 30000, 50000 e 70000 posicionamentos do p�ssaro.
r <- c(1000, 10000, 30000, 50000, 70000)
for(j in r) print(table(pos[1:j])/j)

# Exclus�o dos 10000 primeiros valores
# ... considerando a frequ�ncia relativa para os 60000 ciclos restantes
table(pos[10001:70000])/60000
