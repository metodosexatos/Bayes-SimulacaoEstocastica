#***************************************************************#
# www.metodosexatos.com                                         #
# Prof.Ms. André Santos | andre@metodosexatos.com.br            #
# Curso Análise Bayesiana: Monte Carlo com Cadeias de Markov    #
# Cadeias de Markov                                             #
# Data: 11/12/2019                                              #
#***************************************************************#

# Exemplo - Manejo de Aves (Distribuição estacionária)

# Matriz de transição
nomes <- c("Ilha 1","Ilha 2","Ilha 3", "Ilha 4")
byrow <- TRUE
M <- matrix(data = c(0.5, 0.3, 0.2, 0.0,
                     0.2, 0.4, 0.3, 0.1,
                     0.1, 0.1, 0.6, 0.2,
                     0.1, 0.2, 0.3, 0.4), byrow = byrow, nrow = 4,
            dimnames = list(nomes, nomes))
M
# Cadeia de Markov com espaço de estado discreto
#- Sintaxe para a simulação de uma Cadeia de Markov com 70000 ciclos, partindo da ilha 2 e com as 
#  ... frequência relativas de visitas às ilhas calculadas após 1000, 10000, 30000, 50000 e 70000 ciclos.

# Vetor os valores (1, 2, 3, 4) que indicarão a posição do pássaro no decorrer de 70000 ciclos.
pos <- numeric()

# Ilha 2 como o ponto inicial
pos[1] <- 2

# Simulação do posicionamento do pássaro nos momentos subsequentes
for(i in 2:70000){
  pos[i] <- sample(1:4, size = 1, prob = M[pos[i-1],])
}

# Posição do pássaro nos 10 primeiros momentos
pos[1:10]

# Distribuições de frequência relativa de ocupação das ilhas, para os 1000, 10000, 30000, 50000 e 70000 posicionamentos do pássaro.
r <- c(1000, 10000, 30000, 50000, 70000)
for(j in r) print(table(pos[1:j])/j)

# Exclusão dos 10000 primeiros valores
# ... considerando a frequência relativa para os 60000 ciclos restantes
table(pos[10001:70000])/60000
