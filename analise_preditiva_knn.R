## Aplicando o Knn no dataset - Doença de Parkinson

library(ggplot2)

parkinsons <- read.table("parkinsons.data", header = TRUE, sep = ",", dec = ".", quote="")
View(parkinsons)



## Utilizando o classificador Knn para classificar o indivíduo phon_R01_S06

library(class)

## Usando um dataframe sem o indivíduo phon_R01_S06
df1 <- parkinsons[1:24, ] # pegando a primeira parte (antes do phon_R01_S06)
df2 <- parkinsons[31:195, ] # pegando a segunda parte (depois do phon_R01_S06)

df3 <- df1
df3[25:length(df2$name), ] <- df3[1:length(df2$name), ]
treinamento <- df3
treinamento <- treinamento[ , -18] # removendo a coluna status
treinamento <- treinamento[ , 2:23]


rotulos <- df3$status # criando o vetor de rotulos - status (one) - Parkinson's, (zero) - healthy

# objeto de teste
teste <- parkinsons[25:30, ] # indivíduo phon_R01_S06
teste <- teste[ , -18]
teste <- teste[ , 2:23]

# chamadando a funcao Knn
(y_estimado <- knn(treinamento, teste, rotulos, 23))
