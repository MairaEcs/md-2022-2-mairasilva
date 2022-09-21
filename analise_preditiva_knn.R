## Aplicando o Knn no dataset - Doença de Parkinson

library(ggplot2)

parkinsons <- read.table("C:/Users/Usuário/git-projetos/md-2022-2-mairasilva/dataSets/parkinsons.data", header = TRUE, sep = ",")

parkinsons



## Utilizar o classificador Knn para classificar o indivíduo phon_R01_S06

library(class)

(treinamento <- parkinsons[25:30, 1:23]) 

(rotulos <- parkinsons[25:30, 18:18])

teste <- parkinsons[1:1, 2:23]

(y_estimado <- knn(treinamento, teste, rotulos, 23))
