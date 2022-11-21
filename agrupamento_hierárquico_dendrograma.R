# Os resultados apresentados nos métodos de agrupamento single e ward.D2
# apresentaram diferenças entre si.

# (age), (sex), (cp), (trestbps), (chol), (fbs), (restecg)
# (thalach), (exang), (oldpeak), (slope), (ca), (thal), (num)

df <- read.table("dataSets/processed.cleveland.data", header = FALSE,
                sep = ",", dec = ".", quote = "")

# substituindo dados faltantes por 0
df[df == "?"] <- 0

# removendo coluna 14
df$V14 <- NULL

# Criar a matriz de distância (dissimilaridade)
d <- dist(df, method = "euclidean")

# Realizar a Análise Hierárquica

# Visualizar a Análise Hierárquica como Dendrograma

library(ggplot2)
library(plotly)
library(ggdendro)

my_cluster <- hclust(d, method = "single")

p <- ggdendrogram(my_cluster, rotate = FALSE, size = 2)

ggplotly(p)

# Repetindo o processo para o método ward.D2 da função hclust

my_cluster <- hclust(d, method = "ward.D2")

p <- ggdendrogram(my_cluster, rotate = FALSE, size = 2)

ggplotly(p)
