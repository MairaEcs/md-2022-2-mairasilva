library(stats)

df <- data.frame(TEXTURA = c(5, 4, 6, 8, 10, 9),
                 PERIMETRO = c(1, 1, 6, 6, 10, 8),
                 AREA = c(4, 1, 6, 10, 8, 7),
                 SUAVIDADE = c(2, 3, 3, 6, 8, 4))
df

# Para um K = 2 utilizar a função KMeans do pacote stats, no R, e definir
# os grupos. Imprimir os rótulos utilizando o comando $cluster.

km <- kmeans(df, 2, iter.max = 5)

km$size # número de exemplares associados a cada grupo

km$cluster # índice do grupo ao qual cada exemplar foi associado