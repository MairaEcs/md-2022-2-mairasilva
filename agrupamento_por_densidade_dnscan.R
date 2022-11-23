df <- data.frame(TEXTURA = c(5, 4, 6, 8, 10, 9),
                 PERIMETRO = c(1, 1, 6, 6, 10, 8),
                 AREA = c(4, 1, 6, 10, 8, 7),
                 SUAVIDADE = c(2, 3, 3, 6, 8, 4))
df

# Para um r = 5 e minEx = 2 utilizar a função DBSCAN do pacote fpc, no
# R, e definir os grupos. Imprimir os rótulos utilizando o comando $cluster.

# install.packages('fpc')

library(fpc)

grupos <- dbscan(df, 5, 2)

grupos$cluster