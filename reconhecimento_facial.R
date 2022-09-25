library(ggplot2)

medidas_face <- read.table("dataSets\\medidasface.data",
                        header = TRUE, sep = ",")
medidas_face

library(class)

(treinamento <- medidas_face[1:4, 1:4])

(rotulos <- medidas_face[1:4, 5])

df_teste <- data.frame(olho_esquerdo = 2,
                      olho_direito = 6,
                      nariz = 3,
                      boca = 8)

(teste <- df_teste[1:1, 1:4])

(y_estimado <- knn(treinamento, teste, rotulos, 3))