df <- data.frame(ME = c(6, 7, 6.2, 5.5, 4.8, 3.5, 4, 6),
                 AC = c(150, 165, 100, 150, 110, 88, 150, 120),
                 ES = c(300, 150, 380, 301, 308, 50, 100, 80),
                 SIT = c("D", "D", "D", "D", "D", "F", "F", "F"))
df


# Chamada da função rpart()

library("rpart")

modelo_ad <- rpart(SIT ~ ME + AC + ES, data = df, method = "class",
                   control = rpart.control(minsplit = 1),
                   parms = list(split = "Information"))
modelo_ad


# Visualizando a Árvore de Decisão
library("rpart.plot")

plot_modelo <- rpart.plot(modelo_ad, type = 3)


# Predição
df1 <- rbind(df, c(4, 130, 200, "?"))
teste1 <- df1[9, ]

y_estimado <- predict(modelo_ad, teste1, type = "class")
y_estimado
