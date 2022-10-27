df <- data.frame(HorasEstudo = c(1, 1.5, 3.2, 4, 4.5, 5, 6, 6.5),
                Nota = c(4.2, 4.5, 4.8, 5.2, 6, 6.2, 7, 7.5))
df

##a) A equação do modelo.
modelo_linear <- lm(df$Nota ~ df$HorasEstudo, df)
modelo_linear

##b) Um gráfico formado pelos exemplares do conjunto de dados no eixo x
##e o valor estimado pelo modelo no eixo y.
plot(x, y)
abline(modelo_linear)

##c) Previsão da média da turma na prova final, considerando que a turma
##tem 2,3 horas de estudo, usando o modelo obtido na regressão linear.
nota_media <- 3.365 + 0.583 * 2.3
sprintf("Previsão da média da turma na prova final: %.1f", nota_media)