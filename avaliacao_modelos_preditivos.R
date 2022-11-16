# O modelo escolhido foi o Knn devido o problema ser de classificação e ser um
# algoritmo fácil de se compreender. O objetivo da classificação aqui é prever
# se o cliente irá subscrever (sim/não) um depósito a prazo (variável y).

# bank-full - treinamento
# bank - teste (10% do bank-full)

df <- read.csv2("bank/bank-full.csv", sep = ";", stringsAsFactors = FALSE)
df1 <- read.csv2("bank/bank.csv", sep = ";", stringsAsFactors = FALSE)

# head(df) # nolint
# summary(df) # nolint
# colSums(is.na.data.frame(df)) # nolint
# prop.table(table(df$y)) # nolint

# excluindo colunas que não vão ser usadas
df_treinamento <- subset(df, select = -c(job, marital, education,
                                        contact, month, poutcome))

df_teste <- subset(df1, select = -c(job, marital, education,
                                    contact, month, poutcome))

# substituindo "no"/"yes" para 0 e 1
df_treinamento$default[df_treinamento$default == "no"] <- 0
df_treinamento$default[df_treinamento$default == "yes"] <- 1

df_treinamento$housing[df_treinamento$housing == "no"] <- 0
df_treinamento$housing[df_treinamento$housing == "yes"] <- 1

df_treinamento$loan[df_treinamento$loan == "no"] <- 0
df_treinamento$loan[df_treinamento$loan == "yes"] <- 1

df_teste$default[df_teste$default == "no"] <- 0
df_teste$default[df_teste$default == "yes"] <- 1

df_teste$housing[df_teste$housing == "no"] <- 0
df_teste$housing[df_teste$housing == "yes"] <- 1

df_teste$loan[df_teste$loan == "no"] <- 0
df_teste$loan[df_teste$loan == "yes"] <- 1

# install.packages("class") # nolint
library(class)

treinamento <- df_treinamento[, 1:10]

rotulos <- df_treinamento[, 11]

teste <- df_teste[, 1:10]

# predict
y_estimado <- knn(treinamento, teste, rotulos, 3)
#y_estimado

valor_real <- df_teste[, 11]

# matriz de confusão
table(valor_real, y_estimado)

accuracy <- sum(valor_real == y_estimado) / length(y_estimado)
accuracy

precision <- 3915 / (3915 + 257)
precision

f1_score <- (2 * precision * recall) / (precision + recall)
f1_score
