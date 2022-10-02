parkinsons <- read.table("dataSets/parkinsons.data", header = TRUE, sep = ",",
                        dec = ".", quote = "")
parkinsons


library(e1071)

#chamada a funcao naivebayes

modelo_nb <- naiveBayes(parkinsons[, -18], parkinsons[, 18])


#criar o exemplar teste

exemplar_teste <- parkinsons[25:30, -18]

#chamada a funcao predict

p <- predict(modelo_nb, exemplar_teste, type = "class")
p


p1 <- predict(modelo_nb, exemplar_teste, type = "raw")
p1