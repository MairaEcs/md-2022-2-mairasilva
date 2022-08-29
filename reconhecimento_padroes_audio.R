library(tuneR)
library(dygraphs)


# Abrir o arquivo de som 

strPath <- "C:\\Users\\Usuário\\Downloads\\" # diretório de trabalho

Fname <- list("audioSeq1.wav") # nome do arquivo

strFile <- paste(strPath,Fname, sep = "")

ysound <- readWave(strFile)

fs = ysound@samp.rate # frequência de amostragem em Hz
dt = 1/fs # tempo de resolução em segundos

yl <- ysound@left
yr <- ysound@right

tf <- (length(yl)-1)*dt # tempo final em segundos
t <- seq(from=0, to=tf, by=dt)

df <- data.frame(time=t, yl, yr)

# Visualizar a forma de onda dos sinais 

# ----- dygraph(df)%>% dyRangeSelector() %>%

dyAxis("x", label = "tempo(s)") %>% 
dyAxis("y", label = "amplitude")


# Visualizar os dados em tabela

# Utilizar o comando View() para visualizar os dados em uma tabela
# Dado Estruturado (Série Temporal)
# View(df)

# Os comandos head() e tail() permitem visualizar o começo e o fim de um vetor de dados

print("head")
head(df$yl)

print("tail")
tail(df$yl)

print("tamanho do vetor(número de linhas)")
length(df$yl)

# Definindo o canal yl como o sinal a ser analisado

dfSinal <- data.frame(time = df$time, sinal = df$yl)

# Visualizar o sinal

# ----- dygraph(dfSinal)%>% dyRangeSelector() %>%

dyAxis("x", label = "tempo(s)") %>%
dyAxis("y", label = "amplitude")


# Obtendo o sinal absoluto (módulo)

sinalMod <- data.frame(time = dfSinal$time, sinalAbs = abs(dfSinal$sinal))
# ----- dygraph(sinalMod)%>%dyRangeSelector()

# Aplicando a função smooth.spline() para suavisar o sinal

# Aplicar a função smooth.spine() em todo o sinal não é adequado,
# o melhor é aplicar a função em partes menores do sinal

# criar janelas de tamanho 1000
d = 1000

# tamanho do vetor de dados
length(sinalMod$sinalAbs)

# Zero padding
vetSinalAbs <- c(sinalMod$sinalAbs, 0*(1:1000))

(n = length(vetSinalAbs))

S = NULL # vai receber o sinal após o smooth

s = seq(1, n, by = d) # vetor de janelas onde será aplicado o smooth

#primeira janena
'i = 2
b = vetSinalAbs[s[i-1]:(s[i]-1)]
a = s[i-1]:(s[i]-1)
z = smooth.spline(a,b)
S = rbind(S, cbind(z$x,z$y))
plot(z$x,z$y, type = "l")'

for (i in 2:length(s)){
  b = vetSinalAbs[s[i-1]:(s[i]-1)]
  a = s[i-1]:(s[i]-1)
  z = smooth.spline(a,b)
  S = rbind(S, cbind(z$x,z$y))
}

dim(S)

length(vetSinalAbs)

# fazer a última janela
b = vetSinalAbs[s[i]:n]
a = s[i]:n
z = smooth.spline(a,b)
S = rbind(S, cbind(z$x,z$y))

dim(S)

length(vetSinalAbs)

sinalModZero <- data.frame(time = S[,1], sinalSmooth = S[,2], sinalOrig = vetSinalAbs)

# ----- dygraph(sinalModZero) %>% dyRangeSelector() # equivalente dyRangeSelector(dygraph(sinalModZero)) 

# Definindo um data frame do sinal smooth com a base de tempo em segundos

# dataframe com zero padding
dt <- sinalMod$time[2] - sinalMod$time[1]

tTail <- seq(from= sinalMod$time[length(sinalMod$time)]+dt, to=sinalMod$time[length(sinalMod$time)] + dt*1000, by=dt) #vetor de tempo

vetTime <- c(sinalMod$time, tTail)

sinalModZero <- data.frame(time = vetTime, sinal = sinalModZero$sinalSmooth)

# ----- dygraph(sinalModZero)%>%dyRangeSelector()

# Armazenar o sinal de referência
# Janelar o sinal e obter o sinal de referência (padrão)

# saber o index de dfSinal$time = 3.6 e 4.2 (sim)
idxInicio = which(dfSinal$time == 7.3) # não
idxFim = which(dfSinal$time == 7.9) # não

# sinal smooth
idxInicio1 = which(sinalModZero$time == 7.3)
idxFim1 = which(sinalModZero$time == 7.9)

simPad <- data.frame(time = dfSinal$time[idxInicio:idxFim],sOrig = dfSinal$sinal[idxInicio:idxFim],sSRef = sinalModZero$sinal[idxInicio1:idxFim1] )

dygraph(simPad)

# melhorar o ajuste
idxInicio = which(sinalModZero$time == 7.38) # 3.81
idxFim = which(sinalModZero$time == 7.82) # 4.18

simPad <- data.frame(time = sinalModZero$time[idxInicio:idxFim],sRef = sinalModZero$sinal[idxInicio:idxFim])

dygraph(simPad)

# Avaliação da similaridade entre amostras de um processo: correlação cruzada
# Uma vez definido o padrão, vamos “varrer” o sinal original e detectar onde ocorre similaridade entre o sinal original e o padrão

# Correlação

m = length(simPad$sRef)

n = length(sinalModZero$sinal)

D = 0*S[,1]

for(i in 1:(n-m+1)){
  D[i] = cor(simPad$sRef, sinalModZero$sinal[i:((i+m)-1)])
}

dfCor <- data.frame(time = sinalModZero$time, cor = D)

dygraph(dfCor)

# Matriz de confusão

# Insatll required packages
# install.packages('caret')

# Importa a biblioteca necessária
library(caret)

# Cria vetores com pontos de dados
# 0 - sim 1 - não (0.41)
expected_value <- factor(c(0,0,1,0,1,1,1,0,0,1))
predicted_value <- factor(c(0,0,1,0,1,1,0,0,0,0))

# Criando matriz de confusão
example <- confusionMatrix(data=predicted_value, reference=expected_value)

# Exibindo os resultados
example

table(expected_value, predicted_value)

# instalando os pacotes necessários
# install.packages('gmodels')
# Importa a biblioteca necessária
library(gmodels)

# Computa os cálculos da tabela cruzada
CrossTable(expected_value, predicted_value)

accuracy = (5+3) / (5+0+2+3)
accuracy

errorRate = (2+0) / (5+0+2+3)
errorRate