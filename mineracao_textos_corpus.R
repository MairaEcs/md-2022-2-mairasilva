## Preparação de dados não estruturados (texto)

# install.packages("tm")
library(tm)

# carregar e analisar os dados
getSources()

# ler os dados
getReaders()

# lendo os textos
textos = VCorpus(DirSource("C:/Users/Usuário/git-projetos/md-2022-2-mairasilva/textos", encoding = "UTF-8"), readerControl = list(reader = readPlain, language = "por"))

# inspecionando os dados
inspect(textos)
# inspect(textos[1:2]) # inspeciona o corpus por intervalos de documentos
# inspect(textos[2]) # inspeciona um documento específico no corpus
inspect(textos[[1]]) # inspeciona o texto propriamente dito

as.character(textos[[1]]) # inspeciona o texto por parágrafos
as.character(textos[[1]])[91] # inspeciona o texto por parágrafo específico

meta(textos[[1]]) # inspeciona o metadados do texto


## Mineração de termos

stopwords('portuguese') # stopwords pré definidas

textos = tm_map(textos, removeWords, stopwords('portuguese'))
textos = tm_map(textos, stripWhitespace)
textos = tm_map(textos, removePunctuation)
textos = tm_map(textos, removeNumbers)
textos = tm_map(textos, content_transformer(tolower))

tdm = TermDocumentMatrix(textos)
tdm

m = as.matrix(tdm)
m

v = sort(rowSums(m), decreasing = TRUE) # palavras com mais freq. até palavras com mmenos freq.
v

d = data.frame(word = names(v), freq = v)
head(d, 50)

## WordCloud

# install.packages("wordcloud")
# install.packages("wordcloud2")

library(RColorBrewer)
library(wordcloud)
library(wordcloud2)

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words = 200, random.order = T, rot.per = 0.5,
          colors = brewer.pal(8, "Dark2"))

wordcloud2(d)

# Gerando a nuvem de palavras - a segunda palavra com maior frequência não está aparecendo 
# ao usar o wordcloud2(d)

## É possível inferir algum tipo de informação da nuvem gerada? Qual?
# Sim. É possível perceber que as palavras que estão maiores, ou seja, possuem uma maior frequência,  
# se referem exatamente ao assunto que é retratado em ambos os textos - mineração de dados.