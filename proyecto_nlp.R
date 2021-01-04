#-------------------------------------------PASOS PREVIOS-------------------------------------------------------
install.packages("Rcpp", dependencies = TRUE, INSTALL_opts = '--no-lock')

#instalación de paquetes
install.packages("ggplot2")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer") 
install.packages("sentimentr") 
install.packages("tidytext")
install.packages("textdata")
install.packages("syuzhet")

#Carga de paquetes
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(sentimentr)
library(tidytext)
library(textdata)
library(tidyverse)
library(syuzhet)

  
#---------------------------------------CARGA DEL DATA SET-------------------------------------------------------

#Iniciamos la carga de los datos del fichero amazon_alexa, previamente a cargar el fichero
#Se han eliminado aquellas reviews cuyo comentario esta vacio
alexa_reviews <- read.csv2("data_set_alexa_reviews.csv", row.names = NULL)
names(alexa_reviews) <- c('valoracion','fecha','producto','comentario','feedback')

#Se va a mostar cual es la frecuencia de cada una de las valoraciones
ggplot(data = alexa_reviews, mapping = aes(x = valoracion, fill = as.factor(valoracion))) + 
  geom_bar() +
  labs(x = "valoracion", y = "Cantidad", fill = "valoracion")+ 
  scale_fill_manual("leyenda", values = c("1" = "#D9EEC2", "2" = "#C6EC9D", "3" = "#B6EA7E",
                                         "4" = "#A4E859", "5"= "#8BF01D"))


#------------------------------------LIMPIEZA DEL DATA SET-------------------------------------------------------

#Limpieza de los comentarios, solo queremos elemenos alfa-numericos
alexa_reviews$comentario <-gsub("[^a-zA-Z0-9 ]", "", alexa_reviews$comentario)

corpus <- Corpus(VectorSource(alexa_reviews$comentario))                                                                                                                     

#Eliminamos las letras mayusculas
corpus <-tm_map(corpus,content_transformer(tolower))

#Eliminamos los signos de puntuacion
corpus <- tm_map(corpus,removePunctuation)

#Eliminamos los numeros que hayan en los comentarios
corpus <- tm_map(corpus,removeNumbers)

#Eliminamos los espacios en blanco entre palabas
corpus <- tm_map(corpus,stripWhitespace)

#Eliminamos aquellas palabras que no aportan información
corpus <- tm_map(corpus,removeWords, c(stopwords("en")))

#-----------------------------------------CREACION MATRIZ DE TERMINOS--------------------------------------------------

#A continuación se crea un term-document matrix para crear la nube de puntos
matrix_corpus <- TermDocumentMatrix(corpus)
m <- as.matrix(matrix_corpus)
v <- sort(rowSums(m),decreasing = TRUE)
data <- data.frame(word =names(v),freq = v)
head(data,5)


#Mostramos cuales son las palabras más frecuentes
barplot(data[1:6,]$freq, las = 2, names.arg = data[1:6,]$word,ylim = c(0,1000),
        col ="lightgreen", main ="Palabras más frecuentes de las reviews",
        ylab = "Frecuencia de palabras")




#--------------------------------------CREACION NUBE DE PALABRAS------------------------------------------------------
set.seed(1234)
wordcloud(words = data$word, freq = data$freq, min.freq = 1, max.words =140,
          random.order=FALSE, rot.per = 0.35, colors=brewer.pal(8,"Dark2"))

#Esta función se encarga de mostrar cual es la correlación que tienen con otras palabras aquellas palabras que
#tienen una frecuencia de aparicion en las reviews mayor-igual a 400
findAssocs(matrix_corpus, terms = findFreqTerms(matrix_corpus, lowfreq = 400), corlimit = 0.25)


#-----------------------------------------ANALISIS DE SENTIMIENTOS---------------------------------------------------
afinn_vector <- get_sentiment(alexa_reviews$comentario,method = "afinn")
summary(afinn_vector)

#El resultado que nos arroja esta prueba es que el analisis de sentimientos mediante affinn nos muestra un valor medio de
# 3 lo que nos quiere decir que de media los comentarios son mas positivos, ya que la escala de affin es de -5 a 5

bing_vector <- get_sentiment(alexa_reviews$comentario,method = "bing")
summary(bing_vector)

#El resultado que nos arroja esta prueba es que el analisis de sentimientos mediante bing nos muestra un valor medio de
# 1 lo que nos quiere decir que de media los comentarios son mas positivos, ya que la escala de bing es de -1 a 1


syuzhet_vector <- get_sentiment(alexa_reviews$comentario,method = "syuzhet")
summary(syuzhet_vector)

#El resultado que nos arroja esta prueba es que el analisis de sentimientos mediante syuzhet nos muestra un valor medio de
# 1 lo que nos quiere decir que de media los comentarios son mas positivos, ya que la escala de syuzhet es de -1 a 1


#Se pueden plotear las diferentes funciones para ver su frecuencia de forma gráfica
plot(
  syuzhet_vector, 
  type="h", 
  main="Puntuación de sentimientos syuzhet", 
  xlab = "Comentarios", 
  ylab= "Valor emocional"
)

plot(
  bing_vector, 
  type="h", 
  main="Puntuación de sentimientos bing", 
  xlab = "Comentarios", 
  ylab= "Valor emocional"
)


plot(
  afinn_vector, 
  type="h", 
  main="Puntuación de sentimientos afinn", 
  xlab = "Comentarios", 
  ylab= "Valor emocional"
)

#En estos plots tambien se puede observar de manera clara que los comentarios en su mayoría son positivos

#Aqui lo que se ha utilizado es la función get_nrc_sentiment, la cual devuelve una matriz de 10 columnas
# donde a cada comentario se muesta por cada una de esas columnas apiriciones de palabras asociadas a esos
#sentimientos
data_sentiment<-get_nrc_sentiment(alexa_reviews$comentario)
head (data_sentiment,10)
data_sentiment <- colSums(data_sentiment)

#Aqui se muestra un diagrama de barras donde se muestra cuantas palabras asociadas a cada sentimiento hay en
#los comentarios
barplot(data_sentiment)


#Esta es la diferenciacion entre comentarios positivos y negativos
barplot(data_sentiment[c("negative","positive")],ylim=c(0,5000), col = c("#BD2C2C","#2CBD52"))

                    