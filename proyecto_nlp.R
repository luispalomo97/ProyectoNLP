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
install.packages("naivebayes")
install.packages("e1071")
install.packages("caret")



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
library(naivebayes)
library(e1071)
library(caret)
library(janitor)

  
#---------------------------------------CARGA DEL DATA SET-------------------------------------------------------

#Iniciamos la carga de los datos del fichero amazon_alexa, previamente a cargar el fichero
#Se han eliminado aquellas reviews cuyo comentario esta vacio
alexa_reviews <- read.csv2("data_set_alexa_reviews.csv", row.names = NULL)
names(alexa_reviews) <- c('valoracion','fecha','producto','comentario','feedback')

#Se va a mostar cual es la frecuencia de cada una de las valoraciones
ggplot(data = alexa_reviews, mapping = aes(x = valoracion, fill = as.factor(valoracion))) + 
  geom_bar() +
  labs(x = "valoracion", y = "Cantidad", fill = "valoracion")+ 
  ggtitle("Valoraciones hechas sobre los productos")+
  scale_fill_manual("leyenda", values = c("1" = "#D9EEC2", "2" = "#C6EC9D", "3" = "#B6EA7E",
                                         "4" = "#A4E859", "5"= "#8BF01D"))+
  theme(plot.title = element_text(hjust = 0.5))


#------------------------------------LIMPIEZA DEL DATA SET-------------------------------------------------------

#Limpieza de los comentarios, solo queremos elemenos alfa-numericos
alexa_reviews$comentario <-gsub("[^a-zA-Z0-9 ]", "", alexa_reviews$comentario)
#alexa_reviews$comentario <-gsub(pattern = "(^ +| +$)",replacement = "",x = alexa_reviews$comentario)

corpus <- Corpus(VectorSource(alexa_reviews$comentario))                                                                                                                     

#Eliminamos las letras mayusculas
corpus <-tm_map(corpus,content_transformer(tolower))

#Eliminamos los signos de puntuacion
corpus <- tm_map(corpus,removePunctuation)

#Eliminamos los numeros que hayan en los comentarios
corpus <- tm_map(corpus,removeNumbers)

#Eliminamos aquellas palabras que no aportan información
corpus <- tm_map(corpus,removeWords, c(stopwords("en"),"echo","alexa","music","sound","dot","set",
                                       "amazon","product","get","speaker","home","play",
                                       "device","still","time","just","will"))

#Eliminamos los espacios en blanco entre palabas
corpus <- tm_map(corpus,stripWhitespace)


# Para crear el clasificador inicialemente nos hemos quedado con el conjunto de palabras tokenizadas anteriormente
datafr <- data.frame("coment"= corpus$content)

#-----------------------------------------CREACION MATRIZ DE TERMINOS--------------------------------------------------

#A continuación se crea un term-document matrix para crear la nube de puntos
matrix_corpus <- TermDocumentMatrix(corpus)
m <- as.matrix(matrix_corpus)
v <- sort(rowSums(m),decreasing = TRUE)
data <- data.frame(word =names(v),freq = v)
head(data,5)


#Mostramos cuales son las palabras más frecuentes
barplot(data[1:11,]$freq, las = 2, names.arg = data[1:11,]$word,ylim = c(0,1000),
        col ="lightgreen", main ="Palabras más frecuentes de las reviews",
        ylab = "Frecuencia de palabras")




#--------------------------------------CREACION NUBE DE PALABRAS------------------------------------------------------
set.seed(1234)
wordcloud(words = data$word, freq = data$freq, min.freq = 1, max.words =140,
          random.order=FALSE, rot.per = 0.35, colors=brewer.pal(8,"Dark2"))


#Nube de palabras diferenciando entre palabras positivas y negativas con bing
a <-data %>% inner_join(get_sentiments("bing")) 
a$sentiment<-replace(a$sentiment, a$sentiment == "positive",0) 
a$sentiment<-replace(a$sentiment, a$sentiment == "negative",1) 
colour = ifelse(a$sentiment < 1,"#2CBD52","#BD2C2C")

set.seed(1234)
wordcloud(words = a$word, freq = a$freq, min.freq = 1, max.words =140,
          random.order=FALSE, rot.per = 0.35, colors=colour,ordered.colors=TRUE)


#-----------------------------------------ANALISIS DE SENTIMIENTOS---------------------------------------------------
afinn_vector <- get_sentiment(datafr$coment,method = "afinn")
summary(afinn_vector)

#El resultado que nos arroja esta prueba es que el analisis de sentimientos mediante affinn nos muestra un valor medio de
# 3 lo que nos quiere decir que de media los comentarios son mas positivos, ya que la escala de affin es de -5 a 5

bing_vector <- get_sentiment(datafr$coment,method = "bing")
summary(bing_vector)

#El resultado que nos arroja esta prueba es que el analisis de sentimientos mediante bing nos muestra un valor medio de
# 1 lo que nos quiere decir que de media los comentarios son mas positivos, ya que la escala de bing es de -1 a 1


syuzhet_vector <- get_sentiment(datafr$coment,method = "syuzhet")
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
data_sentiment<-get_nrc_sentiment(datafr$coment)
head (data_sentiment,10)
data_sentiment <- colSums(data_sentiment)

#Aqui se muestra un diagrama de barras donde se muestra cuantas palabras asociadas a cada sentimiento hay en
#los comentarios
barplot(data_sentiment,col=brewer.pal(8,"Dark2"),ylim = c(0,5000),
        ylab = "frecuencia de palabras",las=2,cex.names=.9, main = "Palabras de los comentarios asociados a sentimientos")


#Esta es la diferenciacion entre comentarios positivos y negativos
barplot(data_sentiment[c("negative","positive")],ylim=c(0,5000), col = c("#BD2C2C","#2CBD52"),
        main = "Palabras de los comentarios asociados a sentimientos positivos y negativos")


#--------------------------------Creación del clasificador bayesiano-----------------------------------------------




#Con este clasificador bayesiano buscamos encontrar si a partir del producto, la valoración, el feedback y el numero
# de letras por cada comentario ver si es capaz de generar un valoración similar a la que se ha hecho. Mediante
# aquellos comentarios de caracter positivo podamos llegar a clasificar dichas valoraciones de manera similar
#

# Realizamos una conversión a etiquetas de las valoraciones, es decir, de un valor númerico a uno categorico
firstModif_1 <- function(){
alexamodAux <- alexa_reviews
alexamodAux$valoracion <- as.character(alexamodAux$valoracion)
alexamodAux[alexamodAux$valoracion== 1 | alexamodAux== 2] <- "malo"
alexamodAux[alexamodAux== 3 | alexamodAux== 4] <- "regular"
alexamodAux[alexamodAux== 5] <- "bueno"
alexamodAux <- within(alexamodAux, valoracion <- factor(valoracion, labels = c("bueno","malo","regular")))

#Juntamos el dataframe que contiene el conjunto de palabras del corpus y el dataframe que contiene las modificaciones
#descritas anteriormente. Es en este punto cuando añadimos la columna NumeroLetras, que contiene el numero de palabras
#de cada comentario
dataset_a <- rbind(select(alexamodAux,valoracion,producto,comentario,feedback))
dataset_a <- cbind(dataset_a,datafr)
dataset_a <- transform(dataset_a, "NumeroLetras"=str_count(coment, '\\w+'))  
dataset_a <- dataset_a[dataset_a$NumeroLetras > 0,]
dataset_a$producto <-NULL
dataset_a$comentario <-NULL
#dataset_a$NumeroLetras <-NULL
#dataset_a$feedback <-NULL


return (dataset_a)
}


firstModif_2 <- function(){
  alexamod <- within(alexa_reviews, valoracion <- factor(valoracion, labels = c("uno","dos","tres","cuatro","cinco")))
  
  #Juntamos el dataframe que contiene el conjunto de palabras del corpus y el dataframe que contiene las modificaciones
  #descritas anteriormente. Es en este punto cuando añadimos la columna NumeroLetras, que contiene el numero de palabras
  #de cada comentario
  dataset_a <- rbind(select(alexamod,valoracion,producto,comentario,feedback))
  dataset_a <- cbind(dataset_a,datafr)
  dataset_a <- transform(dataset_a, "NumeroLetras"=str_count(coment, '\\w+'))  
  dataset_a <- dataset_a[dataset_a$NumeroLetras > 0,]
dataset_a$producto <-NULL
dataset_a$comentario <-NULL
#dataset_a$NumeroLetras <-NULL
#dataset_a$feedback <-NULL
  return (dataset_a)
}



#Una vez creado el dataset adecuado procedemos a crear el conjunto de test y el de train
# El conjunto de train será del 70% y el de test 30%
# Primero se probará los datos con las 5 labels
primerTest <- firstModif_2()
trainIndex=createDataPartition(primerTest$valoracion, p=0.8, list = F)
train=primerTest[trainIndex, ]
test=primerTest[-trainIndex, ]

model <- naiveBayes(valoracion~., data = train )
predicted<-predict(model,test)
tab <- table(test$valoracion, predicted, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)



#De forma similar a anteriormente realizamos la division de los conjuntos de train y test
# El conjunto de train será del 70% y el de test 30%
# Ahora los datos estarán clasificados en : malo, regular y bueno
segundoTest <- firstModif_1()
trainIndex=createDataPartition(segundoTest$valoracion, p=0.8, list = F)
train=segundoTest[trainIndex, ]
test=segundoTest[-trainIndex, ]

model <- naiveBayes(valoracion~., data = train )
predicted<-predict(model,test)
tab <- table(test$valoracion, predicted, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)



