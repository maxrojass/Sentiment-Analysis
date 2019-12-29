### C?digo cuenta con explicaci?n de cada parte del proceso.
### Cualquier consulta y/o acotaci?n  del c?digo enviar e-mail a maxrojas@alumnos.uai.cl

#librer?as utilizadas a instalar
install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("tm")
install.packages("SnowballC")
install.packages("caTools")
install.packages("caret")
install.packages("e1071")
install.packages("plyr")
install.packages("wordcloud")
install.packages("rjson")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("leaflet")
install.packages("gganimate")
install.packages("lubridate")
install.packages("maps")
install.packages("ggthemes")
install.packages("ggmap")
install.packages("mapproj")
#carga de librer?as utilizadas
library(twitteR)
library(ROAuth)
library(httr)
library(tm)
library(SnowballC)
library(caTools)
library(caret)
library(e1071)
library(plyr)
library(wordcloud)
library(stringi)
library(rjson)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)
library(ggmap)
library(mapproj)
##credenciales para poder obtener datos desde twitter
consumer_key = ''
consumer_secret <- ''
access_token = ''
access_secret = ''
#funcion para autorizar el acceso a data
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#funcion para sacar tweets 
newTweets = searchTwitter("lima", n=10000, lang="es" )
# vuelca la informacion de los tweets a un data frame
df <- do.call("rbind", lapply(newTweets, as.data.frame))
#guardar lista de tweets para clasificar
write.csv(df, file = "df.csv")

df[is.na(df)] <- -1
tweetsprueba <- df #cargar los datos al tweets ("nombre del archivo.csv", separador)
names(tweets)[names(tweets) == "X1"] <- "sentiment"
names(tweets)[names(tweets) == "X4"] <- "text"
names(tweets)[names(tweets) == "X2"] <- "long"
names(tweets)[names(tweets) == "X3"] <- "lat"

table(tweets$sentiment) #Cuantos tweets positivos y negativos hay
Corpus = Corpus(VectorSource(tweets$text)) #objeto corpus se le asigna el objeto de tweets // lee los tweets
length(Corpus) #cuenta las palabras tiene el corpus
content(Corpus[[20]]) #imprime la posicion 20


#Prepocesamiento
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
(corpus[[1]]$content)
Corpus <- tm_map(Corpus, PlainTextDocument) #vuelve el corpus normal para poder visualizarlo
content(Corpus[[20]]) #imprime la posicion 20
#poner todo en minusculas
Corpus <- tm_map(Corpus, content_transformer(stri_trans_tolower))
Corpus = tm_map(Corpus, content_transformer(tolower))
#remover stopwords
Corpus = tm_map(Corpus, removeWords, stopwords("spanish"))
#remover numeros
Corpus = tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, stemDocument, language = "spanish")  #smemming devolver las apalabras a su rair "devolvieron" "devolver"
content(Corpus[[20]]) #imprime la posicion 20
#codificando texto
#Corpus <- iconv(Corpus, 'UTF-8', 'ASCII')
#Corpus <-as.data.frame(Corpus)
#Corpus = Corpus(VectorSource(Corpus))
#Corpus <- tm_map(Corpus, PlainTextDocument) 

#clasificaci?n

frequencies <- DocumentTermMatrix(Corpus)  #crear matriz
frequencies #imprime atributos de frequencies
inspect(frequencies[15:20, 5:10]) #zoop de la matriz / posiciones [15:20, 5:10]
findFreqTerms(frequencies, lowfreq = 100) #frecuencias de palabras iguales o mayoes a 50
sparse <- removeSparseTerms(frequencies, 0.875) #quitar palabras que son mencionadas muy poco / deja las palabras mas usadas
sparse #imprime sparse
tweetsSparse <- as.data.frame(as.matrix(sparse)) # retornar la variable sparse como una base de datos en formato R
colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) #asinar los nombres de cada palabra al dataframe
tweetsSparse$sentiment <- tweets$sentiment #examina con la base de datos de sentimientos precargada

#modelo de clasificaci?n 

#partir la base de datos en entrenamiento y evaluaci?n 
set.seed(12)
split <- sample.split(tweetsSparse$sentiment, SplitRatio = 0.8) #decide que observaciones se van para un lado u otro
#plitRatio= 0.8 / el 805 se va a entrenamiento
trainSparse = subset(tweetsSparse, split==TRUE) #particion
TestSparse = subset(tweetsSparse, split==FALSE) #particion
table(TestSparse$sentiment)

#algoritmo de clasificaci?n
#para ver mejores parametros de gamma y cost  para evitar overfit
SVM <- tune.svm(as.factor(sentiment)~ ., data=trainSparse, gamma = 2^(-4:4), cost = 2^(-2:2))
summary(SVM) #Descrbe el modelo
#modelo predicci?n con valores para evitar overfit
SVM <- svm(as.factor(sentiment)~ ., data=trainSparse, gamma = 0.125, cost = 0.25)
predictSVM2 <- predict(SVM, newdata = TestSparse)
cfm = confusionMatrix(predictSVM2, TestSparse$sentiment) #evaluar el desempe?o de los tweets
table(predictSVM2, TestSparse$sentiment)
table(predictSVM, TestSparse$sentiment)
#Prueba de datos sacados desde twitter
prediciendo <- searchTwitter("peru", n=50, geocode='42.375,-71.1061111,15km')
df = twListToDF(newTweets)
prediciendo = df$text
Corpus2 = Corpus(VectorSource(df$text)) #objeto corpus se le asigna el objeto de tweets // lee los tweets
length(Corpus2) #cuenta las palabras tiene el corpus
content(Corpus2[[20]]) #imprime la posicion 20
Corpus2 <- tm_map(Corpus2, PlainTextDocument) #vuelve el corpus normal para poder visualizarlo
content(Corpus2[[20]]) #imprime la posicion 20
Corpus2 <- sapply(Corpus2,function(row) iconv(row, "latin1", "ASCII", sub=""))
Corpus2 = Corpus(VectorSource(Corpus2)) #objeto corpus se le asigna el objeto de tweets // lee los tweets
#poner todo en minusculas
Corpus2 = tm_map(Corpus2, content_transformer(tolower))

#volver a poner como corpus 
Corpus2 = Corpus(VectorSource(Corpus2))
#remover stopwords
Corpus2 = tm_map(Corpus2, removeWords, stopwords("spanish"))
#remover numeros
Corpus2 = tm_map(Corpus2, removeNumbers)
Corpus2 <- tm_map(Corpus2, stemDocument, language = "spanish")  #smemming devolver las apalabras a su rair "devolvieron" "devolver"
content(Corpus2[[20]]) 
frequencies2 <- DocumentTermMatrix(Corpus2)  #crear matriz
frequencies2 #imprime atributos de frequencies
inspect(frequencies2[15:20, 5:10]) #zoop de la matriz / posiciones [15:20, 5:10]
findFreqTerms(frequencies2, lowfreq = 50) #frecuencias de palabras iguales o mayoes a 50
sparse2 <- removeSparseTerms(frequencies2, 0.995) #quitar palabras que son mencionadas muy poco / deja las palabras mas usadas
sparse2 #imprime sparse
tweetsSparse2 <- as.data.frame(as.matrix(sparse2)) # retornar la variable sparse como una base de datos en formato R
colnames(tweetsSparse2) = make.names(colnames(tweetsSparse[0:50])) #asinar los nombres de cada palabra al dataframe
tweetsSparse2$sentiment[0:50] <- tweets$sentiment[0:50] #examina
predictSVM2 <- predict(SVM, newdata = tweetsSparse2)
confusionMatrix(predictSVM, tweetsSparse2$sentiment)
##### si predicci?n presenta error de test data does not match model probar este codigo para nuevos train y test
levels <- unique(c(trainSparse$sentiment, TestSparse$sentiment))
TestSparse$sentiment  <- factor(TestSparse$sentiment, levels=levels)
trainSparse$sentiment <- factor(trainSparse$sentiment, levels=levels)

#separando corpus de predicciones positivas y negativas
positivos <- SVM
positivos <- do.call("rbind", lapply(positivos, as.data.frame))
negativos <- SVM2[1199:6013,]
negativos <- do.call("rbind", lapply(negativos, as.data.frame))
### GR?fico de informaci?n
## sacando coordenadas de corpus 
geo = as.data.frame(Muestra_textualgeo)
geo[is.na(geo)] <- -1
geo <- geo #cargar los datos al tweets ("nombre del archivo.csv", separador)
names(geo)[names(geo) == "X1"] <- "sentiment"
names(geo)[names(geo) == "X4"] <- "text"
names(geo)[names(geo) == "X2"] <- "long"
names(geo)[names(geo) == "X3"] <- "lat"
#graficando info
####### FALTA LOGRAR ESCALAR LOS DATOS Y PODER DISTINGUIR ENTRE LOS TWEETS.
####### CLASIFICADOS PARA PODER GRAFICARLOS EN EL MAPA CON COLORES DISTINTOS.

#crear mapa de lima
map <- get_googlemap(location = 'lima', zoom = 11, maptype="terrain")
ggmap(map)
#crear subset con datos a graficar m?s peque?o para no saturar el mapa
split <- sample.split(tweetsSparse$sentiment, SplitRatio = 0.05)
geo = subset(Muestra_textualgeo, split==TRUE)
split2 <- sample.split(tweetsSparse$sentiment, SplitRatio = 0.15)
geo2 = subset(geo, split==TRUE)
#############MAPA REAL(saturado)###############
ggmap(map) +geom_point(data = Muestra_textualgeo, aes(x = Muestra_textualgeo$X3, y = Muestra_textualgeo$X2),
                       colour = 'green')


##########MAPA IDEAL(con menos info)############
ggmap(map) +geom_point(data = geo, aes(x = geo$X3, y = geo$X2*0.9965),
                       colour = 'green')  +
  scale_size_continuous(range = c(1, 800),
                        breaks = c(250, 500, 750, 1000))
