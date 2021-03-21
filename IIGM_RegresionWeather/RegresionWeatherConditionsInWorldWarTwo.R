library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(kknn)
library(Cubist)



#Cargamos dataset y preprocesamos los datos
#setwd(' ')
Weather<-read.csv('SummaryOfWeather.csv')

Weather$Date <- as.Date(Weather$Date)
Weather <- Weather %>% select(-MAX, -MIN, -MEA, -YR, -MO, -DA,-PRCP,-MeanTemp)
Weather$PoorWeather[Weather$PoorWeather==""]<-0 #Introducimos valores 0 en columna Poor Weather
Weather$PoorWeather[Weather$PoorWeather!=""]<-1
Weather$TSHDSBRSGF[Weather$TSHDSBRSGF!=""]<-1 #Simplificamos esta columna que nos informa de acontecimientos meteorológicos poco frecuentes a 1 o 0
Weather$TSHDSBRSGF[Weather$TSHDSBRSGF==""]<-0
Weather[,3:ncol(Weather)] %>% 
  mutate_all(~na_if(., ' '))
Weather$Precip <- as.numeric(as.character(Weather$Precip))
Weather$Snowfall <- as.numeric(as.character(Weather$Snowfall))
Weather$SNF <- as.numeric(as.character(Weather$SNF))
Weather$PoorWeather <- as.numeric(as.character(Weather$PoorWeather))
Weather$TSHDSBRSGF <- as.numeric(as.character(Weather$TSHDSBRSGF))
Weather<-Weather[, which(colMeans(!is.na(Weather)) > 0.5)] #eliminamos observables con datos insuficientes
Weather <- na.omit(Weather)

head(Weather)
str(Weather)

#Representamos precipitaciones en las distintas estaciones
n_estaciones <- 10
estaciones<-sample(unique(Weather$STA), n_estaciones)
df_plot <-data.frame(Weather$Date[Weather$STA==estaciones], Weather$Precip[Weather$STA==estaciones], as.character(Weather$STA[Weather$STA==estaciones]))
colnames(df_plot) <- c("Fecha", "Precipitaciones", "Estacion")
ggplot(data=df_plot, aes(x=Fecha, y=Precipitaciones)) +
  geom_line(aes(color = Estacion, linetype = Estacion)) +
  ggtitle('Precipitaciones en distintas estaciones del mundo durante la IIGM')


#Seleccionamos las variables continuas para la regresión lineal múltiple y el algoritmo KNN
df_continuo <- Weather %>% select(-Snowfall, -SNF, -STA,- PoorWeather, -TSHDSBRSGF ) #ELIMINAMOS CATEGÓRICAS
plot(df_continuo[2:4]) #Mostramos gráficamente la relación entre las variables
cor(df_continuo[2:4])

#Buscamos una regresión lineal para la TºC máxima
fitLM <- lm(MaxTemp ~ MinTemp+Precip, data=df_continuo) #Generamos el modelo
yprime = predict(fitLM,df_continuo)
summary(fitLM)
##Representamos gráficamente la predicción
df_plot <-data.frame(df_continuo$Date, df_continuo$MaxTemp, yprime)
colnames(df_plot) <- c("Fecha", "TemMaxExp", "TemMaxTeo")
melt(df_plot[,c('Fecha','TemMaxExp','TemMaxTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.1) +
  ylab('Temperatura')+
  ggtitle('Predicción con regresión lineal')

#Aplicamos KNN a la temperatura máxima
fitKNN <- kknn(MaxTemp ~ MinTemp+ Precip, df_continuo, df_continuo)
yprime = fitKNN$fitted.values
##Representamos gráficamente la predicción
df_plot <-data.frame(df_continuo$Date, df_continuo$MaxTemp, yprime)
colnames(df_plot) <- c("Fecha", "TemMaxExp", "TemMaxTeo")
melt(df_plot[,c('Fecha','TemMaxExp','TemMaxTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.3) +
  ylab('Temperatura')+
  ggtitle('Predicción con knn')

#Aplicamos M5 a la temperatura máxima
df<-select_if(df_M5, is.numeric) # Seleccionamos variables numéricas
fitM5 <- cubist(x = df[, -3], y = df$MaxTemp)
yprime <- predict(fitM5,df)
summary(fitM5)
dotplot(fitM5, what = "splits",main='Condiciones') # Representación divisiones del modelo y sus condiciones
dotplot(fitM5, what = "coefs",main='Coeficientes') # Representación coeficientes del modelo

##Representamos gráficamente la predicción
df_plot <-data.frame(df_M5$Date, df$MaxTemp, yprime)
colnames(df_plot) <- c("Fecha", "TemMaxExp", "TemMaxTeo")
melt(df_plot[,c('Fecha','TemMaxExp','TemMaxTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.3) +
  ylab('Temperatura')+
  ggtitle('Predicción con M5')

#Aplicamos algoritmo cruzado para comprobar que el algoritmo M5 es el que mejor nos predice los datos
set.seed(123456)
k <- 5            
Weather$kfold <- sample(1:k, nrow(Weather), replace = T)
performance_lineal <- c()
performance_m5 <- c()
performance_knn <- c()
for (fold in 1:k){
  # Se crea el conjunto de entrenamiento para la iteración
  training_set <- Weather[Weather$kfold != fold,]
  testing_set <- Weather[Weather$kfold == fold,]
  #Entrenando regresión lineal
  training_set_NoCategorico <- training_set %>% select(-Snowfall, -SNF, -STA,- PoorWeather, -TSHDSBRSGF ) #ELIMINAMOS CATEGÓRICAS
  fitLM <- lm(MaxTemp ~ MinTemp+Precip, data=training_set_NoCategorico) 
  yprime <- predict(fitLM,testing_set)
  RMSE_lineal <- sqrt(sum((testing_set$MaxTemp-yprime)^2)/length(yprime))
  performance_lineal[fold] <- RMSE_lineal
  #Entrenando knn
  fitKNN <- kknn(MaxTemp ~ MinTemp+ Precip, training_set_NoCategorico, testing_set)
  yprime = fitKNN$fitted.values
  RMSE_knn <- sqrt(sum((testing_set$MaxTemp-yprime)^2)/length(yprime))
  performance_knn[fold] <- RMSE_knn 
  #Entrenando m5
  training_set_Categorico<-select_if(training_set, is.numeric) # Seleccionamos variables numéricas
  fitM5 <- cubist(x = training_set_Categorico[, -3], y = training_set_Categorico$MaxTemp)
  yprime <- predict(fitM5,testing_set)
  RMSE_M5 <- sqrt(sum((testing_set$MaxTemp-yprime)^2)/length(yprime))
  performance_m5[fold] <- RMSE_M5 
}

cat("RECM medio en test regresión lineal para 5-fcv:", mean(performance_lineal))
cat("RECM medio en test knn para 5-fcv:", mean(performance_knn))
cat("RECM medio en test m5 para 5-fcv:", mean(performance_m5))


#Predicción precipitaciones
#Buscamos una regresión lineal para las precipitaciones
fitLM <- lm(Precip ~ MinTemp+MaxTemp, data=df_continuo) 
yprime = predict(fitLM,df_continuo)
cat('Raíz del error cuadrático medio para predicción de las precipitaciones:', sqrt(sum((df_continuo$Precip-yprime)^2)/length(yprime)), "\n") #RECM->en inglés RMSE
summary(fitLM)
##Representamos gráficamente la predicción
df_plot <-data.frame(df_continuo$Date, df_continuo$Precip, yprime)
colnames(df_plot) <- c("Fecha", "PrecipExp", "PrecipTeo")
melt(df_plot[,c('Fecha','PrecipExp','PrecipTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.1) +
  ylab('Precipitaciones')+
  ggtitle('Predicción con regresión lineal')
#Aplicamos KNN a las precipitaciones
fitKNN <- kknn(Precip ~ MinTemp+ MaxTemp, df_continuo, df_continuo)
yprime = fitKNN$fitted.values
cat('Raíz del error cuadrático medio para predicción precipitaciones con algoritmo KNN::', sqrt(sum((df_continuo$Precip-yprime)^2)/length(yprime)), "\n") #RECM->en inglés RMSE
##Representamos gráficamente la predicción
df_plot <-data.frame(df_continuo$Date, df_continuo$Precip, yprime)
colnames(df_plot) <- c("Fecha", "PrecipExp", "PrecipTeo")
melt(df_plot[,c('Fecha','PrecipExp','PrecipTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.1) +
  ylab('Precpitaciones')+
  ggtitle('Predicción con knn')
#Aplicamos M5 a las precipitaciones
fitM5 <- cubist(x = df[, -2], y = df$Precip)
yprime <- predict(fitM5,df)
cat('Raíz del error cuadrático medio para predicción precipitaciones con algoritmo M5:', sqrt(sum((df$MaxTemp-yprime)^2)/length(yprime)), "\n") 
summary(fitM5)
##Representamos gráficamente la predicción
df_plot <-data.frame(df_M5$Date, df$Precip, yprime)
colnames(df_plot) <- c("Fecha", "PrecipExp", "PrecipTeo")
melt(df_plot[,c('Fecha','PrecipExp','PrecipTeo')],id.vars = 1)%>%
  ggplot(aes(x=Fecha, y=value)) +
  geom_point(aes(color = variable),stat = "identity", alpha = 0.3) +
  ylab('Precipitaciones')+
  ggtitle('Predicción con M5')