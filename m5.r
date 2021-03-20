library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Cubist)

# Cargamos el dataset
setwd('ProyectosGithub/Bioinformática/MoocBioinformatica/')
data_recetas<-read.csv('epi_r.csv')
data_recetas<-na.omit(data_recetas)#retiramos NaN values
head(data_recetas)

##En caso de querer usar un número inferior de observaciones para hacer más ágil el cálculo
#set.seed(1120)
#data_recetas<-sample_n(data_recetas, 5000)

#Implementamos el algoritmo M5
df<-select_if(data_recetas, is.numeric) # Seleccionamos variables numéricas
fitM5 <- cubist(x = df[, -1], y = df$rating)
yprime <- predict(fitM5,data_recetas)

#Realizamos cálculos y representaciones gráficas para evaluar la bondad de las predicciones y la forma en que se configura el modelo

cat('\n Raíz del error cuadrático medio:', sqrt(sum((data_recetas$rating-yprime)^2)/length(yprime)), "\n") 
## Representación gráfica de la predicha por el modelo y la dada por los usuarios
n_sample <- 30 #número de recetas a incluir
index <-sample(1:nrow(df), n_sample)
recetas <- data_recetas$title[index]
exp <- df$rating[index]
calc <- yprime[index]
data <-data.frame(index, recetas, exp, calc)
melt(data[,c('recetas','exp','calc')],id.vars = 1)%>%
  ggplot(aes(x=recetas, y=value)) +
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge", alpha=.6) + 
  coord_flip() +
  xlab("Predicción puntuación recetas basado en algoritmo M5") 
  ylab("Puntuación de las recetas") +

dotplot(fitM5, what = "splits",main='Condiciones') # Representación divisiones del modelo y sus condiciones
dotplot(fitM5, what = "coefs",main='Coeficientes') # Representación coeficientes del modelo

summary(fitM5) # Información sobre el modelo detallada