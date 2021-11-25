library(fpp2)
library(forecast)
library(trend)


#Inicialmente os dados estavam organizados em tabela
class(Sealevel)

#Transformação da tabela em série temporal
Sealevel<-ts((Sealevel), start = c(1997), frequency = 1) 
Sealevel

#representação gráfica
autoplot(Sealevel) #não funciona

head(Sealevel,10) #não funciona
