library(fpp2)
library(forecast)
library(trend)

#Permite saber se se trata de uma série temporal ou não
class(UKgas)

#Representação gráfica da série temporal
autoplot(UKgas)

UKga
autoplot(UKgas, xlab = "Anos", ylab = "Milhões", main = "Consumo de gás no Reino Unido") 
