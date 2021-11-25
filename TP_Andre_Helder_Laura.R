library(fpp2)
library(forecast)
library(trend)

#Permite saber se se trata de uma série temporal ou não
class(UKgas)

#Representação gráfica da série temporal
autoplot(UKgas)

UKgas
autoplot(UKgas, xlab = "Anos", ylab = "Milhões", main = "Consumo de gás no Reino Unido entre 1960 e 1986") 

ggAcf(UKgas, main="Correlograma", lag=12)

######################################################################
###############PROCESSOS ESTOCÁSTICOS#################################

#função para aplicar no teste Mann-Kendall com sazonalidade

smk.test(UKgas)
plot(nottem) #1º testar a sazonalidade
autoplot(nottem) #tem sazonalidade
class(nottem) #para sabermos se é ts




#2º- definir as hipóteses:
# H0: Não há tendeência
# H1: Há tendência

#3º Estatística de teste : z= 2.0919
#consideramos alfa= 0.05
# valor p= .03654 < 0.05 , rejeita-se H0, logo há tendência
# Como s= 224 >0 , a tendência é crescente.


#TESTE DE MANN-WHITNEY-PETTITT - MUDANÇA NA VARIÂNCIA

pettitt.test() #função

autoplot(ts(PagesData))
pettitt.test(PagesData)
s.res<-pettitt.test(PagesData) # variável criada s. res para guardar os valores do teste
s.res 

