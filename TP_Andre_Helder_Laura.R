library(fpp2)
library(forecast)
library(trend)
library(gridExtra)
library(readxl)

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

#definir as hipóteses:
# H0: Não há tendência
# H1: Há tendência

#3º Estatística de teste : z= 13.838
#consideramos alfa= 0.05
# valor p= 2.2e-16 < 0.05 , rejeita-se H0, logo há tendência
# Como s= 1328 >0 , a tendência é crescente.


#TESTE DE MANN-WHITNEY-PETTITT - MUDANÇA NA VARIÂNCIA

pettitt.test() #função

pettitt.test(UKgas)
s.res<-pettitt.test(UKgas) # variável criada s. res para guardar os valores do teste
s.res 

########################################################################
####################### Médias Móveis ##################################

ma(UKgas, 5)
ma<- ma(UKgas, 5)
autoplot(UKgas, series="Dados", xlab="ano", ylab="Milhoes", main="Consumo de gás no Reino Unido entre 1960 e 1986") + 
  autolayer(ma, series="MM-5")


########
autoplot(UKgas, series="dados", xlab = "Ano", ylab =, main=)+autolayer(ma(UKgas,12,centre = TRUE), series="2*12MM")

adit<-decompose(UKgas, type="Additive")
adit
adit$trend  #componente da tendência obtida por mMM
adit$seasonal #componente sazonal
adit$random #resíduos
adit$figure #12 indices sazonais
autoplot(adit)



#########################################################
##Cálculo manual dos índices sazonais - decomposição Aditiva
###########################################################


adit<-decompose(UKgas, type="additive")
adit
adit$trend  #componente da tendência obtida por mMM
adit$seasonal #componente sazonal
adit$random #resíduos
adit$figure #12 indices sazonais
autoplot(adit)

#obter tendência por MM12*2
ma<-ma(UKgas,12)
ma
head(ma,12)
head(adit$trend,12)

#subtrair a tendência aos dados x_t - ^T_t
aux1<-round(UKgas-ma,4)
aux1

#calcular a média por mês dos dados sem a tendência
aux2<-colMeans(matrix(aux1, ncol=12,byrow = TRUE), na.rm=T) #na.rm=T ignora os NA que aparecem na tabela
aux2

#Modelo aditivo a soma dos indices sazonais deve ser próxima de zero
sum(aux2)
#Fazer a correção subtraindo a cada índice a média sum(aux2)/12
indice_adit<-aux2-sum(aux2)/12
indice_adit
adit$figure


########################################################
##Cálculo manual dos índices sazonais - decomposição multiplicativa
#################################################

#versão automática
mult<-decompose(UKgas, type="multiplicative")
mult
mult$trend  #componente da tendência obtida por mMM
mult$seasonal #componente sazonal
mult$random #resíduos
mult$figure #12 indices sazonais
autoplot(mult)

#versão manual
#obter tendência por MM12*2
ma<-ma(UKgas,12)
ma
head(ma,12)
head(mult$trend,12)

#dividir a tendência aos dados x_t / ^T_t
aux3<-round(UKgas/ma,4)
aux3

#calcular a média por mês dos dados sem a tendência
aux4<-colMeans(matrix(aux3, ncol=12,byrow = TRUE), na.rm=T) #na.rm=T ignora os NA que aparecem na tabela
aux4

#Modelo multiplicativo a soma dos indices sazonais deve ser próxima de um
aux5<-sum(aux4)/12
aux5
#Fazer a correção dividindo os índices sazonais pela média anterior
indice_mult<-aux4/aux5
indice_mult
mult$figure

#####DEcomposiçao STL

stl_UKgas_fixed_st<-stl(UKgas, t.window = 5, s.window = "periodic", robust=TRUE)
#p/ que a sazonalidade seja fixa s.window=periodic
#resíduos não estão perto de zero--a decomposição stl não será a mais adequada

autoplot(stl_Ukgas_fixed_st)+xlab("")+ylab("")+ggtitle("Consumo de gás no Reino Unido")



#para ser variável a sazonalidade, s.window tem que variar, passa a ser um numero,
#tem que ser superior ao nº de trimestres observados para não excluir nenhum (4), por isso usamos 5.
#Na ST, a sazonalidade apresenta variação por isso significa que devemos usar esta decomposição
stl_UKgas_changing_st<-stl(UKgas, t.window = 5, s.window = 5, robust=TRUE)
autoplot(stl_UKgas_changing_st)+xlab("")+ylab("")+ggtitle("Consumo de gás no Reino Unido")

autoplot(UKgas, series = "dados")+
  autolayer(trendcycle(stl_UKgas_fixed_st),
            series = "Tendência")+
  autolayer(seasadj(stl_Ukgas_fixed_st), #componente sazonal ajustada
            series="Sazonalidade ajustada")+
  ggtitle("Consumo de gás no Reino Unido",
          subtitle = "STL decomposição fixa") +
  scale_color_manual(values=c("gray", "red", "blue"),
                     breaks=c("Dados", "Tendência", "Sazonalidade ajustada"))

#
xx<- forecast(stl_UKgas_fixed_st, method="naive")
xx1<- autoplot(xx)

yy<- forecast(stl_UKgas_changing_st, method = "naive")
yy1<- autoplot(yy)

grid.arrange(xx1,yy1, nrow=2)

#Resíduos
res<-yy$residuals
checkresiduals(res)

#Usar dois métodos de previsao: naive e drift

stl1<-stl(UKgas, t.window = 13, s.window = 13, robust=TRUE)
autoplot(stl1)

fit<-stlf(UKgas, t.window = 13, s.window = "periodic", robust=TRUE, method="rwdrift")
pl<-autoplot(fit)

fit2<-stlf(UKgas, t.window = 13, s.window = "periodic", robust=TRUE, method="naive")
p2<-autoplot(fit2)

grid.arrange(pl, p2, nrow=2)

#Previsões
autoplot(UKgas)+autolayer(meanf(UKgas, h=11), series="Média", PI=FALSE) +  #h=11 h é a previsaõ futura 
  autolayer(naive(UKgas, h=11), series="Naive", PI=FALSE)+
  autolayer(snaive(UKgas, h=11), series="Naive Sazonal", PI=FALSE)+
  ggtitle("Previsões para o consumo de gás  no Reino Unido") + 
  xlab("Ano")+ ylab("Milhoes")+
  guides(colour=guide_legend(title = "Previsões"))



#comparar com a série original
autoplot(UKgas)+
  autolayer(meanf(UKgas, h=11), series="Média", PI=FALSE) +  
  autolayer(naive(UKgas, h=11), series="Naive", PI=FALSE)+
  autolayer(snaive(UKgas, h=11), series="Naive Sazonal", PI=FALSE)+
  autolayer(rwf(UKgas, h=11), series="Drift", PI=FALSE)

#Metodo de tendência de Holt
gascons <- window(UKgas, start=1960)
autoplot(gascons)
fc <- holt(gascons, h=5)#alisamento exponencial com tend???ncia
summary(fc)
fc$fitted

#Exemplo método de Holt amortecido damped
fc3 <- holt(gascons, damped=TRUE, h=35) #sem definir phi, é estimado
summary(fc3)
autoplot(gascons) +
  autolayer(fc, series="Método de Holt", PI=FALSE) +
  autolayer(fc3, series="Método de Holt amortecido", PI=FALSE) +
  ggtitle("Previsões") + xlab("Anos") +
  ylab("Gás consumido no reino unido") +
  guides(colour=guide_legend(title="Previsões"))
