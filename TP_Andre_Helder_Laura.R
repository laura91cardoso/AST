library(fpp2)
library(forecast)
library(trend)
library(gridExtra)
library(readxl)
library(ggplot2)

UKgas

#Permite saber se se trata de uma s�rie temporal ou n�o
class(UKgas)

#Representa��o gr�fica da s�rie temporal
autoplot(UKgas, xlab = "Anos", ylab = "Milh�es", main = "Consumo de g�s no Reino Unido entre 1960 e 1986") 


########################################################
##                  Correlograma                      ##
########################################################
ggAcf(UKgas, main="Correlograma", lag=36, xlab="Desfasamento", ylab="correla��es", main="Correlograma")

######################################################################
###############PROCESSOS ESTOC�STICOS#################################
######################################################################

########################################################
##      Teste Mann-Kendall com sazonalidade           ##
########################################################

smk.test(UKgas)

########################################################
##           TESTE DE MANN-WHITNEY-PETTITT            ##
########################################################

pettitt.test(UKgas)

########################################################################
####################### M�dias M�veis ##################################

########################################################
##                  M�dias M�veis                     ##
########################################################
########
ma(UKgas, 5)
autoplot(UKgas, series="Dados", xlab="ano", ylab="Milhoes", main="Consumo de g�s no Reino Unido entre 1960 e 1986") + 
  autolayer(ma, series="MM-5")


########
autoplot(UKgas, series="dados", xlab = "Ano", ylab =, main=)+autolayer(ma(UKgas,12,centre = TRUE), series="2*12MM")


##############################################################
##C�lculo manual dos �ndices sazonais - decomposi��o Aditiva##
##############################################################

###########CALCULO DE FORMA AUTOM�TICA###############
adit<-decompose(UKgas, type="additive")
adit
adit$trend  #componente da tend�ncia obtida por mMM
adit$seasonal #componente sazonal
adit$random #res�duos
adit$figure #12 indices sazonais
autoplot(adit)


##########CALCULO DE FORMA MANUAL###############
#obter tend�ncia por MM12*2
ma<-ma(UKgas,12)
ma
head(ma,12)
head(adit$trend,12)

#subtrair a tend�ncia aos dados x_t - ^T_t
aux1<-round(UKgas-ma,4)
aux1

#calcular a m�dia por m�s dos dados sem a tend�ncia
aux2<-colMeans(matrix(aux1, ncol=12,byrow = TRUE), na.rm=T) #na.rm=T ignora os NA que aparecem na tabela
aux2

#Modelo aditivo a soma dos indices sazonais deve ser pr�xima de zero
sum(aux2)/12
#Fazer a corre��o subtraindo a cada �ndice a m�dia sum(aux2)/12
indice_adit<-aux2-sum(aux2)/12
indice_adit
adit$figure


#####################################################################
##C�lculo manual dos �ndices sazonais - decomposi��o multiplicativa##
#####################################################################

###########CALCULO DE FORMA AUTOM�TICA###############
mult<-decompose(UKgas, type="multiplicative")
mult
mult$trend  #componente da tend�ncia obtida por mMM
mult$seasonal #componente sazonal
mult$random #res�duos
mult$figure #12 indices sazonais
autoplot(mult)

##########CALCULO DE FORMA MANUAL###############
#obter tend�ncia por MM12*2
ma<-ma(UKgas,12)
ma
head(ma,12)
head(mult$trend,12)

#dividir a tend�ncia aos dados x_t / ^T_t
aux3<-round(UKgas/ma,4)
aux3

#calcular a m�dia por m�s dos dados sem a tend�ncia
aux4<-colMeans(matrix(aux3, ncol=12,byrow = TRUE), na.rm=T) #na.rm=T ignora os NA que aparecem na tabela
aux4

#Modelo multiplicativo a soma dos indices sazonais deve ser pr�xima de um
aux5<-sum(aux4)/12
aux5
#Fazer a corre��o dividindo os �ndices sazonais pela m�dia anterior
indice_mult<-aux4/aux5
indice_mult
mult$figure


#####################################################################
##                         Decomposi�ao STL                        ##
#####################################################################
#p/ que a sazonalidade seja fixa s.window=periodic
stl_UKgas_fixed_st<-stl(UKgas, t.window = 5, s.window = "periodic", robust=TRUE)
autoplot(stl_UKgas_fixed_st)+xlab("")+ylab("")+ggtitle("Consumo de g�s no Reino Unido")
#res�duos n�o est�o perto de zero--a decomposi��o stl n�o ser� a mais adequada



#para ser vari�vel a sazonalidade, s.window tem que variar, passa a ser um numero,
#tem que ser superior ao n� de trimestres observados para n�o excluir nenhum (4), por isso usamos 5.
#Na STL, a sazonalidade apresenta varia��o por isso significa que devemos usar esta decomposi��o
stl_UKgas_changing_st<-stl(UKgas, t.window = 5, s.window = 5, robust=TRUE)
autoplot(stl_UKgas_changing_st)+xlab("")+ylab("")+ggtitle("Consumo de g�s no Reino Unido")

autoplot(UKgas, series = "Dados")+
  autolayer(trendcycle(stl_UKgas_changing_st),
            series = "Tend�ncia")+
  autolayer(seasadj(stl_UKgas_changing_st), #componente sazonal ajustada
            series="Sazonalidade ajustada")+
  ggtitle("Consumo de g�s no Reino Unido",
          subtitle = "Decomposi��o STL") +
  scale_color_manual(values=c("gray", "red", "blue"),
                     breaks=c("Dados", "Tend�ncia", "Sazonalidade ajustada"))



########################################################
##            M�todos simples de previs�o             ##
########################################################
autoplot(UKgas)+
  autolayer(meanf(UKgas, h=10), 
            series="M�dia", PI=FALSE) +  #h=10 h � a previsa� futura 
  autolayer(naive(UKgas, h=10),
            series="Naive", PI=FALSE)+
  autolayer(snaive(UKgas, h=10),
            series="Naive Sazonal", PI=FALSE)+
  autolayer(rwf(UKgas, h=10, drift = TRUE),
            series="Drift", PI=FALSE)+
  ggtitle("Previs�es para o consumo de g�s  no Reino Unido") + 
  xlab("Ano")+ ylab("Milhoes")+
  guides(colour=guide_legend(title = "Previs�es"))
  

########################################################
##                  Res�duos                          ##
########################################################
res <-residuals(snaive(UKgas))
autoplot(res) + xlab("Ano")+ ylab("")+ ggtitle("Residuos utilizando o metodo snaive")
gghistogram(res) + ggtitle("Histograma do Residuos")
ggAcf(res) + ggtitle("ACF dos Res�duos")
checkresiduals(res)



checkresiduals(meanf(UKgas))

checkresiduals(snaive(UKgas))

checkresiduals(naive(UKgas))

checkresiduals(rwf(UKgas, drift = TRUE))

########################################################
##            Intervalos de Previs�o                  ##
########################################################
autoplot(snaive(UKgas))+ggtitle("Previsoes utilizando o m�todo Naive Sazonal")


#Usar dois m�todos de previsao: naive e drift

stl1<-stl(UKgas, t.window = 13, s.window = 13, robust=TRUE)
autoplot(stl1)

fit<-stlf(UKgas, t.window = 13, s.window = "periodic", robust=TRUE, method="rwdrift")
pl<-autoplot(fit)

fit2<-stlf(UKgas, t.window = 13, s.window = "periodic", robust=TRUE, method="naive")
p2<-autoplot(fit2)

grid.arrange(pl, p2, nrow=2)


########################################################
##            Metodo de tend�ncia de Holt             ##
########################################################
gascons <- window(UKgas, start=1970)
autoplot(gascons)
fc <- holt(gascons, h=5)#alisamento exponencial com tend�ncia
summary(fc)
fc$fitted


########################################################
##            M�todo de Holt amortecido damped        ##
########################################################
gascons <- window(UKgas, start=1970)
fc3 <- holt(gascons, damped=TRUE, h=5) #sem definir phi, � estimado
summary(fc3)
autoplot(gascons) +
  autolayer(fc, series="M�todo de Holt", PI=FALSE) +
  autolayer(fc3, series="M�todo de Holt amortecido", PI=FALSE) +
  ggtitle("Previs�es") + xlab("Anos") +
  ylab("G�s consumido no reino unido") +
  guides(colour=guide_legend(title="Previs�es"))


#################################################################
##  M�todo Sazonal de Holt-Winters (Aditivo e Multiplicativo)  ##
#################################################################
gascons <- window (UKgas,start=1970)
fit1 <- hw(gascons, seasonal ="additive")
fit2 <- hw(gascons, seasonal ="multiplicative")
autoplot(gascons) +
  autolayer(fit1, series="HW previs�es aditivas", PI=FALSE)+
  autolayer(fit2, series="HW previs�es multiplicativas",PI=FALSE)+
  ggtitle("Previs�es") + xlab("Anos") +
  ylab("G�s consumido no reino unido") +
  guides(colour=guide_legend(title="Previs�es"))












