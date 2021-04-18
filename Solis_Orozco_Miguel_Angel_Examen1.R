# Miguel Angel Solis Orozco
#Series de tiempo - Examen 1

######################
#1. Graficar la serie.
######################

library(quantmod)
library(forecast)
library(tseries)
library(tsoutliers)

#ds <- read.csv("/Users/MikeSolis/Google Drive/Maestría en ciencia de datos/2do semestre/Series de tiempo/Examen 1/examen_iteso_21a.csv")
ds <- read.csv("C:/Users/Miguel/Google Drive/Maestr�a en ciencia de datos/2do semestre/Series de tiempo/Examen 1/examen_iteso_21a.csv")
ts <- ds$V5
ts.plot(ts, type = "l", xlab = "Time", ylab = "Vector 5")
#Ya podemos ir descartando que sea ruido blanco.


####################################################
#2. Determinar el orden de integraci?n de la serie.
####################################################

adf.test(ts)
kpss.test(ts)
pacf(ts)
acf(ts)

#Ya hay incongruencia entre Dickey-Fuller y KPSS pues en kpps el resultado es
#estacionaria y en kpss es no estacionaria. Sin embargo al correr nuestra
#función de auto correlación y ver la gr?fica nos encontramos con que hay
#ra?z unitaria, por lo tanto confirmamos que no es estacionaria.
#Correremos la integral de orden 1.


##########################################################################
#3- Obtener la Función de Autocorrelación Parcial, así como la Función
#de Autocorrelación.
#        a. Comentar las implicaciones de sus resultados
#           (rezagos significativos)
##########################################################################

tsd1 <- diff(ts)
adf.test(tsd1)
kpss.test(tsd1)
pacf(tsd1)
acf(tsd1)

#La prueba KPSS nos sigue dando que no es estacionaria, aunque ya se
#acerca mucho al l?mite con el p-value. Los resagos significativos
#bajaron bastante pero hay 5 que podemos considerar de esta naturaleza.
#Intentar? con otra diferenciaci?n.

tsd2 <- diff(ts, differences = 2) 
adf.test(tsd2)
kpss.test(tsd2)
pacf(tsd2)
acf(tsd2)

#Con la segunda difernciaci?n ya ambas pruebas nos demuestran
#que hay estacionariedad.


##############################################################
#4- Establecer la lista de posibles candidatos a modelo ARIMA.
##############################################################

#Arima(5, 2, 22)
#Arima(5, 2, 6)
#Arima(5, 2, 1)
#Arima(1, 2, 1)


##############################################################################
#5- Analizar auto.arima. Por favor discuta sus diferencias con los candidatos
#del modelo 4.
##############################################################################

opcaa <- auto.arima(ts)
#ARIMA(2, 1, 0)
#Significativos:
#ar1, ar2

#log likelihood = 24.04
#aic = 42.08

#Lo primero que salta a la vista es que el auto Arima nos da un orden
#de integraci?n 1, lo cual difiere totalmente con los modelos seleccionados,
#que todos son de orden 2.
#Esto me hace dudar si fu? buena idea en un principio haber generado una
#segunda diferenciaci?n.


#####################################################################
#6- Estimar los modelos para cada uno de los candidatos. Eliminar por
#significancia (determine el nivel de significancia).
#####################################################################

#Tomar? el 5% como indicador para definir si alg?n resago es significativo.
opc1 <- Arima(ts, c(5, 2, 22), method = "ML")
opc1
#Significativos
#ar1, ar3, ar5
#ma1, ma3, ma5, ma6, ma22  

#log likelihood = 35.37
#Akaike = -14.75

#Creo que podría intentar con un arima(5, 2, 6) ya que el último valor
#significativo en ma es el ma22 y el penúltimo es el ma6
opc2 <- Arima(ts, c(5, 2, 6), method = "ML")
opc2
#Significativos
#ar1, ar2, ar4, ar5
#ma4

#log likelihood = 26.81
#Akaike = -29.62

#Tanto el log likelihood es más confiable que opc1 como el aic es más
#preciso que que opc1

opc3 <- Arima(ts, c(5, 2, 1), method = "ML")
opc3
#significativos
#ar1, ar2
#ma1

#log likelihood = 22.2
#Akaike = -30.4

#En opc3 el log likelihood y el Akaike son menores que en opc 2 sin embargo
#la diferencia no es mucha. En su contra, tuve que quitar varios
#ma significativos.

opc4 <- Arima(ts, c(1, 2, 1), method = "ML")
opc4
#significativos
#ar1
#ma1

#log likelihood = 12.43
#Akaike = -18.87

#En este caso he reducido los ar debido a que los 5 primeros se encontraban
#muy a la orilla, sin embargo los resultados, a pesar de que nos favorece
#en log likelihood, afecta mucho en el Akaike, por lo tanto definitivamente
#no es por qqu?.


fix_opc1 <- Arima(ts, c(5, 2, 22), method = "ML",
                  fixed = c(NA, 0, NA, 0, NA,
                            NA, 0, NA, 0, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA))
fix_opc1
#log likelihood = 23.29
#Akaike = -28.58

#Si mejora en comparaci?n con su equivalente
fix_opc2 <- Arima(ts, c(5, 2, 6), method = "ML",
                  fixed = c(NA, NA, 0, NA, NA,
                            0, 0, 0, NA, 0, 0))
fix_opc2
#log likelihood = 8.92
#Akaike = -5.84

#No mejora

fix_opc3 <- Arima(ts, c(5, 2, 1), method = "ML", 
                  fixed = c(NA, NA, 0, 0, 0,
                            NA))
fix_opc3
#log likelihood = 21.83
#Akaike = -35.65

#Si mejora (actualmente el mejor)

#NOTA: No se puede hacer un fix de opc4 porque todos sus ar y na son
#significativos.


######################################################################
#7- Establecer los modelos con AIC mas alto, y evaluar los residuales.
######################################################################

#Los modelos AIC m?s altos son:
# 1- fix_opc3    AIC: -35.65
# 2- opc3        AIC: -30.4
# 3- opc2        AIC: -29.62

pacf(fix_opc3$residuals) #Residual 21 sale de la franja.
acf(fix_opc3$residuals)  #Aqu? ninguno sale de la franja, ni siquiera el 21.
tsdiag(fix_opc3)

Box.test(fix_opc3$residuals,type="Ljung")
#p-value = 0.9506, por lo tanto es v?lida la hip?tesis nula.

jarque.bera.test(fix_opc3$residuals)
qqnorm(fix_opc3$residuals, pch=1, frame = F)
qqline(fix_opc3$residuals, col="steelblue", lwd=2)

pacf(opc3$residuals) #Ning?n residual se sale del l?mite.
acf(opc3$residuals)  #Ning?n residual se sale del l?mite.
tsdiag(opc3)

Box.test(opc3$residuals,type="Ljung")
#p-value = 0.982, por lo tanto es v?lida la hip?tesis nula.

jarque.bera.test(opc3$residuals)
qqnorm(opc3$residuals, pch=1, frame = F)
qqline(opc3$residuals, col="steelblue", lwd=2)

pacf(opc2$residuals) #Ning?n residual se sale del l?mite.
acf(opc2$residuals)  #Ning?n residual se sale del l?mite.
tsdiag(opc2)

Box.test(opc2$residuals,type="Ljung")
#p-value = 0.9265, por lo tanto es v?lida la hip?tesis nula.

jarque.bera.test(opc2$residuals)
qqnorm(opc2$residuals, pch=1, frame = F)
qqline(opc2$residuals, col="steelblue", lwd=2)

pacf(opcaa$residuals) #Residual 21 sale de la franja.
acf(opcaa$residuals)  #Aqu? ninguno sale de la franja, ni siquiera el 21.
tsdiag(opcaa)

Box.test(opc2$residuals,type="Ljung")
#p-value = 0.9263, por lo tanto es v?lida la hip?tesis nula.

jarque.bera.test(opc2$residuals)
qqnorm(opc2$residuals, pch=1, frame = F)
qqline(opc2$residuals, col="steelblue", lwd=2)


######################################################################
#8- Backtesting, utilice 3 diferentes ventanas de tiempo
#(defina que amplitud de tiempo utilizar para realizar el pron?stico).
######################################################################

#Utilizar? una ventana de tiempo de 14 ciclos.

#Pron?sticos de los modelos
fc_opc1 <- forecast(opc1, 7)
plot(fc_opc1)
lines(ts, col="red")

fc_opc2 <- forecast(opc2, 7)
plot(fc_opc2)
lines(ts, col="red")

fc_opc3 <- forecast(opc3, 7)
plot(fc_opc3)
lines(ts, col="red")

fc_opc4 <- forecast(opc4, 7)
plot(fc_opc4)
lines(ts, col="red")

fc_opcaa <- forecast(opcaa, 7)
plot(fc_opcaa)
lines(ts, col="red")

fc_fix_opc1 <- forecast(fix_opc1, 7)
plot(fc_fix_opc1)
lines(ts, col="red")

fc_fix_opc2 <- forecast(fix_opc2, 7)
plot(fc_fix_opc2)
lines(ts, col="red")

fc_fix_opc3 <- forecast(fix_opc3, 7)
plot(fc_fix_opc3)
lines(ts, col="red")


#############################################################################
#9- Calcular la Suma de los Errores al Cuadrado del error de pron?stico
#?Qu? modelo se comporta mejor? ?Coincide con el modelo con el AIC m?s bajo?
#############################################################################

sum(fc_opc1$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_opc2$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_opc3$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_opc4$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_opcaa$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_fix_opc1$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_fix_opc2$mean - ts[(length(ts)-6):(length(ts))])^2
sum(fc_fix_opc3$mean - ts[(length(ts)-6):(length(ts))])^2

#El modelo con menor sumatoria de las diferencias al cuadrado es el autoarima.
#El que mejor akaike hab?a obtenido (Arima(5, 2, 1) con no significativos = 0)
#fu? el de segundo peor rendimiento.


########################################################
#10- Definir el modelo ganador. Justificar su respuesta.
########################################################




