# Miguel Angel Solis Orozco
#Series de tiempo - Examen 1

#NOTA PARA EL PROFESOR (y spoiler alert)
#Originalmente había intentado integral de orden 2 pero el autoarima me salió
#de orden 1 y resultó ser el mejor resultado, por lo tanto decidí rehacer el 
#examen con orden 1 y los resultados de mis arimas fueron mucho mejores.
#(Si quiere ver la vieja versión con orden 2 hágamelo saber)


######################
#1. Graficar la serie.
######################

library(quantmod)
library(forecast)
library(tseries)
library(tsoutliers)

#ds <- read.csv("/Users/MikeSolis/Google Drive/Maestría en ciencia de datos/2do semestre/Series de tiempo/Examen 1/examen_iteso_21a.csv")
ds <- read.csv("C:/Users/Miguel/Google Drive/Maestría en ciencia de datos/2do semestre/Series de tiempo/Examen 1/examen_iteso_21a.csv")
ts <- ds$V5
ts.plot(ts, type = "l", xlab = "Time", ylab = "Vector 5")
#Ya podemos ir descartando que sea ruido blanco. u_u


####################################################
#2. Determinar el orden de integraci?n de la serie.
####################################################

adf.test(ts)
kpss.test(ts)
pacf(ts)
acf(ts)

#Ya hay incongruencia entre Dickey-Fuller y KPSS pues en kpps el resultado es
#estacionaria y en kpss es no estacionaria. Sin embargo al correr nuestra
#función de auto correlación y ver la gráfica nos encontramos con que hay
#raíz unitaria, por lo tanto confirmamos que no es estacionaria.
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
#acerca mucho al límite con el p-value. Lo dejaremos en orden 1.
#Los resagos significativos bajaron bastante en aacf pero hay 4 que podemos
#considerar de esta naturaleza.


##############################################################
#4- Establecer la lista de posibles candidatos a modelo ARIMA.
##############################################################

#Opción1: Arima(2, 1, 4)
#Opción2: Arima(2, 1, 3)
#Opción3: Arima(2, 1, 2)


##############################################################################
#5- Analizar auto.arima. Por favor discuta sus diferencias con los candidatos
#del modelo 4.
##############################################################################

opcaa <- auto.arima(ts)
opcaa
#ARIMA(2, 1, 0)
#Significativos:
#ar1, ar2

#log likelihood = 24.04
#aic = -42.08

#Un akaike de -42.08 es muy bueno. Veamos los arimas propuestos,


#####################################################################
#6- Estimar los modelos para cada uno de los candidatos. Eliminar por
#significancia (determine el nivel de significancia).
#####################################################################

#Tomaré el 5% como indicador para definir si algún resago es significativo.
opc1 <- Arima(ts, c(2, 1, 4), method = "ML")
opc1
#No hay coeficientes significativos

#log likelihood = 24.58
#AIC = -35.17


opc2 <- Arima(ts, c(2, 1, 3), method = "ML")
opc2
#Significativos
#ar2
#ma1

#log likelihood = 24.58
#AIC = -37.16


opc3 <- Arima(ts, c(2, 1, 2), method = "ML")
opc3
#significativos
#ar2

#log likelihood = 24.24
#AIC = -38.48

#Los Akaikes no compiten contra el auto arima. Aunque la opción 3 se acerca
#bastante. El log likelihood es prácticamente el mismo en todos los modelos.
#Intentaré hacer un fix en la opción 2 y 3, igualando a 0 todos los
#coeficientes que no son significativos.

fix_opc2 <- Arima(ts, c(2, 1, 3), method = "ML",
                  fixed = c(0, NA,
                            NA, 0, 0))
fix_opc2
#log likelihood = 23.66
#Akaike = -41.31

#si mejora, aunque el auto arima sigue teniendo un mejor akaike (.42.08)
#este fix ya se acerca por mucho.

fix_opc3 <- Arima(ts, c(2, 1, 2), method = "ML", 
                  fixed = c(0, NA,
                            0, 0))
fix_opc3
#log likelihood = 8.49
#Akaike = -12.99

#No mejora. Descartado.


######################################################################
#7- Establecer los modelos con AIC mas alto, y evaluar los residuales.
######################################################################

#Los modelos AIC más altos son:
# 1- fix_opc2    AIC: -41.31
# 2- opc3        AIC: -38.48
# 3- opc2        AIC: -37.16

pacf(fix_opc2$residuals) #Ningún residual se sale de la franja. El 21 se acerca.
acf(fix_opc2$residuals)  #Ningún residual se sale de la franja. El 21 se acerca.
tsdiag(fix_opc2)         #El tercer p value es un poco bajo pero muy lejos del
                         #límite.      

Box.test(fix_opc2$residuals,type="Ljung")
#p-value = 0.8326, por lo tanto es válida la hipótesis nula.

jarque.bera.test(fix_opc2$residuals)
#p-value = 0.4352 por lo tanto es válida la hipótesis nula y es normal.
qqnorm(fix_opc2$residuals, pch=1, frame = F)
qqline(fix_opc2$residuals, col="steelblue", lwd=2)
#Se sale de la línea en el extremo superior derecho pero no agudamente.

pacf(opc3$residuals) #Ningún residual se sale del límite. El 21 se acerca.
acf(opc3$residuals)  #Ningún residual se sale del límite. El 21 se acerca.
tsdiag(opc3)         #Todo en orden con los p-values.

Box.test(opc3$residuals,type="Ljung")
#p-value = 0.9284, por lo tanto es válida la hipótesis nula.

jarque.bera.test(opc3$residuals)
#p-value = 0.4051, por lo tanto es válida la hipótesis nula y es normal.
qqnorm(opc3$residuals, pch=1, frame = F)
qqline(opc3$residuals, col="steelblue", lwd=2)
#Se sale ampliamente de la línea en el extremo superior derecho pero no
#agudamente.

pacf(opc2$residuals) #Ningún residual se sale de la franja. El 21 se acerca.
acf(opc2$residuals)  #Ningún residual se sale de la franja. El 21 se acerca.
tsdiag(opc2)         #Todo en orden con los p-values.

Box.test(opc2$residuals,type="Ljung")
#p-value = 0.9317, por lo tanto es válida la hipótesis nula.

jarque.bera.test(opc2$residuals)
#p-value = 0.4921, por lo tanto es válida la hipótesis nula y es normal.
qqnorm(opc2$residuals, pch=1, frame = F)
qqline(opc2$residuals, col="steelblue", lwd=2)
#Se sale ampliamente de la línea en el extremo superior derecho pero no
#agudamente.

#Quise correr las evaluaciones de residuales del auto arima
#solo por curiosidad y también porque tiene el akaike más alto de todos los
#modelos.

pacf(opcaa$residuals) #Residual 21 apenjas toca la franja.
acf(opcaa$residuals)  #Aquí ninguno sale de la franja, ni siquiera el 21.
tsdiag(opcaa)         #Todo en orden con los p-values.

Box.test(opcaa$residuals, type="Ljung")
#p-value = 0.9538, por lo tanto es válida la hipótesis nula.

jarque.bera.test(opcaa$residuals)
#p-value = 0.4983, por lo tanto es válida la hipótesis nula y es normal.
qqnorm(opcaa$residuals, pch=1, frame = F)
qqline(opcaa$residuals, col="steelblue", lwd=2)
#Se sale tenuemente de la línea en el extremo superior derecho pero no
#agudamente.


######################################################################
#8- Backtesting, utilice 3 diferentes ventanas de tiempo
#(defina que amplitud de tiempo utilizar para realizar el pronóstico).
######################################################################

#Utilizaré una amplitud de tiempo de 7 ciclos.

#Pronósticos de los modelos
fc_fix_opc2 <- forecast(fix_opc2, 7)
plot(fc_fix_opc2)
lines(ts, col="red")

fc_opc2 <- forecast(opc2, 7)
plot(fc_opc2)
lines(ts, col="red")

fc_opc3 <- forecast(opc3, 7)
plot(fc_opc3)
lines(ts, col="red")

#Correré el pronótico del autoarima ya que es muy similar en akaike que el
#modelo fix_opc2. 

fc_opcaa <- forecast(opcaa, 7)
plot(fc_opcaa)
lines(ts, col="red")

#Decidí incluír el auto arima para ver su comportamiento en comparación a
#los otros modelos.

#A simple vista los pronósticos son muy similares pero veamos que nos
#dicen las sumas de los errores al cuadrado.


#############################################################################
#9- Calcular la Suma de los Errores al Cuadrado del error de pronóstico
#¿Qué modelo se comporta mejor? ¿Coincide con el modelo con el AIC más bajo?
#############################################################################

sum(fc_fix_opc2$mean - ts[(length(ts)-6):(length(ts))])^2
#1.56627
sum(fc_opc2$mean - ts[(length(ts)-6):(length(ts))])^2
#2.361964
sum(fc_opc3$mean - ts[(length(ts)-6):(length(ts))])^2
#2.417854
sum(fc_opcaa$mean - ts[(length(ts)-6):(length(ts))])^2
#2.397817

#El modelo con menor sumatoria de las diferencias al cuadrado es fix_opc2.
#Incluso más que el auto arima el cual tiene mejor akaike.


########################################################
#10- Definir el modelo ganador. Justificar su respuesta.
########################################################

#Mi conclusión es que el modelo ganador es el fc_fix_opc2. Ya que, aunque
#el auto arima tiene mejor akaike, la diferencia entre ambos es de tan sólo
#0.77. En la suma de los errores cuadrados por otro lado, la diferencia es de
#0.831547. Incluso la opción 2 (sin arreglar(opc2)) esmejor que el autoarima.
#El log likelihood no jugó un papel importante en la decisión al ser casi igual
#en todos los modelos. Los otros dos modelos arima tienen peor akaike y salieron
#peor en la suma de las diferencias que el modelo ganador.

#Así quedaría el orden de mejor a peor:

#fc_fix_opc2
#fc_opcaa
#fc_opc2
#fc_opc3

#NOTA EXTRA:
#Como ya había comentado, anteriormente me aventuré a hacer la integral de
#orden 2 pero esto causó que mis modelos arima propuestos tuvieran mucho menor
#rendimiento a la hora de predecir, comparados con el autoarima. 
#Incluso mi modelo con mejor akaike (-35.65) era el segundo modelo menos
#eficiente en la suma de errores cuadrados. No me quise quedar con esta
#conclusión, así que cambié a derivada de orden 1 y todo cambió e hizo mucho
#más sentido.

#########
#THE END#
#########

