####################
# REGRESIÓN LINEAL #
####################

# En su forma más simple, la regresión lineal establece una línea recta 
# (llamada "línea de regresión") que mejor se ajusta a los datos datos, 
# según el criterio de los mínimos cuadrados. Esta línea se representa con 
# la ecuación y=b0+b1X1
# donde y es la variable dependiente, 
# x es la variable independiente, 
# b1 es la pendiente de la línea, y 
# y b0 es el intercepto.

# El objetivo es minimizar la suma de los cuadrados de 
# las diferencias entre los valores observados y los predichos por el modelo. 
# Esto se hace ajustando la pendiente 
# b1 y el intercepto b0 de la línea.

# Los coeficientes indican la importancia de cada variable independiente en la 
# predicción de la variable dependiente. Un coeficiente positivo sugiere una relación 
# directa, mientras que un coeficiente negativo indica una relación inversa.

# La regresión lineal se usa en muchas áreas, como economía, biología, 
# ingeniería y ciencias sociales, para entender las relaciones entre variables 
# y hacer predicciones.

# LIBRERÍAS
library(texreg) # Salida de regresión (screenreg)
library(rio) # importación
library(skimr) # skim
library(quantreg) # regresión cuantil
library(survey) # Ponderadores
library(haven) # trabajar con haven labelled

# IMPORTACIÓN DE DATOS
data <- import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/Affairs.csv")

# PARA MAYOR INFORMACIÓN SOBRE LA BASE DE DATOS
# https://www.statsmodels.org/stable/datasets/generated/fair.html

skim(data)

# EJERCICIO
# REALIZAR UN GRÁFICO PARA VISUALIZAR LA RELACIÓN ENTRE EDAD Y AFFAIRS
# REALIZAR UN TEST DE CORRELACIÓN, INTERPRETAR EL IC

# AJUSTAR MODELO
modelo <- lm(affairs ~ age, data = data)
# resumir la información del modelo
summary(modelo)

# intervalos de confianza para los beta
confint(modelo)

# El paquete texreg nos permite usar la función screenreg
screenreg(modelo,
          custom.model.names = "Modelo 1",
          custom.coef.names = c("Constante", "Edad"))

# REGRESIÓN LINEAL CON PONDERADORES
# DATOS
data <- rio::import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/jovenes.rds")

# limpieza
data$P82[data$P82 == 99] <- NA
data$P82[data$P82 == 97] <- NA
data$SEXO <- factor(data$SEXO, levels = c(1,2), labels = c("Hombre", "Mujer"))

# Creamos el diseño de encuesta polietápico con pesos combinados
survey <- svydesign(id = ~CONGLOMERADO, strata = ~ESTRATO, 
                    data = data, weights = ~PONDERADOR)


psych::describeBy(data$P82, data$SEXO)

modelo_ponderado <- svyglm(P82 ~ SEXO, design = survey)
summary(modelo_ponderado)

# REGRESIÓN MÚLTIPLE
modelo2 <- lm(P82 ~ SEXO + EDAD, data = data) # se agrega una variable con +
summary(modelo2)
# los coeficientes varían de acuerdo a la configuración de variables independientes del modelo

modelo2_ponderado <- svyglm(P82 ~ SEXO + EDAD, design = survey)
summary(modelo2_ponderado)

# PREDICCIONES
# ----------------------------
# REGRESIÓN LINEAL CON PREDICT
# ----------------------------

# Generamos una nueva data frame con valores de SEXO y EDAD dentro del rango solicitado (15 a 29 años)
nueva_data <- expand.grid(
  SEXO = factor(c("Hombre", "Mujer"), levels = c("Hombre", "Mujer")),
  EDAD = 15:29  # Rango de edades entre 15 y 29 años
)

# Generar predicciones para el modelo no ponderado
predicciones_no_ponderado <- predict(modelo2, newdata = nueva_data)

# Generar predicciones para el modelo ponderado
predicciones_ponderado <- predict(modelo2_ponderado, newdata = nueva_data)

# Añadimos las predicciones al nuevo data frame
nueva_data$Pred_no_ponderado <- predicciones_no_ponderado
nueva_data$Pred_ponderado <- predicciones_ponderado

# Visualizamos los primeros valores de la nueva data frame con las predicciones
head(nueva_data)

# ----------------------------
# VISUALIZACIÓN DE LAS PREDICCIONES CON R BASE
# ----------------------------

# Configuración gráfica para R base
par(mfrow = c(1, 2))  # Dividir el espacio gráfico en 2 columnas

# Gráfico para el modelo no ponderado (una línea para Hombres y otra para Mujeres)
plot(nueva_data$EDAD[nueva_data$SEXO == "Hombre"], nueva_data$Pred_no_ponderado[nueva_data$SEXO == "Hombre"], 
     type = "l", col = "blue", xlab = "Edad", ylab = "Predicción de Affairs (P82)", 
     main = "Modelo No Ponderado", lwd = 2, ylim = range(nueva_data$Pred_no_ponderado))
lines(nueva_data$EDAD[nueva_data$SEXO == "Mujer"], nueva_data$Pred_no_ponderado[nueva_data$SEXO == "Mujer"], 
      col = "red", lwd = 2)  # Línea para Mujeres
legend("topleft", legend = c("Hombre", "Mujer"), col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.4)

# Gráfico para el modelo ponderado (una línea para Hombres y otra para Mujeres)
plot(nueva_data$EDAD[nueva_data$SEXO == "Hombre"], nueva_data$Pred_ponderado[nueva_data$SEXO == "Hombre"], 
     type = "l", col = "blue", xlab = "Edad", ylab = "Predicción de Affairs (P82)", 
     main = "Modelo Ponderado", lwd = 2, ylim = range(nueva_data$Pred_ponderado))
lines(nueva_data$EDAD[nueva_data$SEXO == "Mujer"], nueva_data$Pred_ponderado[nueva_data$SEXO == "Mujer"], 
      col = "red", lwd = 2)  # Línea para Mujeres
legend("topleft", legend = c("Hombre", "Mujer"), col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.4)

# Restaurar la configuración gráfica original
par(mfrow = c(1, 1))

# EJERCICIO:
# Ajuste un modelo de regresión lineal incluyendo más variables
# interprete los coeficientes, el R2 y el ANOVA de la regresión
# Compare los modelos lineal y cuantil usando screenreg


