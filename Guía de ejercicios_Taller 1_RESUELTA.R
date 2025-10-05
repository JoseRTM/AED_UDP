# GUÍA DE EJERCICIOS ESTADÍSTICA DESCRIPTIVA Y VISUALIZACIÓN
# Active las librerías que va a usar
library(rio)
library(psych)
library(dplyr)
library(janitor)
# 1. Importe la base de datos jovenes.rds
# Es la base de datos de la Encuesta Nacional de la Juventud
# Encuesta poblacional realizada por INJUV
# La población representada son jóvenes de 15 a 29 años.
data <- import("jovenes.rds")

# 2. Guarde como objeto una base de datos reducida
# Debe contener las variables: sexo, edad, p76_1, nse, p81
# Hint: utilice []
# Hint: La p81 es una variable que mide el inicio de la actividad sexual
# Variable discreta.

data2 <- data[,c("SEXO", "EDAD", "P76_1", "NSE", "P81")]
# 3. De un vistazo general de la base de datos
# Hint: Puede usar str() o skim()
str(data2)
skimr::skim(data2)
# 4. Limpie las variables de ser necesario
data2$P76_1[data2$P76_1 == 99] <- NA
data2$P81[data2$P81 > 29] <- NA

# 5. Etiquete las variables siguiendo la siguiente estructura:
# p76_1: Es binaria, mide si una persona declara haber consumido
# alcohol alguna vez en su vida. (1: SI, 2: NO)
# nse: Nivel socioeconómico. (1: Alto, 2: Medio, 3: Bajo)
# sexo (1: Hombre, 2: Mujer)
data2$SEXO <- factor(data2$SEXO, levels = 1:2, labels = c("Hombre", "Mujer"))
data2$NSE <- factor(data2$NSE, levels = 1:3, labels = c("Alto", "Medio", "Bajo"))
data2$P76_1 <- factor(data2$P76_1, levels = 1:2, labels = c("Si", "No"))

# 6. Realice un análisis descriptivo completo de las variables. Construya un relato
# breve comentando los resultados.
describe(data2$P81)
# En promedio, las personas declararon iniciar su actividad sexual 
# a los 16 años en promedio.
# sesgo negativo, leptocúrtica
# El sesgo es bajo, la media y la mediana son muy parecidas

describe(data2$EDAD)
# simétrica, el promedio es de 22 años.

data2 %>% tabyl(SEXO)
# 55% mujeres

# Hint: Utilice describe() para cuantitativas
# Hint: Utilice table, tab1 o tabyl para las cualitativas

# 7. Caracterice la distribución de la variable p81
# utilizando medidas descriptivas y un gráfico de densidad
# Hint: Puede usar plot() y la funcion density()
plot(density(data2$P81, na.rm = T),  # Especificamos la variable numérica
     main="Edad de inicio de la actividad Sexual",  # Título del gráfico
     xlab="Edad",  # Etiqueta del eje X
     col="blue",  # Color de la curva
     lwd=2)  # Ancho de la línea

# 8. Compare a través de un gráfico la diferencia en la edad de inicio
# entre quienes declaran haber consumido alcohol y las que no lo hicieron.
# Discuta los resultados. 
boxplot(P81 ~ P76_1,  # Fórmula: mpg es la variable dependiente y cyl es la variable categórica
        data=data2,  # Especificamos el dataset
        main="Edad de inicio por Consumo de alcohol (Vida)",  # Título del gráfico
        xlab="Uso de alcohol",  # Etiqueta del eje X
        ylab="Edad en años",  # Etiqueta del eje Y
        col="orange")  # Color de los boxplots
# No se observan diferencias en torno a la edad de inicio, se comportan
# prácticamente igual entre quienes declararon consumir alcohol.

# 9. Compare a través de un gráfico la diferencia en la edad de inicio por sexo
# Discuta los resultados
boxplot(P81 ~ SEXO,  # Fórmula: mpg es la variable dependiente y cyl es la variable categórica
        data=data2,  # Especificamos el dataset
        main="Edad de inicio por Consumo de alcohol (Vida)",  # Título del gráfico
        xlab="Uso de alcohol",  # Etiqueta del eje X
        ylab="Edad en años",  # Etiqueta del eje Y
        col="orange")  # Color de los boxplots
# 10. Compare a través un gráfico la frecuencia de consumo de alcohol por sexo
# Discuta los resultados
freq <- table(data2$SEXO, data2$P76_1)  
barplot(freq, 
        beside=TRUE,  # beside=TRUE pone las barras una al lado de la otra
        main="Consumo de Alcohol por sexo",  # Título del gráfico
        xlab="Consumo de Alcohol",  # Etiqueta del eje X
        col=c("red", "black"),
        ylim = c(0,3200),# Colores de las barras para cada grupo
        legend = rownames(freq))  # Leyenda para cada grupo de cilindros

# 10. Compare a través un gráfico el porcentaje de consumo de alcohol por sexo
# Discuta los resultados
# Hint: Utilice margins = 1 para obtener las proporciones por fila
# Discuta la diferencia entre este gráfico y el anterior.

freq2 <- (table(data2$SEXO, data2$P76_1)  %>% prop.table(margin = 2)*100) %>% round(1)
barplot(freq2, 
        beside=TRUE,  # beside=TRUE pone las barras una al lado de la otra
        main="Consumo de Alcohol por sexo",  # Título del gráfico
        xlab="Consumo de Alcohol",  # Etiqueta del eje X
        col=c("red", "black"),
        ylim = c(0,70),# Colores de las barras para cada grupo
        legend = rownames(freq2))  # Leyenda para cada grupo de cilindros