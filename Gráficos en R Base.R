
# GRÁFICOS DE DATOS CON BASE R
# Este script muestra cómo generar diferentes tipos de gráficos utilizando solo funciones base de R.

# Cargar datos y manipulación de datos
# Para estos ejemplos, usamos datasets incluidos en R o simulamos datos.

# 1. GRÁFICO DE BARRAS
# El gráfico de barras es útil para visualizar datos categóricos. Aquí mostramos la frecuencia de los cilindros en mtcars.

# Datos: mtcars
data <- mtcars
?mtcars

# ETIQUETAS
data$am <- factor(data$am, levels = c(0,1), 
                  labels = c("Automático", "Manual"))
# Generar gráfico de barras con barplot() 
freq <- table(data$cyl)  # Table genera una tabla de frecuencias
barplot(freq, 
        main="Gráfico de Barras: Número de Cilindros",  # Título del gráfico
        xlab="Cilindros",  # Etiqueta del eje X
        ylab="Frecuencia",  # Etiqueta del eje Y
        col=c("blue", "green", "red"))  # Color de las barras

# 2. GRÁFICO DE BARRAS AGRUPADAS
# En este gráfico agrupamos las barras según una segunda variable. Usamos el dataset mtcars.

# Generar gráfico de barras agrupadas usando barplot() 
freq2 <- table(data$cyl, data$am)  # Tabulación cruzada entre cilindros y transmisión
barplot(freq2, 
        beside=TRUE,  # beside=TRUE pone las barras una al lado de la otra
        main="Gráfico de Barras Agrupadas",  # Título del gráfico
        xlab="Cilindros",  # Etiqueta del eje X
        col=c("blue", "red", "green"),  # Colores de las barras para cada grupo
        legend = rownames(freq2))  # Leyenda para cada grupo de cilindros

# beside = FALSE
barplot(freq2, 
        beside=F,  # beside=TRUE pone las barras una al lado de la otra
        main="Gráfico de Barras Agrupadas",  # Título del gráfico
        xlab="Cilindros",  # Etiqueta del eje X
        col=c("blue", "red", "green"),  # Colores de las barras para cada grupo
        legend = rownames(freq2))  # Leyenda para cada grupo de cilindros

# 3. GRÁFICO DE LÍNEAS
# El gráfico de líneas es útil para visualizar tendencias a lo largo del tiempo.

# Datos: el dataset 'economics' incluido en el paquete ggplot2
rm(list = ls())
data <- ggplot2::economics
?economics

# TRANSFORMAR A DATE
data$date <- as.Date(data$date)
# Crear gráfico de líneas básico
plot(data$date, data$pop, 
     type="l",  # type="l" indica que es un gráfico de líneas
     main="Gráfico de Líneas: Población",  # Título del gráfico
     xlab="Tiempo",  # Etiqueta del eje X
     ylab="Población total",  # Etiqueta del eje Y
     col="blue")  # Color de la línea

# Gráfico con doble línea
plot(data$date, data$uempmed, 
     type="l",  # Gráfico de líneas
     main="Ahorros personas y tiempo medio de desempleo",  # Título del gráfico
     xlab="Tiempo",  # Etiqueta del eje X
     ylab="Indicador",  # Etiqueta del eje Y
     col="blue",  # Color de la línea
     xaxt='n',# Desactivar el eje X para personalizarlo
     ylim = c(0,25))  # límites del eje Y

# Personalizar los ticks del eje X con intervalos de 3 años
axis(1, at=seq(as.Date("1967-01-01"), as.Date("2015-12-31"), by="3 years"), 
     labels=format(seq(as.Date("1967-01-01"), as.Date("2015-12-31"), by="3 years"), "%Y"))

lines(data$date, data$psavert, type = "l", col = "red")
legend("topleft", legend=c("Ahorros personales", "Tiempo medio de desempleo"),
       col=c("red", "blue"), lty = 1:1, cex=0.7) 

#El argumento lty en la función legend() en R se refiere al "tipo de línea" o "line type". Controla el estilo de las líneas que aparecen en la leyenda, y puede tomar los siguientes valores:
# lty = 1: línea sólida (por defecto).
# lty = 2: línea punteada.
# lty = 3: línea con guiones y puntos (dash-dot).
# lty = 4: línea con guiones largos.
# lty = 5: línea con guiones cortos.
# lty = 6: línea con guiones y dos puntos.
# lty = 1:2 significa que la primera línea en la leyenda (para "Ahorros personales") será sólida (lty = 1), 
# y la segunda línea (para "Tiempo medio de desempleo") será punteada (lty = 2).

# 4. BOXPLOT
# El boxplot es útil para mostrar la distribución de una variable numérica agrupada por una variable categórica.

# Gráfico de boxplot en base R
boxplot(mpg ~ cyl,  # Fórmula: mpg es la variable dependiente y cyl es la variable categórica
        data=mtcars,  # Especificamos el dataset
        main="Boxplot de Consumo por Número de Cilindros",  # Título del gráfico
        xlab="Cilindros",  # Etiqueta del eje X
        ylab="Millas por Galón",  # Etiqueta del eje Y
        col="orange")  # Color de los boxplots

# 5. GRÁFICO DE DISPERSIÓN
# El gráfico de dispersión muestra la relación entre dos variables numéricas.

# Gráfico de dispersión en base R
plot(mtcars$wt, mtcars$mpg,  # Especificamos las variables del eje X e Y
     main="Gráfico de Dispersión: Peso vs Consumo",  # Título del gráfico
     xlab="Peso del Vehículo",  # Etiqueta del eje X
     ylab="Millas por Galón",  # Etiqueta del eje Y
     col="blue",  # Color de los puntos
     pch=19)  # Tipo de punto

# Añadir una línea de regresión para ver la tendencia
abline(lm(mpg ~ wt, data=mtcars),  # Línea de regresión
       col="red",  # Color de la línea
       lwd=2)  # Ancho de la línea

# 6. DISPERSIÓN MULTIVARIADA
# Para visualizar relaciones entre varias variables numéricas, usamos el gráfico pairs().

# Gráfico de dispersión multivariada
pairs(~ mpg + wt + hp + qsec,  # Fórmula con varias variables
      data=mtcars,  # Especificamos el dataset
      main="Dispersión Multivariada: mtcars",  # Título del gráfico
      col="darkgreen")  # Color de los puntos

# 7. HISTOGRAMA
# El histograma es útil para visualizar la distribución de una variable numérica.

# Generar histograma en base R
hist(mtcars$mpg,  # Especificamos la variable numérica
     main="Histograma: Millas por Galón",  # Título del gráfico
     xlab="Millas por Galón",  # Etiqueta del eje X
     col="lightblue",  # Color de las barras
     border="black")  # Color del borde de las barras
abline(v = mean(mtcars$mpg), col = "red", lty = 2, lwd = 2)
# añade una linea vertical
# el argumento v indica que es vertical
abline(h = 1, col = "red", lty = 2, lwd = 2)
# h indica que la linea es horizontal
# el argumento lwd cambia el grosor de la línea

# 8. GRÁFICO DE DENSIDAD
# El gráfico de densidad muestra una estimación suavizada de la distribución de una variable numérica.

# Generar gráfico de densidad en base R
plot(density(mtcars$mpg),  # Especificamos la variable numérica
     main="Gráfico de Densidad: Millas por Galón",  # Título del gráfico
     xlab="Millas por Galón",  # Etiqueta del eje X
     col="purple",  # Color de la curva
     lwd=2)  # Ancho de la línea

# 9. USO DE MFROW PARA MÚLTIPLES GRÁFICOS
# Con mfrow, podemos mostrar múltiples gráficos en una sola ventana.
# Aquí mostramos 2 gráficos lado a lado.

par(mfrow=c(1, 2))  # Configura la ventana para mostrar 2 gráficos (1 fila, 2 columnas)

# Generar dos gráficos en la misma ventana
plot(mtcars$wt, mtcars$mpg, main="Peso vs Consumo", col="blue", pch=19)
hist(mtcars$mpg, main="Histograma: Millas por Galón", col="lightblue", border="black")

# Resetear a una sola ventana de gráfico
par(mfrow=c(1, 1))

# Ejercicio
# Importar la base de datos de la paes y generar 
# gráficos de dispersión entre todos los puntajes
# Recuerde limpiar las variables.
