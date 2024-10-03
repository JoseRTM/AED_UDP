
# MEDIDAS DE TENDENCIA CENTRAL   

# ACTIVAR LIBRERÍAS
library(haven)
library(psych)
library(skimr)

# CARGAR DATOS
data <- read_sav("casen_red.sav")

# VISTAZO
str(data)
skim(data)

# MEDIA Y MEDIANA DE V18 usando funciones base
mean(data$v18, na.rm = TRUE) # Calcula la media de v18 ignorando los NA
median(data$v18, na.rm = TRUE) # Calcula la mediana de v18 ignorando los NA

# RECORDATORIO: LA VARIABLE INCLUYE VALORES -88
# En lugar de mutate() de dplyr, utilizamos subsetting base de R
data$v18[data$v18 == -88] <- NA # Reemplaza valores -88 por NA
mean(data$v18, na.rm = TRUE) # Recalcula la media
median(data$v18, na.rm = TRUE) # Recalcula la mediana

# MODA
# Para calcular la moda en base R, se puede usar la tabla de frecuencias.
frecuencia <- table(data$v18) # Tabla de frecuencias de v18
moda <- as.numeric(names(frecuencia)[frecuencia == max(frecuencia)]) # Encuentra el valor con mayor frecuencia
moda # Imprime la moda

# Nota: Para ver las 5 observaciones más frecuentes
sort(frecuencia, decreasing = TRUE)[1:5] # Muestra las 5 frecuencias más altas

# MEDIDAS DE DISPERSIÓN

# RANGO
range(data$v18, na.rm = TRUE) # Calcula el rango de los valores ignorando los NA

# RANGO INTER CUARTILICO
# Es la diferencia entre Q3 y Q1 usando funciones base
quantile(data$v18, 0.75, na.rm = TRUE) - quantile(data$v18, 0.25, na.rm = TRUE)

# Rango intercuartílico más simple con IQR
IQR(data$v18, na.rm = TRUE)

# VARIANZA
var(data$v18, na.rm = TRUE) # Calcula la varianza de v18

# DESVIACIÓN ESTÁNDAR
sd(data$v18, na.rm = TRUE) # Calcula la desviación estándar

# COEFICIENTE DE VARIACIÓN
(sd(data$v18, na.rm = TRUE) / mean(data$v18, na.rm = TRUE)) * 100 # Calcula el coeficiente de variación

# MEDIDAS DE POSICIÓN

# Función para calcular posiciones es quantile

# Cálculo de percentiles, cuartiles, quintiles y deciles usando quantile()
cuartiles <- quantile(data$v18, probs = c(0.25, 0.50, 0.75), na.rm = T)
quintiles <- quantile(data$v18, probs = c(0.20, 0.40, 0.60, 0.80), na.rm = T)
deciles <- quantile(data$v18, probs = seq(0.1, 0.9, by = 0.1), na.rm = T)

# NOTA: en el argumento probs debemos realizar los cortes. 
# Recuerda que el total de la distribución es 1

# SESGO

# El sesgo mide la asimetría de una distribución. 
# Si la distribución es simétrica, el sesgo será cero.
simetrica <- rnorm(10000,100,5)
hist(simetrica, main = "Distribución simétrica", xlab = "Valores x",
     ylab = "Frecuencia")
abline(v = mean(simetrica), col = "red", lwd = 2, lty = 2)
abline(v = median(simetrica), col = "blue", lwd = 2, lty = 2)

# CUANDO LA DISTRIBUCIÓN ES SIMÉTRICA LA MEDIA Y LA MEDIANA SON IGUALES
# O MUY PARECIDAS

# Sesgo positivo indica que la cola de la distribución 
# se extiende hacia la derecha (valores más altos).
hist(data$v18, main = "Costo del arriendo", xlab = "Costo en $", ylab= "Frecuencia")
abline(v = mean(data$v18), col = "red", lwd = 2, lty = 2) 
abline(v = min(data$v18), col = "blue", lwd = 2, lty = 2) 
abline(v = max(data$v18), col = "blue", lwd = 2, lty = 2) 

# Sesgo negativo indica que la cola de la distribución 
# se extiende hacia la izquierda (valores más bajos).
sesgo <- -rlnorm(1000, meanlog = 2, sdlog = 0.5)
hist(sesgo, main = "Sesgo negativo", ylab= "Frecuencia")
abline(v = mean(sesgo), col = "red", lwd = 2, lty = 2) 
abline(v = min(sesgo), col = "blue", lwd = 2, lty = 2)
abline(v = max(sesgo), col = "blue", lwd = 2, lty = 2)

# CURTOSIS

# La curtosis mide la "forma" o el grado de concentración de los datos 
# en los extremos (colas) de la distribución.
# Curtosis alta (leptocúrtica): Indica que la distribución tiene colas largas, 
# con más datos concentrados en los extremos.
# Curtosis baja (platicúrtica): Indica colas más cortas y distribución más plana.
# Curtosis normal (mesocúrtica): Tiene un valor cercano a 3, 
# que corresponde a la curtosis de una distribución normal estándar.

# EL PAQUETE PSYCH DESBLOQUEA LA FUNCIÓN describe()
describe(data$v18)

# ESTADÍSTICA BIVARIADA
# LIMPIEZA Y MANIPULACIÓN
arriendo <- data[data$v18 >= 0 & (data$region == 2 | data$region == 5 | data$region == 13), ]
arriendo$region <- factor(arriendo$region, levels = c(2, 5, 13),
                          labels = c("Antofagasta", "Valparaíso", "RM"))
# ¿ EN QUÉ REGIÓN SE PAGA MÁS ARRIENDO?
# Para agrupar en base R, utilizamos tapply()
tapply(arriendo$v18, arriendo$region, mean, na.rm = TRUE)
# PRIMERO LA VARIABLE CUANTITATIVA
# SEGUNDA LA CUALITATIVA
# TERCERO LA FUNCIÓN (mean)

# PSYCH nos desbloquea la función describeBy
# Podemos seguir usando describeBy para estadística bivariada
describeBy(arriendo$v18, arriendo$region)