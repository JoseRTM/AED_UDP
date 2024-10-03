# Mesocúrtica - Distribución normal
set.seed(123)  # Para reproducibilidad
mesocurtica <- rnorm(1000, mean = 0, sd = 1)

# Leptocúrtica - Distribución t de Student
leptocurtica <- rt(1000, df = 3)  # t-student con df=3, que genera curtosis alta

# Platicúrtica - Distribución uniforme
platicurtica <- runif(1000, min = -sqrt(3), max = sqrt(3))  # ajustada para tener media 0 y varianza 1

# Visualización conjunta
par(mfrow = c(3, 1))  # Dividimos la ventana gráfica en 3 filas
hist(mesocurtica, breaks = 30, probability = TRUE, main = "Distribución Mesocúrtica (Normal)")
lines(density(mesocurtica), col = "blue", lwd = 2)

hist(leptocurtica, breaks = 30, probability = TRUE, main = "Distribución Leptocúrtica (t-student df=3)")
lines(density(leptocurtica), col = "red", lwd = 2)

hist(platicurtica, breaks = 30, probability = TRUE, main = "Distribución Platicúrtica (Uniforme)")
lines(density(platicurtica), col = "green", lwd = 2)

# APLICACIÓN COEFICIENTE DE VARIACIÓN
# Set de datos 1: baja desviación estándar, media baja
set.seed(123)
peso <- rnorm(1000, mean = 70, sd = 10)

# Set de datos 2: alta desviación estándar, pero media mucho mayor
altura <- rnorm(1000, mean = 170, sd = 20)

# Calcular la desviación estándar y el coeficiente de variación para ambos conjuntos de datos
sd1 <- sd(peso)
cv1 <- sd1 / mean(peso)*100

sd2 <- sd(altura)
cv2 <- sd2 / mean(altura)*100
