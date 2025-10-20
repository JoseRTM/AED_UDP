
# Cargar paquetes necesarios
library(skimr)        # Descripción rápida de la data
library(car)          # Funciones para análisis de regresión
library(lmtest)       # Tests de regresión
library(sandwich)     # Estimación robusta de errores estándar
library(nortest)      # Prueba de normalidad
library(DescTools)    # Test de colinealidad
library(rio)          # Importar datos
library(texreg)       # Permite usar screenreg

# Importar los datos
data <- import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/welfare.dta")

# Descripción rápida de la base de datos
skim(data)

# El objetivo de esta clase será modelar el índice Gini en relación con el gasto en educación

# ANALISIS DESCRIPTIVO
# Graficar sin usar ggplot2
plot(data$educ_expend, data$gini_slc, 
     xlab = "Gasto en educación (% del PIB)", 
     ylab = "Índice Gini", 
     main = "Relación entre Gasto en Educación e Índice Gini",
     pch = 19)  # Puntos sólidos
abline(lm(gini_slc ~ educ_expend, data = data), col = "red")  # Agregar línea de regresión

# MODELO 1: Regresión simple
modelo_1 <- lm(gini_slc ~ educ_expend + gdp, data = data)

# Regresión logarítmica
modelo_1_log <- lm(log(gini_slc) ~ educ_expend + gdp, data = data)

# Ver los resultados del modelo
screenreg(
  list(modelo_1, modelo_1_log),
  custom.model.names = c("Modelo 1", "Modelo log"),
  custom.coef.names = c("Constante", "Gasto en educación", "GDP")
)
# SUPUESTOS DE LA REGRESIÓN

# 1. LINEALIDAD
# Graficar los residuos contra los valores predichos (debe ser una nube de puntos)
plot(modelo_1$fitted.values, modelo_1$residuals, 
     xlab = "Valores Predichos", ylab = "Residuos", 
     main = "Valores Predichos vs Residuos")
abline(h = 0, col = "red")  # Línea horizontal en cero

# Corrección: Incluir un término cuadrático
data$educ_expend2 <- data$educ_expend^2
model_1_quadratic <- lm(gini_slc ~ educ_expend + educ_expend2 + gdp, data = data)
screenreg(model_1_quadratic)

# 2. HOMOCEDASTICIDAD
# Graficar los residuos
par(mfrow = c(2,2))
plot(modelo_1)
plot(modelo_1_log)

# Test de Breusch-Pagan para verificar homocedasticidad
bptest(modelo_1)
bptest(modelo_1_log)
#H0: Homocedasticidad
#H1: Heterocedasticidad

# Solución: Errores estándar robustos
modelo_1_robust_3 <- coeftest(modelo_1, vcov = vcovHC(modelo_1, "HC3"))
modelo_1_robust_1 <- coeftest(modelo_1, vcov = vcovHC(modelo_1, "HC1"))
modelo_1_robust_0 <- coeftest(modelo_1, vcov = vcovHC(modelo_1, "HC0"))

models_robust <- list(modelo_1, modelo_1_robust_0, modelo_1_robust_1, modelo_1_robust_3)
screenreg(models_robust,
          custom.model.names = c("Modelo sin SE robustos", "Robusto HC0", "Robusto HC1", "Robusto HC3"))

# 3. NORMALIDAD DE LOS RESIDUOS
# Q-Q plot para evaluar la normalidad de los residuos
par(mfrow = c(1,2))
qqnorm(modelo_1_log$residuals, main = "Q-Q Plot de los Residuos")
qqline(modelo_1_log$residuals, col = "red")
qqnorm(modelo_1$residuals, main = "Q-Q Plot de los Residuos")
qqline(modelo_1$residuals, col = "red")


# 4. AUSENCIA DE MULTICOLINEALIDAD
# Verificar VIF (Variance Inflation Factor)
vif(modelo_1)

# OBS: SI EL VIF ES ALTO ENTRE DOS VARIABLES LA SOLUCIÓN ES DEJAR SOLO 1.

# 5. INDEPENDENCIA DE LOS ERRORES
# Durbin-Watson test para verificar autocorrelación
dwtest(modelo_1)

#Si la autocorrelación es significativa, podrías considerar las siguientes opciones para corregirla:

# Usar un modelo adecuado para datos autocorrelacionados, como una regresión de Prais-Winsten o una regresión de Cochrane-Orcutt.
# Introducir términos de defasaje (lag) en el modelo.
# Ajustar un modelo ARIMA o algún otro modelo específico para series temporales, si los datos lo requieren.

# EJERCICIO
# CREAR LA VARIABLE PROMEDIO PUNTAJE QUE SEA EL PROMEDIO DE TODAS LAS PRUEBAS PAES
# HACER UN MODELO DE REGRESIÓN LINEAL USANDO COMO VARIABLE DEPENDIENTE LA VARIABLE CREADA
# LAS VARIABLES INDEPENDIENTES DEBEN SER DECIL DE INGRESO + PUNTAJE NEM + RANKING NEM
# TESTEAR SUPUESTOS E INTERPRETAR

