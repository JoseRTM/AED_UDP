#--------------------------------------------#
# GUÍA DE EJERCICIOS ESTADÍSTICA DESCRIPTIVA #
#--------------------------------------------#

# 1) CARGAR LIBRERÍAS

# 2) IMPORTAR LA BASE DE DATOS mineduc_paes
data <- rio::import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/mineduc_paes.rds")
# 3) RESPONDER LAS SIGUIENTES PREGUNTAS CON VERDADERO O FALSO
# DEBE JUSTIFICAR SU ELECCIÓN CON EVIDENCIA EMPÍRICA

# 1) La distribución de puntajes del NEM es simétrica.
data$PTJE_NEM[data$PTJE_NEM == 0] <- NA # Reemplaza valores 0 por NA
data$PTJE_RANKING[data$PTJE_RANKING == 0] <- NA # Reemplaza valores 0 por NA
data$CLEC_REG_ACTUAL[data$CLEC_REG_ACTUAL == 0] <- NA # Reemplaza valores 0 por NA
data$MATE1_REG_ACTUAL[data$MATE1_REG_ACTUAL == 0] <- NA # Reemplaza valores 0 por NA
data$MATE2_REG_ACTUAL[data$MATE2_REG_ACTUAL == 0] <- NA # Reemplaza valores 0 por NA
data$INGRESO_PERCAPITA_GRUPO_FA[data$INGRESO_PERCAPITA_GRUPO_FA == 99] <- NA # Reemplaza valores 99 por NA
data$HCSOC_REG_ACTUAL[data$HCSOC_REG_ACTUAL == 0] <- NA # Reemplaza valores 0 por NA
data$CIEN_REG_ACTUAL[data$CIEN_REG_ACTUAL == 0] <- NA # Reemplaza valores 0 por NA
describe(data$PTJE_NEM)
hist(data$PTJE_NEM)

# 2) Los puntaje de la prueba de matemáticas 1 son mayores que los
# puntajes de comprensión lectora.
describe(data$MATE1_REG_ACTUAL)
describe(data$CLEC_REG_ACTUAL)

# 3) Los estudiantes del décimo decil de ingreso tienen mayores
# puntajes en todas las pruebas de la PAES en comparación a los
# deciles inferiores.
data$INGRESO_PERCAPITA_GRUPO_FA[data$INGRESO_PERCAPITA_GRUPO_FA == 99] <- NA
describeBy(data$CLEC_REG_ACTUAL, data$INGRESO_PERCAPITA_GRUPO_FA)
describeBy(data$MATE1_REG_ACTUAL, data$INGRESO_PERCAPITA_GRUPO_FA)
describeBy(data$MATE2_REG_ACTUAL, data$INGRESO_PERCAPITA_GRUPO_FA)
describeBy(data$HCSOC_REG_ACTUAL, data$INGRESO_PERCAPITA_GRUPO_FA)
describeBy(data$CIEN_REG_ACTUAL, data$INGRESO_PERCAPITA_GRUPO_FA)

# 4) La distribución de los puntajes de la prueba de matemáticas
# es sesgada a la derecha.
describe(data$MATE1_REG_ACTUAL)
describe(data$MATE2_REG_ACTUAL)
hist(data$MATE2_REG_ACTUAL)

# 5) El puntaje del NEM tiene mayor dispersión relativa en comparación
# al puntaje de las pruebas PAES.
cv_nem <- round(sd(data$PTJE_NEM, na.rm = T) / mean(data$PTJE_NEM, na.rm = T),3)*100
cv_clec <- round(sd(data$CLEC_REG_ACTUAL, na.rm = T) / mean(data$CLEC_REG_ACTUAL, na.rm = T),3)*100
cv_mate1 <- round(sd(data$MATE1_REG_ACTUAL, na.rm = T) / mean(data$MATE1_REG_ACTUAL, na.rm = T),3)*100
cv_mate2 <- round(sd(data$MATE2_REG_ACTUAL, na.rm = T) / mean(data$MATE2_REG_ACTUAL, na.rm = T),3)*100
cv_hsoc <- round(sd(data$HCSOC_REG_ACTUAL, na.rm = T) / mean(data$HCSOC_REG_ACTUAL, na.rm = T),3)*100
cv_cien <- round(sd(data$CIEN_REG_ACTUAL, na.rm = T) / mean(data$CIEN_REG_ACTUAL, na.rm = T),3)*100

cbind(NEM = cv_nem, CLEC = cv_clec, 
      MATE1 = cv_mate1, MATE2 = cv_mate2, 
      HSOC = cv_hsoc, CIEN = cv_cien)
