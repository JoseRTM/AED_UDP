
#-------------------# 
# TEST DE HIPÓTESIS #
#-------------------#

# librerías
library(rio)
library(psych)
library(survey) # Se agrega el paquete survey para análisis ponderados

#------------------------------#
# PRUEBA Z PARA UNA PROPORCION #
#------------------------------#
# En los 2000 al 30% le gustaba el centella, en una muestra de 50 personas,
# a 22 de ellas le gustaba el centella. ¿Es significativa esta diferencia?
prop.test(22,50,p=.3, alternative="greater")
# H0: P <=0.3
# H1: p > 0.3

# el test también nos proporciona el intervalo de confianza
# noten que va desde el .32 hasta 1
# eso es porque está calculando el intervalo a 1 cola.
prop.test(22,50,p=.3, alternative="two.sided")
# H0: P = 0.3
# H1: P != 0.3


#------------------------------#
# PRUEBA Z PARA 2 PROPORCIONES #
#------------------------------#
# UN ESTUDIO CONTROLADO ALEATORIZADO
# CON 400 PACIENTES ONCOLÓGICOS INTENTABA 
# TESTEAR EL DESARROLLO DE UN FÁRMACO PARA LAS NÁUSEAS
# 88 DEL GRUPO CONTROL PRESENTARON NAUSEAS POST TRATAMIENTO
# EN EL GRUPO EXPERIMENTAL, 80 PRESENTARON NAUSEAS
# ES ESTADÍSTICAMENTE SIGNIFICATIVA ESTA DIFERENCIA?
datos <- matrix(c(88,112,80,120), ncol = 2, byrow = TRUE)
rownames(datos) <- c("ctrl","exp")
colnames(datos) <- c("nauseas","Sin nauseas")
datos <- as.table(datos)
datos
prop.test(datos, alternative = "greater", correct = T) # dos colas
prop.test(datos, alternative = "less", correct = T) # cola inferior
prop.test(datos,alternative = "greater", correct = T) # cola superior

# PARA ENCONTRAR EL Z DEBEMOS SACAR LA RAIZ CUADRADA DE X-SQUARED
z <- sqrt(0.50287);z

# Versión ponderada utilizando svychisq (test ponderado)
svychisq(~var1 + var2, design = survey_design)


#---------------------------#
# PRUEBA T PARA UNA MUESTRA #
#---------------------------#
data <- import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/mineduc_paes.rds")
View(data)

# ESTE COMANDO ME DA UN INTERVALO DE CONFIANZA PARA LA MEDIA
t.test(data$PTJE_NEM, mu = 500)

# Versión ponderada usando svyttest para una muestra
svyttest(variable ~ 1, design = survey_design)


#---------------------------#
# PRUEBA T PARA DOS MUESTRAS #
#---------------------------#
# H0: MU1 = MU2
# H1: MU1 != MU2
t.test(data$PTJE_NEM~data$SEXO)

# Versión ponderada usando svyttest para dos muestras
svyttest(variable_cuanti ~ variable_cuali, design = survey_design)


#---------------------------#
# ANOVA #
#---------------------------#
# H0: MU1 = MU2 = MU3 = MU_K 
# H1: HAY AL MENOS UN PAR DE MU DISTINTAS

anova <- aov(PTJE_NEM~factor(INGRESO_PERCAPITA_GRUPO_FA),data=data)
summary(anova)

# comparaciones múltiples
tukey <- TukeyHSD(anova)
plot(tukey)

# Versión ponderada usando svyglm (ANOVA ponderada)
anova_ponderada <- svyglm(var1 ~ var2, design = survey_design)
summary(anova_ponderada)

#---------------#
# CORRELACIONES #
#---------------#
# H0: r = 0
# H1: r != 0
# SE USA ENTRE DOS VARIABLES CUANTITATIVAS
cor.test(data$MATE1_REG_ACTUAL,data$MATE2_REG_ACTUAL, use = "complete.obs")

# matriz de correlación

corPlot(data[,2:8])

# Versión ponderada para correlaciones usando svyvar
svyvar(~var1 + var2, design = survey_design)

