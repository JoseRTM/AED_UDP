# Ley de los Grandes Números con un ejemplo de lanzamiento de moneda
# Explicación de la falacia del apostador

set.seed(123) # Fijamos la semilla para reproducibilidad
n_lanzamientos <- 10000 # Número de lanzamientos de la moneda

# Simulamos lanzamientos de una moneda justa (cara = 1, sello = 0)
lanzamientos <- rbinom(n_lanzamientos, 1, 0.5)

# Proporción acumulada de caras a medida que aumenta el número de lanzamientos
proporciones_acumuladas <- cumsum(lanzamientos) / (1:n_lanzamientos)

# Gráfico de la convergencia de la proporción de caras hacia 0.5
plot(proporciones_acumuladas, type="l", col="blue", lwd=2,
     main="Convergencia de la Proporción de Caras",
     xlab="Número de Lanzamientos", ylab="Proporción Acumulada de Caras")
abline(h=0.5, col="red", lwd=2, lty=2) # Línea de referencia en 0.5
legend("topright", legend=c("Proporción acumulada", "Valor esperado (0.5)"), col=c("blue", "red"), lty=c(1, 2), cex = 0.7)


#Demostracion Teorema del limite central

# Primero generamos una poblacion con numeros pseudoaleatorios
# Vamos a generar una poblacion de 100 mil personas
# a partir de una distribucion uniforme con el comando runif()
set.seed(1234)
poblacion<-runif(100000,70,100) # mu=85.01
summary(poblacion)
hist(poblacion) # Asi se ve una distribucion uniforme
# esta muy lejos de ser una distribucion normal

# Vamos a sacar 1000 veces una muestra de esa poblacion y calcularemos la media
# Para esto, vamos a crear tres vectores vacios
m1=c() # Esta muestra sera de 10 personas
m2=c() # Esta muestra sera de 50 personas
m3=c() # Esta muestra sera de 100 personas

# El comando for es un loop que hace repetidas veces lo que le pedimos
# En este caso, le estoy pidiendo que haga mil veces lo siguiente:
# Sacar la media de una muestra de la poblacion de 10 personas
# guardar la media en el vector m1
# Sacar la media de una muestra de la poblacion de 50 personas
# guardar la media en el vector m2
#Sacar la media de una muestra de la poblacion de 100 personas
# guardar la media en el vector m3

for (i in 1:1000){
  m1[i]=mean(sample(poblacion, 10, replace=T)) # Sacamos una muestra aleatoria de tamaño 10
  m2[i]=mean(sample(poblacion, 50, replace=T)) # Sacamos una muestra aleatoria de tamaño 50
  m3[i]=mean(sample(poblacion, 100, replace=T)) # Sacamos una muestra aleatoria de tamaño 100
}

# Ahora debemos graficar
par(mfrow=c(1,3))
hist(m1, col="orange", main="n=10", xlab="Peso")
abline(v=mean(m1), col="red")
hist(m2, col="lightblue", main="n=50", xlab="Peso")
abline(v=mean(m1), col="red")
hist(m3, col="lightgreen", main="n=100", xlab="Peso")
abline(v=mean(m1), col="red")

# Notemos la diferencia en la desviacion estandar
sd(m1)
sd(m2)
sd(m3)

# ¿Por que ocurre eso?


#DEMOSTRACIONES INTERVALOS DE CONFIANZA 
library(dplyr)
#CREAMOS LA POBLACION DE APRUEBO
set.seed(123)
apruebo<-rbinom(12000, 1, 0.6) # SIMULAMOS UNA POBLACION DONDE EL PORCENTAJE DE VOTOS POR EL APRUEBO ES DE
#  60%. ES DECIR, SI SACO UNA PERSONA AL AZAR, LA PROBABILIDAD DE SACAR UNA 
# PERSONA QUE VOTE APRUEBO ES DEL 60%.
head(apruebo) # CODIFICADA COMO 1 Y 0. ES DECIR, 1 SIGNIFICA QUE VOTA APRUEBO, 0 EN CASO CONTRARIO.
round(prop.table(table(apruebo)),2)#LA PROPORCION DE DIABETES EN LA POBLACION ES DE 58%
#SACAMOS UNA MUESTRA ALEATORIA DE 75 PERSONAS
set.seed(175)
muestra<-sample(apruebo, 75) # EL COMANDO SAMPLE SACA UNA MUESTRA ALEATORIA DE 75 PERSONAS. 

table(muestra)
prop.table(table(muestra))

mean(muestra) #LA PROPORCION DE APRUEBO EN LA MUESTRA ES DE 63%
# NOTA: LA MEDIA DE UNA VARIABLE CODIFICADA COMO 1 Y 0 ES IGUAL A LA PROPORCION. 
#  SI QUIEREN OBTENER EL PORCENTAJE HAY QUE MULTIPLICAR POR 100.


# LA IDEA ES CONSTRUIR UN INTERVALO DE CONFIANZA PARA LA PROPORCION (PORCENTAJE)
# PODEMOS VER QUE LA ESTIMACION (63%) ES RELATIVAMENTE DIFERENTE AL PARAMETRO (60%)
# VAMOS A VER SI AL HACER UN INTERVALO DE CONFIANZA, SE INCLUYE EL VALOR DEL PARAMETRO
# QUE EN ESTE CASO ES EL PORCENTAJE DE APRUEBO EN UNA ESCUELA QUE SE PRESUME TIENE 1200 PERSONAS. 

n = 75 # N DE LA MUESTRA
p = mean(muestra) # PROPORCION DE APRUEBO EN LA MUESTRA
q = 1-p # COMPLEMENTO DE LA PROPORCION
z = qnorm(1-(0.05)/2) # CUANTIL DE LA DISTRIBUCION NORMAL ESTANDAR (IC95%)
e = sqrt(p*q/n) # ERROR ESTANDAR, ES LA RAIZ CUADRADA DE LA VARIANZA DEL ESTIMADOR (PQ/N)

li = p-(z*e) 
ls = p+(z*e)
cbind(li,ls) # A UN 95% DE CONFIANZA, PODEMOS DE DECIR QUE EL PORCENTAJE DE APRUEBO 
# EN LA ESCUELA DE 12000 PERSONAS SE ENCUENTRA ENTRE EL 52% Y EL 74%. 
# NOTEMOS QUE ESTE INTERVALO INCLUYE AL PARAMETRO (60%)

# EN TEORIA, COMO ESTAMOS TRABAJANDO A UN 95% DE CONFIANZA, EL INTERVALO DEBERIA 
# CONTENER AL PARAMETRO 95 DE 100 VECES. 
# PARA PODER PROBAR ESTO, ES NECESARIO CREAR NUESTRA PROPIA FUNCION QUE CONSTRUYE
# INTERVALOS DE CONFIANZA
ci.p<-function(var,a){ # EL COMANDO FUNCTION DEBE INCLUIR EN PARENTESIS TODO LO QUE VARIA DENTRO DE LA FUNCION
  n=length(var) # AQUI EL N DEPENDE DEL NUMERO DE DATOS QUE TENGAMOS, POR ESO, PODEMOS GENERALIZAR EL CODIGO USANGO LENGTH()
  p=mean(var) # CALCULAMOS LA MEDIA DE LA VARIABLE 1 Y 0 PARA OBTENER LA PROPORCION
  q=1-p 
  e=qnorm(1-(a/2))*(sqrt(p*q/n))
  li=p-e
  ls=p+e
  cbind(li,ls)
}

ci.p(muestra,0.05) #PROBAMOS SI FUNCIONA

# AHORA VAMOS A PROBAR LA TEORIA, HAREMOS ESTE EXPERIMENTO 1000 VECES.
# ES DECIR, VAMOS A SACAR 1000 MUESTRAS DIFERENTES DE ESTA POBLACION
# Y A HACER 1000 INTERVALOS DE CONFIANZA

limites05=NULL
for (i in 1:1000){
  muestra<-sample(apruebo, 75)
  intervalo=ci.p(muestra,0.05)
  limites05=rbind(limites05,intervalo)
}

limites05 <- as.data.frame(limites05)
limites05$param <- ifelse(limites05$li<mean(apruebo)& limites05$ls>(mean(apruebo)), 1,0) # VAMOS A CREAR
# UNA VARIABLE QUE TOME EL VALOR 1 CUANDO EL PARAMETRO(8%) ESTE DENTRO
# DE LOS LIMITES DEL INTERVALO DE CONFIANZA.
prop.table(table(limites05$param)) # VEMOS QUE EL 94% DE LAS VECES 
# EL INTERVALO DE CONFIANZA INCLUYE AL PARAMETRO.

# EJERCICIO: REPRODUCIR ESTE CODIGO PARA LOS INTERVALOS DE CONFIANZA AL 90%
# Y AL 99%. 
# LLAMAR AL DF limites1 y limites01, respectivamente.
# REVISAR EL PORCENTAJE DE VECES QUE EL INTERVALO CONTIENE AL PARAMETRO
# ¿QUE OCURRE CON LA AMPLITUD DEL INTERVALO?

# FACTORES DE EXPANSIÓN

# LIBRERÍAS
library(survey)
library(haven)

# DATOS
data <- rio::import("https://github.com/JoseRTM/AED_UDP/raw/refs/heads/main/jovenes.rds")

# Explicación de la función svydesign:
# id = ~conglomerado: Definimos la primera del diseño.
# strata = ~estrato: Variable que identifica los estratos (combinación de región y zona).
# data = encuesta: El data frame con los datos de la encuesta.
# weights = ponderador.

# Creamos el diseño de encuesta polietápico con pesos combinados
survey <- svydesign(id = ~CONGLOMERADO, strata = ~ESTRATO, 
                    data = data, weights = ~PONDERADOR)
# GENERALMENTE, LAS VARIABLES QUE DEBEMOS INCLUIR EN svydesign 
# SON CLARAMENTE RECONOCIBLES EN LA BBDD

##################
# ESTIMAR MEDIAS #
##################

# Calculemos la media del ingreso con y sin ponderación
mean(data$P82, na.rm = T)
# svymean no permite argumentos como na.rm = T
survey_clean <- subset(survey, !is.na(P82))
media_parejas <- svymean(~data_clean$P82, design = survey_clean)

# INTERVALOS DE CONFIANZA
confint(media_parejas)

# PROPORCIONES

table(data$SEXO) %>% prop.table()
svytable(~data$SEXO, design = survey) %>% prop.table()
props <- svymean(~factor(SEXO), design = survey)
confint(props)
