# GUÍA DE EJERCICIOS ESTADÍSTICA DESCRIPTIVA Y VISUALIZACIÓN
# Active las librerías que va a usar

# 1. Importe la base de datos jovenes.rds
# Es la base de datos de la Encuesta Nacional de la Juventud
# Encuesta poblacional realizada por INJUV
# La población representada son jóvenes de 15 a 29 años.


# 2. Guarde como objeto una base de datos reducida
# Debe contener las variables: sexo, edad, p76_1, nse, p81
# Hint: utilice []
# Hint: La p81 es una variable que mide el inicio de la actividad sexual
# Variable discreta.


# 3. De un vistazo general de la base de datos
# Hint: Puede usar str() o skim()

# 4. Limpie las variables de ser necesario

# 5. Etiquete las variables siguiendo la siguiente estructura:
# p76_1: Es binaria, mide si una persona declara haber consumido
# alcohol alguna vez en su vida. (1: SI, 2: NO)
# nse: Nivel socioeconómico. (1: Alto, 2: Medio, 3: Bajo)
# sexo (1: Hombre, 2: Mujer)

# 6. Realice un análisis descriptivo completo de las variables. Construya un relato
# breve comentando los resultados.
# Hint: Utilice describe() para cuantitativas
# Hint: Utilice table, tab1 o tabyl para las cualitativas

# 7. Caracterice la distribución de la variable p81
# utilizando medidas descriptivas y un gráfico de densidad
# Hint: Puede usar plot() y la funcion density()

# 8. Compare a través de un gráfico la diferencia en la edad de inicio
# entre quienes declaran haber consumido alcohol y las que no lo hicieron.
# Discuta los resultados. 

# 9. Compare a través de un gráfico la diferencia en la edad de inicio por sexo
# Discuta los resultados

# 10. Compare a través un gráfico la frecuencia de consumo de alcohol por sexo
# Discuta los resultados

# 10. Compare a través un gráfico el porcentaje de consumo de alcohol por sexo
# Discuta los resultados
# Hint: Utilice margins = 1 para obtener las proporciones por fila
# Discuta la diferencia entre este gráfico y el anterior.
