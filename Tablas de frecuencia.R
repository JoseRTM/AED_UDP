# TABLAS DE FRECUENCIA

# ACTIVAR LIBRERÍAS
library(dplyr)
library(epiDisplay)
library(haven)
library(janitor)

# CARGAR DATOS
data <- read_sav("casen_red.sav")

# TABLA DE FRECUENCIA
# R BASE
head(unique(data$nse))
head(unique(data$sexo))

table(data$nse)
table(data$sexo)

# EN R se pueden anidar funciones, por ejemplo

sqrt(log(100))
# aqui estamos sacando la raiz del logaritmo de 100
# esto es equivalente

100 %>% log() %>% sqrt()

# esto ayuda a entender mejor el código, lo ordena

# PROPORCIONES
prop.table(table(data$nse))
table(data$sexo) %>% prop.table()

# PORCENTAJE
table(data$sexo) %>% prop.table()*100
table(data$nse) %>% prop.table()*100

# FUNCIÓN TAB1
# NOTA: tab1() proviene de la librería epiDisplay
tab1(data$sexo)
tab1(data$nse)

# TABLAS DE CONTINGENCIA
tabla_plana <- table(data$sexo,data$nse)
tabla_fila <- table(data$sexo,data$nse) %>% prop.table(margin = 1)*100 # % por fila
table(data$sexo,data$nse) %>% prop.table(margin = 2)*100 # % por columna
# ESTO FUNCIONA
(table(data$sexo, data$nse) %>% prop.table(margin = 1)*100) %>% round(1)
# ESTO NO FUNCIONA
tabla_round <- table(data$sexo, data$nse) %>% prop.table(margin = 1)*100 %>% round(1)
tabla_round
# se guarda redondeado, pero se imprime sin redondear.
# esto ocurre porque primero se imprime en la consola y luego lo redondea.
# si se aplican parentesis, se imprime correctamente 

# VERSIÓN JANITOR
# tabyl() PUEDE USARSE SI TENEMOS INSTALADO EL PAQUETE janitor
data %>%
  tabyl(sexo, nse) %>%
  adorn_percentages("col") %>% # % por columna
  adorn_totals("row") %>% # TOTALES POR COLUMNA
  adorn_pct_formatting(digits = 1) # N DECIMALES

