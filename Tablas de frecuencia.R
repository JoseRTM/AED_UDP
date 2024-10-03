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

# VERSIÓN DPLYR
data %>% 
  count(sexo) %>% 
  mutate(porcentaje = n/sum(n)*100)

# TABLAS DE CONTINGENCIA
table(data$sexo,data$nse)
table(data$sexo,data$nse) %>% prop.table(margin = 1)*100 # % por fila
table(data$sexo,data$nse) %>% prop.table(margin = 2)*100 # % por columna

# VERSIÓN JANITOR
# tabyl() PUEDE USARSE SI TENEMOS INSTALADO EL PAQUETE janitor
data %>%
  tabyl(sexo, nse) %>%
  adorn_percentages("col") %>% # % por columna
  adorn_totals("col") %>% # TOTALES POR COLUMNA
  adorn_pct_formatting(digits = 1) # N DECIMALES

