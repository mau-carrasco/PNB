#############################################################################
####### Base de datos de víctimas de asesinato y desaparición forzada #######
#############################################################################

#### Paquetes ####
library(tidyverse)
library(readxl)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/Desktop/PNB/victimas")

#### Fuentes de información ####
# Base de datos del Programa de Derechos Humanos - PDH
victimas_pdh <- read_excel("datos/20230907-Informe-Víctimas.xlsx")

# Base de datos de Víctimas de Desaparición forzada - U. Inv. Administrativa
victimas_df <- read_excel("datos/Víctimas-Desaparición-Forzada.xlsx")

# Vase de datos del Museo de la Memoria y los DDHH
victimas_museo <- read_excel("C:/Users/mauricio.carrasco/Desktop/PNB/web_scraping_ddhh/resultados/base_museo.xlsx")

#### Limpieza de bases de datos ####

victimas_pdh <- victimas_pdh %>%
  select(id,
         victima = Víctima,
         rut = 3,
         fecha_nacimiento = 4,
         edad = 5,
         sexo = 6,
         nacionalidad = Nacionalidad,
         actividad = Actividad,
         tipo_actividad = Categoria_Actividad,
         militancia = Militancia,
         cargo = Cargo_Publico_Politico) %>%
  mutate(nacionalidad = replace_na(nacionalidad, "CHILENA"),
         nacionalidad = case_when(nacionalidad == "CHILENO" ~ "CHILENA",
                           TRUE ~ nacionalidad),
         sexo = factor(sexo),
         tipo_actividad = factor(tipo_actividad))


