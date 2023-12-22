################################
#### Fusión BBDD de lugares ####
################################

#### Carga paquetes ####

library(tidyverse)
library(readxl)
library(tidygeocoder)
library(labelled)

#### Carga de bases de datos ####

lugares_hallazgos <- readRDS("~/GitHub/PNB/lugares/resultados/lugares_hallazgos.rds")
lugares_detencion <- readRDS("~/GitHub/PNB/lugares/resultados/lugares_detencion.rds")
lugares_trabajos <- readRDS("~/GitHub/PNB/lugares/resultados/lugares_trabajos.rds")

lugares_hallazgos$tipo_lugar <- "Lugar de hallazgo"

#### Unión de bases de datos ####

lugares <- lugares_detencion %>%
  full_join(lugares_hallazgos) %>%
  full_join(lugares_trabajos)

# factorización de variables
lugares_trabajos <- read_excel("GitHub/PNB/lugares/datos/lugares_trabajos.xlsx")
lugares$tipo_ubicacion[lugares$tipo_lugar == "Trabajo en terreno"] <- lugares_trabajos$tipo_ubicacion

lugares$tipo_ubicacion <- factor(lugares$tipo_ubicacion)
lugares$tipo_lugar <- factor(lugares$tipo_lugar)

# etiquetas

var_label(lugares) <- list(
  codigo_pais = "Código país",
  nombre_pais = "Nombre país",
  codigo_region = "Código región",
  nombre_region = "Nombre región",
  codigo_provincia = "Código provincia",
  nombre_provincia = "Nombre provincia",
  codigo_comuna = "Código comuna",
  nombre_comuna = "Nombre comuna",
  codigo_lugar = "Código lugar",
  nombre_lugar = "Nombre lugar",
  tipo_lugar = "Tipo lugar",
  lat = "Ubicación: Coordenadas de latitud",
  long = "Ubicación: Coordenadas de longitud", 
  direccion = "Ubicación: Dirección lugar",
  tipo_ubicacion = "Tipo ubicación"
)

#### Guardado de base de datos de lugares ####

openxlsx::write.xlsx(lugares, "~/GitHub/PNB/lugares/resultados/lugares.xlsx")
write_rds(lugares, "~/GitHub/PNB/lugares/resultados/lugares.rds")
