################################
#### Fusión BBDD de lugares ####
################################

#### Carga paquetes ####

library(tidyverse)
library(readxl)
library(tidygeocoder)
library(labelled)

#### Carga de bases de datos ####

lugares_hallazgos <- readRDS("C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares_hallazgos.rds")
lugares_detencion <- readRDS("C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares_detencion.rds")

#### Unión de bases de datos ####

lugares <- lugares_detencion %>%
  full_join(lugares_hallazgos)

#### Guardado de base de datos de lugares ####

openxlsx::write.xlsx(lugares, "C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares.xlsx")
write_rds(lugares, "C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares.rds")
