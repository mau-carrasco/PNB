#############################################################################
####### Base de datos de víctimas de asesinato y desaparición forzada #######
#############################################################################

#### Paquetes ####
library(tidyverse)
library(readxl)
library(labelled)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/Desktop/PNB/victimas/")

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

#### Unión de bases de datos ####
victimas <- victimas_pdh %>%
  left_join(victimas_museo %>%
              select(id, fecha_desaparicion_muerte, region, ciudad, comuna, comision, calificacion, relato)) %>%
  left_join(victimas_df %>%
              select(id, tipo_caso = TIPO, identificacion = IDENTIFICACIÓN_REC, identificacion_det = IDENTIFICACIÓN)) %>%
  mutate(tipo_caso = case_when(tipo_caso %in% c("DETENIDA DESAPARECIDA", "DETENIDO DESAPARECIDO") ~ 1,
                               tipo_caso %in% c("EJECUTADA POLITICA S/E CUERPO", "EJECUTADO POLITICO S/E CUERPO") ~ 2,
                               TRUE ~ 3),
         tipo_caso = factor(tipo_caso, labels = c("DESAPARECIDO/A", "ASESINADO/A SIN ENTREGA DE CUERPO", "ASESINADO/A")))

victimas <- victimas %>%
  mutate(fecha_nacimiento = as.Date(fecha_nacimiento),
         fecha_desaparicion_muerte = as.Date(fecha_desaparicion_muerte),
         comision = case_when(comision == "CVR" ~ 1,
                              comision == "CNRR" ~ 2,
                              TRUE ~ 3),
         comision = factor(comision, labels = c("CVR", "CNRR", "CPACDDEPVPPT")),
         calificacion = factor(calificacion),
         identificacion = factor(identificacion),
         nacionalidad = factor(nacionalidad),
         militancia = factor(militancia),
         cargo = factor(cargo),
         region = factor(region),
         ciudad = factor(ciudad),
         comuna = factor(comuna))

victimas <- victimas %>%
  select(1:17, 19:21, 18)

#### Factorización regiones ####

victimas <- victimas %>%
  mutate(
    region = case_when(
      ciudad == "ARICA" ~ "XV Arica-Parinacota",
      ciudad == "PUTRE" ~ "XV Arica-Parinacota",
      ciudad == "VALDIVIA" ~ "XIV Los Ríos",
      ciudad == "PANGUIPULLI" ~ "XIV Los Ríos",
      ciudad == "RIO BUENO" ~ "XIV Los Ríos",
      ciudad == "LA UNION" ~ "XIV Los Ríos",
      ciudad == "LIQUIÑE" ~ "XIV Los Ríos",
      ciudad == "NELTUME" ~ "XIV Los Ríos",
      ciudad == "CHILLAN" ~ "XVI ÑUBLE",
      ciudad == "CHILLÁN" ~ "XVI ÑUBLE",
      ciudad == "COBQUECURA" ~ "XVI ÑUBLE",
      ciudad == "COELEMU" ~ "XVI ÑUBLE",
      ciudad == "COIHUECO" ~ "XVI ÑUBLE",
      comuna == "CHILLAN" ~ "XVI ÑUBLE",
      comuna == "PEMUCO" ~ "XVI ÑUBLE",
      comuna == "COIHUECO" ~ "XVI ÑUBLE",
      comuna == "BULNES" ~ "XVI ÑUBLE",
      comuna == "COBQUECURA" ~ "XVI ÑUBLE",
      ciudad == "NINHUE" ~ "XVI ÑUBLE",
      ciudad == "ÑUBLE" ~ "XVI ÑUBLE",
      ciudad == "SAN CARLOS" ~ "XVI ÑUBLE",
      ciudad == "SAN NICOLAS" ~ "XVI ÑUBLE",
      region == "XIV Arica-Parinacota" ~ "XV Arica-Parinacota",
      region == "XV Los Lagos" ~ "XIV Los Ríos",
      region == "RM METROPOLITANA" ~ "RM Metropolitana",
      region == "-" ~ "Extranjero",
      comuna == "PANGUIPULLI" ~ "XIV Los Ríos",
      comuna == "LAGO RANCO" ~ "XIV Los Ríos",
      comuna == "RIO BUENO" ~ "XIV Los Ríos",
      comuna == "S.J.DE LA MARIQUINA" ~ "XIV Los Ríos",
      comuna == "VALDIVIA" ~ "XIV Los Ríos",
      comuna == "ZONA FRONTERIZA DE PAIMUN" ~ "XIV Los Ríos",
      TRUE ~ region
    )
  ) %>%
  mutate(
    region = case_when(
      region == "Extranjero" ~ 0,
      region == "XV Arica-Parinacota" ~ 1,
      region == "I Tarapacá" ~ 2,
      region == "II Antofagasta" ~ 3,
      region == "III Atacama" ~ 4,
      region == "IV Coquimbo" ~ 5,
      region == "V Valparaíso" ~ 6,
      region == "RM Metropolitana" ~ 7,
      region == "VI O`Higgins" ~ 8,
      region == "VII Maule" ~ 9,
      region == "XVI ÑUBLE" ~ 10,
      region == "VIII Biobío" ~ 11,
      region == "IX Araucanía" ~ 12,
      region == "XIV Los Ríos" ~ 13,
      region == "X Los Lagos" ~ 14,
      region == "XI Aysén" ~ 15,
      region == "XII Magallanes" ~ 16
    )
  ) %>%
  mutate(
    region = factor(region,
                    labels = c("Extranjero",
                               "Arica y Parinacota",
                               "Tarapacá",
                               "Antofagasta",
                               "Atacama",
                               "Coquimbo",
                               "Valparaíso",
                               "Metropolitana",
                               "O`Higgins",
                               "Maule",
                               "Ñuble",
                               "Biobío",
                               "Araucanía",
                               "Los Ríos",
                               "Los Lagos",
                               "Aysén",
                               "Magallanes"))
  )

#### Etiquetas de variables ####
var_label(victimas) <- list(
  id = "Código único de víctima",
  victima = "Nombre completo",
  rut = "RUN",
  fecha_nacimiento = "Fecha de nacimiento",
  edad = "Años de edad al momento del secuestro o asesinato",
  sexo = "Sexo registral",
  nacionalidad = "Nacionalidad",
  actividad = "Actividad principal",
  tipo_actividad = "Tipo de actividad principal",
  militancia = "Partido o movimiento político",
  cargo = "Cargo público y/o político",
  fecha_desaparicion_muerte = "Fecha de desaparición o muerte",
  region = "Región donde ocurrió el secuestro o asesinato",
  ciudad = "Ciudad donde ocurrió el secuestro o asesinato",
  comuna = "Comuna donde ocurrió el secuestro o asesinato",
  comision = "Comisión de la verdad que califica a la víctima",
  calificacion = "Calificación",
  tipo_caso = "Tipo de caso",
  identificacion = "¿Víctima identificada?",
  identificacion_det = "Detalle de la identificación",
  relato = "Relato del caso"
)

libro_codigos <- look_for(victimas) %>% select(!value_labels)

#### Guardado de base de datos ####
openxlsx::write.xlsx(victimas, "C:/Users/mauricio.carrasco/Desktop/PNB/victimas/resultados/victimas.xlsx")
write_rds(victimas, "C:/Users/mauricio.carrasco/Desktop/PNB/victimas/resultados/victimas.rds")
openxlsx::write.xlsx(libro_codigos, "C:/Users/mauricio.carrasco/Desktop/PNB/victimas/resultados/libro_codigos.xlsx")
