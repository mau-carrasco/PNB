#### Preámbulo ####

# Directorio de trabajo
setwd("C:/Users/mauricio.carrasco/Desktop/criminales/")

# Carga de paquetes
library(tidyverse)
library(readxl)
library(kableExtra)

# Carga de datos del PDH
agentes <- read_excel("20240104-Informe-historial-procesal-Agente.xlsx")
causas <- read_excel("20240105-Informe-PJP.xlsx")


#### Procesamiento de datos ####

# Limpieza de base de datos de causas
rol_causas <- causas %>%
  filter(EstadoProc == "Sentencia condenatoria ejecut.") %>%
  group_by(Rol) %>%
  summarise(n = n()) %>%
  ungroup()

# Limpieza de base de datos de criminales
agentes <- agentes %>%
  select(Agente, Rama_Perteneciente, Rol, Estado_Procesal, Fecha, Delito, Condena) %>%
  filter(Estado_Procesal %in% c("SENTENCIA 1° INST", "SENTENCIA 2° INST", "FONDO", "FORMA")) %>%
  group_by(Agente, Rol) %>%
  summarise(Rama_Perteneciente = last(Rama_Perteneciente),
            Estado_Procesal = last(Estado_Procesal),
            Fecha = last(Fecha),
            Delito = last(Delito),
            Condena = last(Condena)) %>%
  ungroup()
agentes <- agentes %>%
  filter(Rol %in% c(paste0(rol_causas$Rol))) %>%
  filter(grepl("Año|año|día|dia", Condena))

n_distinct(agentes$Agente) # 923 agentes condenados
n_distinct(agentes$Rol) # 513 sentencias definitivas

# Creación de excel con datos limpios
openxlsx::write.xlsx(agentes, "criminales_condenados.xlsx")

agentes <- agentes %>%
  mutate(
    Homicidio = if_else(grepl("Homicidio|homicidio", Delito), 1, 0),
    Secuestro = if_else(grepl("Secuestro|secuestro", Delito), 1, 0),
    Tortura = if_else(grepl("tormentos|Apremios|apremios|Artículo 150 N°1 C.P.|Artículo N° 150 C.P.|Tortura|tortura|Torturas|torturas", Delito), 1, 0),
    Asociacion_ilicita = if_else(grepl("Asociación ilícita|asoc. ilicita", Delito), 1, 0),
    Otros = if_else(grepl("Exhumación Ilegal|iolencia innecesaria|iolencias innecesarias|Sustracción de menores|otros|Robo|Lesiones|lesiones|control de armas|Falso testimonio en causa criminal|Falsificación de instrumento público|etencion ilegal", Delito), 1, 0)
         )

#### Análisis de datos ####

# Tabla 1: Criminales por institución de pertenencia
agentes %>%
  group_by(Institucion = Rama_Perteneciente) %>%
  summarise(n = n_distinct(Agente)) %>%
  mutate("%" = n/sum(n)*100) %>%
  janitor::adorn_totals("row") %>%
  kableExtra::kbl(digits = 2, 
                  caption = "Número de criminales condenados por delitos de lesa humanidad, según institución de pertenencia") %>%
  kableExtra::kable_classic_2() %>%
  kableExtra::footnote(general = "Programa de Derechos Humanos - Subsecretaría de Derechos Humanos",
                       general_title = "Fuente:")

# Tabla 2: Criminales por delito
agentes %>%
  group_by(Agente) %>%
  summarise("Total condenas" = n_distinct(Rol),
            Homicidio = n_distinct(Rol[Homicidio==1]),
            Secuestro = n_distinct(Rol[Secuestro==1]),
            Tortura = n_distinct(Rol[Tortura==1]),
            "Asoc. ilíc." = n_distinct(Rol[Asociacion_ilicita==1]),
            Otros = n_distinct(Rol[Otros==1])) %>%
  arrange(desc(`Total condenas`)) %>%
  head(10) %>%
  kbl(booktabs = T,
      caption = "Ranking: Los 10 criminales de lesa humanidad con mayor número de condenas") %>%
  add_header_above(c(" " = 2, "Delitos" = 5)) %>%
  kable_classic_2() %>%
  footnote(general = "Programa de Derechos Humanos - Subsecretaría de Derechos Humanos",
           general_title = "Fuente:",
           number = c("Una persona puede ser condenada en la misma causa por más de un delito (e.g. Secuestro y homicidio calificado), por lo que el total de condenas no corresponde a la suma de los delitos mencionados en los veredictos de culpabilidad",
                      "La categoría Otros agrupa los delitos de sustracción de menores, robo con intimidación, violencia innecesaria, lesiones graves o gravísimas, falso testimonio en causa criminal y falsificación de instrumento público"),
           number_title = "Notas:")


# Figura 1: Fecha de emisión de las setencias definitivas
par(mfrow = c(1, 2))
agentes %>%
  mutate(Anno = lubridate::year(Fecha)) %>%
  filter(Anno > 1989) %>%
  group_by(Rol) %>%
  summarise(Anno = last(Anno)) %>%
  ungroup() %>%
  count(Anno) %>%
  plot(type = "h",
       main = "Sentencias condenatorias por año",
       xlab = "Fecha", ylab = "N° de sentencias condenatorias")
agentes %>%
  mutate(Anno = lubridate::year(Fecha)) %>%
  filter(Anno > 1989) %>%
  group_by(Rol) %>%
  summarise(Anno = last(Anno)) %>%
  ungroup() %>%
  count(Anno) %>%
  mutate(Acum = cumsum(n)) %>%
  select(!n) %>%
  plot(type = "h",
       main = "Sentencias condenatorias acumuladas",
       xlab = "Fecha", ylab = "N° de sentencias condenatorias acumuladas")
par(mfrow = c(1, 1))

# Figura 2
par(mfrow = c(1, 2))
causas %>%
  drop_na(Fecha_ini) %>%
  mutate(Anno = lubridate::year(Fecha_ini)) %>%
  filter(Anno > 1989) %>%
  group_by(Anno) %>%
  summarise(n = n_distinct(Rol)) %>%
  plot(type = "h",
       main = "Interposición de querellas por año",
       xlab = "Fecha", ylab = "N° de querellas interpuestas")
agentes %>%
  mutate(Anno = lubridate::year(Fecha)) %>%
  filter(Anno > 1989) %>%
  group_by(Rol) %>%
  summarise(Anno = last(Anno)) %>%
  ungroup() %>%
  count(Anno) %>%
  plot(type = "h",
       main = "Sentencias condenatorias por año",
       xlab = "Fecha", ylab = "N° de sentencias condenatorias")
par(mfrow = c(1, 1))

duracion_causas <- causas %>%
  filter(Rol %in% c(paste0(rol_causas$Rol))) %>%
  mutate(Anno_inicio = lubridate::year(Fecha_ini),
         Anno_cierre = lubridate::year(Fecha_EstProc)) %>%
  group_by(Rol) %>%
  summarise(Anno_inicio = min(Anno_inicio), Anno_cierre = max(Anno_cierre)) %>%
  drop_na() %>%
  filter(Anno_inicio>1989) %>%
  mutate(Duracion = Anno_cierre - Anno_inicio) %>%
  ungroup()

hist(duracion_causas$Duracion,
     xlab = "Años de duración",
     ylab = "Número de causas",
     main = "")

psych::describe(duracion_causas$Duracion)
