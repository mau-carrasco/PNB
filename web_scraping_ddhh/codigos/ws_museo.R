#### Obtención de listado de víctimas con link ####

#### Carga de paquetes ####
library(tidyverse)
library(rvest)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/OneDrive - Ministerio de Justicia/Datos PNB/web_scraping_ddhh/")

#### URL y páginas de expediente de la represión ####
paginas <- 1:17 #página con el listado de victimas
urls <- paste0("https://interactivos.museodelamemoria.cl/victimas/?s=+&cat=0&region=0&calificacion=0&militancia=0&year2=0&paged=", paginas)

#### Función que estrae información de varias páginas ####
get_web_data <- function(url) {
  webpage <- url %>% read_html()
  nombre_victima <- webpage %>% html_nodes("a") %>% html_text2() %>% trimws()
  link <- webpage %>% html_nodes("a") %>% html_attr('href')
  data.frame(nombre_victima, link)
}

#### Base de datos con el listado de causas ####
lista_victimas <- purrr::map_df(urls, get_web_data)

lista_victimas <- lista_victimas %>%
  filter(grepl("p=", link))

lista_victimas <- unique(lista_victimas)

#### URL de páginas con información a extraer de las víctimas ####
links <- paste0(lista_victimas$link)

links[1451] %>% read_html() %>% html_elements(".cate a") %>% html_text2()

#### Función que extrae información de las víctimas ####
get_victimas_data <- function(url) {
  webpage <- url %>% read_html()
  nombre <- webpage %>% html_elements(".contenidu h1")  %>% html_text()
  edad <- webpage %>% html_elements(".col:nth-child(4) .resp~ .resp") %>% html_text()
  ocupacion <- webpage %>% html_elements(".col:nth-child(5) .resp~ .resp") %>% html_text()
  militancia <- webpage %>% html_elements(".cate+ .col .resp:nth-child(2)") %>% html_text()
  fecha_desaparicion_muerte <- webpage %>% html_elements(".col:nth-child(4) .resp:nth-child(2)") %>% html_text()
  region <- webpage %>% html_elements(".cate+ .col .resp~ .resp") %>% html_text()
  ciudad <- webpage %>% html_elements(".col:nth-child(5) .resp:nth-child(2)") %>% html_text()
  comuna <- webpage %>% html_elements(".col:nth-child(6) .resp:nth-child(2)") %>% html_text()
  calificacion <- webpage %>% html_elements(".col:nth-child(6) .resp~ .resp") %>% html_text()
  tipo_caso <- webpage %>% html_elements(".cate a") %>% html_text()
  relato <- webpage %>% html_elements("#modal-ready") %>% html_text2()
  data.frame(nombre, edad, ocupacion, militancia, fecha_desaparicion_muerte, region, ciudad, comuna, calificacion, tipo_caso, relato)
}

#### Base de datos museo ####
base_museo <- purrr::map_df(links, get_victimas_data)

base_museo <- separate(base_museo, col=nombre, into=c('victima', 'eliminar'), sep=' «')

base_museo <- base_museo %>% select(!eliminar)

#### Limpieza de la base de datos ####
base_museo <- base_museo %>%
  mutate(
    calificacion = if_else(calificacion == "Sin calificación", "CVR violación de DDHH", calificacion)
  )

base_museo <- base_museo %>%
  mutate(
    comision = case_when(
      grepl("CVR", calificacion) ~ 1,
      grepl("CNRR", calificacion) ~ 2,
      TRUE ~ 3
      ),
    comision = factor(
      comision, labels = c("CVR", "CNRR", "CPACDDEPVPPT")
      ),
    calificacion = if_else(grepl("DDHH", calificacion), 1, 2),
    calificacion = factor(calificacion, labels = c("Violación de DDHH", "Violencia política"))
  )

base_museo <- base_museo %>%
  mutate(tipo_caso = if_else(tipo_caso == "Sin categoría", "Detenido Desaparecido", tipo_caso))

base_museo <- base_museo %>%select(1:8, 12, 9:11)

#### Guardado de base de datos del museo ####

openxlsx::write.xlsx(base_museo, "C:/Users/mauricio.carrasco/OneDrive - Ministerio de Justicia/Datos PNB/web_scraping_ddhh/resultados/base_museo.xlsx")
