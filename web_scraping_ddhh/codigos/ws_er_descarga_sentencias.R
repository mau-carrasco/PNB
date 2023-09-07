#### Carga de paquetes ####
library(tidyverse)
library(rvest)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/Desktop/PNB/web_scraping_ddhh/")

#### URL y páginas de expediente de la represión ####
paginas <- 1:41 #página con el listado de expedientes judiciales
urls <- paste0("https://expedientesdelarepresion.cl/base-de-datos/?sf_paged=", paginas)

#### Función que estrae información de varias páginas ####
get_web_data <- function(url) {
  webpage <- url %>% read_html()
  nombre_causa <- webpage %>% html_nodes('h2 a') %>%html_text() %>% trimws()
  link <- webpage %>% html_nodes('h2 a') %>% html_attr('href')
  data.frame(nombre_causa, link)
}

#### Base de datos con el listado de causas ####
lista_causas <- purrr::map_df(urls, get_web_data)

#### URL de páginas con información a extraer ####
links <- paste0(lista_causas$link)

#### Función que extrae información de las causas ####
get_data_causas <- function(url) {
  webpage <- url %>% read_html()
  nombre_causa <- webpage %>% html_elements(".breadcrumb_last") %>% html_text() %>% trimws()
  sentencia_pi <- webpage %>% html_elements("h2+ .link_sentencias") %>% html_attr('href')
  sentencia_ca <- webpage %>% html_elements(".link_sentencias:nth-child(3)") %>% html_attr('href')
  sentencia_cs <- webpage %>% html_elements(".link_sentencias~ .link_sentencias+ .link_sentencias") %>% html_attr('href')
  if (purrr::is_empty(sentencia_cs)){sentencia_cs=NA}
  data.frame(nombre_causa, sentencia_pi, sentencia_ca, sentencia_cs)
}

#### Base de datos con información de las sentencias ####
info_condenas <- purrr::map_df(links, get_data_causas)

#### Descarga de sentencias ####
info_condenas$nombre_causa <- gsub("[^[:alnum:]]", " ", info_condenas$nombre_causa)

#### Sentencias de primera instancia ####
Map(function(x, y) {
  download.file(url = x,
                paste0("C:/Users/mauricio.carrasco/Desktop/PNB/web_scraping_ddhh/documentos/sentencias_pi/",y,".pdf"),
                mode="wb")
}, info_condenas$sentencia_pi, info_condenas$nombre_causa)

#### Sentencias de la Corte de Apelaciones ####
Map(function(x, y) {
  download.file(url = x,
                paste0("C:/Users/mauricio.carrasco/Desktop/PNB/web_scraping_ddhh/documentos/sentencias_ca/",y,".pdf"),
                mode="wb")
}, info_condenas$sentencia_ca, info_condenas$nombre_causa)

#### Sentencias de la Corte Suprema ####
info_condenas <- info_condenas %>%
  mutate(sentencia_cs = na_if(sentencia_cs, "#")) %>%
  drop_na()

Map(function(x, y) {
  download.file(url = x,
                paste0("C:/Users/mauricio.carrasco/Desktop/PNB/web_scraping_ddhh/documentos/sentencias_cs/",y,".pdf"),
                mode="wb")
}, info_condenas$sentencia_cs, info_condenas$nombre_causa)
