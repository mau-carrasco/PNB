#### Obtención de listado de sitios con link ####

#### Carga de paquetes ####
library(tidyverse)
library(rvest)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/OneDrive - Ministerio de Justicia/Datos PNB/")

#### Región de Tarapacá (incluye Arica y Parinacota) ####

# Carga de la url con el lista de sitios de la región
url_tarapaca <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/i-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_tarapaca <- read_html(url_tarapaca)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_tarapaca <- data.frame(sitios = web_tarapaca %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
           link = web_tarapaca %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Limpiesa de datos de los nombres de los sitios de la región
sitios_tarapaca$sitios <- gsub("[^[:alnum:]]", " ", sitios_tarapaca$sitios)

# Lista de links con información de los sitios de la región
links <- sitios_tarapaca$link

#Función que extrae la ubicación de los sitios consignados en la página de la región
get_web_data <- function(url) {
  webpage <- url %>% read_html() # Lectura de la página de cada sitio en la región
  sitio <- webpage %>% html_nodes('.page-title') %>%html_text() %>% trimws() # Nombre completo del sitio
  coordenadas <- webpage %>% html_nodes("p a") %>% html_attr("href") # Link con las coordenadas del sitio
  if (purrr::is_empty(coordenadas)){coordenadas=NA} # Reemplaza los sitios que no tienen link de coordenadas por valores NA
  data.frame(sitio, coordenadas) # Devuelve un set de datos con el nombre y el link de coordenadas de cada sitio
}

sitios_tarapaca <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_tarapaca$region <- "Región de Tarapacá" # Asignación del nombre de la región
sitios_tarapaca <- sitios_tarapaca %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_tarapaca <- sitios_tarapaca %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2


#### Región de Antofagasta ####

# Carga de la url con el lista de sitios de la región
url_antofagasta <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/ii-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_antofagasta <- read_html(url_antofagasta)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_antofagasta <- data.frame(sitios = web_antofagasta %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_antofagasta %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Limpiesa de datos de los nombres de los sitios de la región
sitios_antofagasta$sitios <- gsub("[^[:alnum:]]", " ", sitios_antofagasta$sitios)

# Lista de links con información de los sitios de la región
links <- sitios_antofagasta$link

sitios_antofagasta <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_antofagasta$region <- "Región de Antofagasta" # Asignación del nombre de la región
sitios_antofagasta <- sitios_antofagasta %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_antofagasta <- sitios_antofagasta %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_antofagasta <- sitios_antofagasta %>% unique() # Borrar repetidos


#### Región de Atacama ####

# Carga de la url con el lista de sitios de la región
url_atacama <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/iii-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_atacama <- read_html(url_atacama)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_atacama <- data.frame(sitios = web_atacama %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                                 link = web_atacama %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_atacama$link

sitios_atacama <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_atacama$region <- "Región de Atacama" # Asignación del nombre de la región
sitios_atacama <- sitios_atacama %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_atacama <- sitios_atacama %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_atacama <- sitios_atacama %>% unique() # Borrar repetidos


#### Región de Coquimbo ####

# Carga de la url con el lista de sitios de la región
url_coquimbo <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/iv-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_coquimbo <- read_html(url_coquimbo)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_coquimbo <- data.frame(sitios = web_coquimbo %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                             link = web_coquimbo %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_coquimbo$link

sitios_coquimbo <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_coquimbo$region <- "Región de Atacama" # Asignación del nombre de la región
sitios_coquimbo <- sitios_coquimbo %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_coquimbo <- sitios_coquimbo %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_coquimbo <- sitios_coquimbo %>% unique() # Borrar repetidos


#### Región de Vaparaíso ####

# Carga de la url con el lista de sitios de la región
url_valparaiso <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/v-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_valparaiso <- read_html(url_valparaiso)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_valparaiso <- data.frame(sitios = web_valparaiso %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_valparaiso %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_valparaiso$link

sitios_valparaiso$sitios <- gsub("[^[:alnum:]]", " ", sitios_valparaiso$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/valpo/",y,".html"))
}, sitios_valparaiso$link, sitios_valparaiso$sitios)

links <- list.files("webs/valpo", full.names = T)

sitios_valparaiso <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_valparaiso <- sitios_valparaiso %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))


sitios_valparaiso$region <- "Región de Valparaíso" # Asignación del nombre de la región
sitios_valparaiso <- sitios_valparaiso %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_valparaiso <- sitios_valparaiso %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_valparaiso <- sitios_valparaiso %>% unique() # Borrar repetidos


#### Región Metropolitana ####

# Carga de la url con el lista de sitios de la región
url_metropolitana <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/metropolitana/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_metropolitana <- read_html(url_metropolitana)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_metropolitana <- data.frame(sitios = web_metropolitana %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_metropolitana %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_metropolitana$link

sitios_metropolitana$sitios <- gsub("[^[:alnum:]]", " ", sitios_metropolitana$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/metropolitana/",y,".html"))
}, sitios_metropolitana$link[185:212], sitios_metropolitana$sitios[185:212])

links <- list.files("webs/metropolitana", full.names = T)

sitios_metropolitana <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_metropolitana <- sitios_metropolitana %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_metropolitana$region <- "Región de Metropolitana" # Asignación del nombre de la región
sitios_metropolitana <- sitios_metropolitana %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_metropolitana <- sitios_metropolitana %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_metropolitana <- sitios_metropolitana %>% unique() # Borrar repetidos


#### Región de Ohiggins ####

# Carga de la url con el lista de sitios de la región
url_ohiggins <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/vi-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_ohiggins <- read_html(url_ohiggins)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_ohiggins <- data.frame(sitios = web_ohiggins %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_ohiggins %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_ohiggins$link

sitios_ohiggins$sitios <- gsub("[^[:alnum:]]", " ", sitios_ohiggins$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/ohiggins/",y,".html"))
}, sitios_ohiggins$link[49:60], sitios_ohiggins$sitios[49:60])

links <- list.files("webs/ohiggins", full.names = T)

sitios_ohiggins <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_ohiggins$region <- "Región de Ohiggins" # Asignación del nombre de la región
sitios_ohiggins <- sitios_ohiggins %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_ohiggins <- sitios_ohiggins %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_ohiggins <- sitios_ohiggins %>% unique() # Borrar repetidos


#### Región del Maule ####

# Carga de la url con el lista de sitios de la región
url_maule <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/vii-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_maule <- read_html(url_maule)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_maule <- data.frame(sitios = web_maule %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_maule %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_maule$link

sitios_maule$sitios <- gsub("[^[:alnum:]]", " ", sitios_maule$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/maule/",y,".html"))
}, sitios_maule$link[59:75], sitios_maule$sitios[59:75])

links <- list.files("webs/maule", full.names = T)

sitios_maule <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_maule <- sitios_maule %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_maule$region <- "Región del Maule" # Asignación del nombre de la región
sitios_maule <- sitios_maule %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_maule <- sitios_maule %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_maule <- sitios_maule %>% unique() # Borrar repetidos


#### Región del Biobío ####

# Carga de la url con el lista de sitios de la región
url_biobio <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/viii-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_biobio <- read_html(url_biobio)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_biobio <- data.frame(sitios = web_biobio %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                           link = web_biobio %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_biobio$link

sitios_biobio$sitios <- gsub("[^[:alnum:]]", " ", sitios_biobio$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/biobio/",y,".html"))
}, sitios_biobio$link[133:137], sitios_biobio$sitios[133:137])

links <- list.files("webs/biobio", full.names = T)

sitios_biobio <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_biobio <- sitios_biobio %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_biobio$region <- "Región del Biobío" # Asignación del nombre de la región
sitios_biobio <- sitios_biobio %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_biobio <- sitios_biobio %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_biobio <- sitios_biobio %>% unique() # Borrar repetidos


#### Región de la Araucanía ####

# Carga de la url con el lista de sitios de la región
url_araucania <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/ix-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_araucania <- read_html(url_araucania)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_araucania <- data.frame(sitios = web_araucania %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                            link = web_araucania %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_araucania$link

sitios_araucania$sitios <- gsub("[^[:alnum:]]", " ", sitios_araucania$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/araucania/",y,".html"))
}, sitios_araucania$link[70:75], sitios_araucania$sitios[70:75])

links <- list.files("webs/araucania", full.names = T)

sitios_araucania <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_araucania <- sitios_araucania %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_araucania$region <- "Región del Araucanía" # Asignación del nombre de la región
sitios_araucania <- sitios_araucania %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_araucania <- sitios_araucania %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_araucania <- sitios_araucania %>% unique() # Borrar repetidos


#### Región de Los Lagos ####

# Carga de la url con el lista de sitios de la región
url_los_lagos <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/x-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_los_lagos <- read_html(url_los_lagos)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_los_lagos <- data.frame(sitios = web_los_lagos %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                               link = web_los_lagos %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_los_lagos$link

sitios_los_lagos$sitios <- gsub("[^[:alnum:]]", " ", sitios_los_lagos$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/los_lagos/",y,".html"))
}, sitios_los_lagos$link, sitios_los_lagos$sitios)

links <- list.files("webs/los_lagos", full.names = T)

sitios_los_lagos <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_los_lagos <- sitios_los_lagos %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_los_lagos$region <- "Región de Los Lagos" # Asignación del nombre de la región
sitios_los_lagos <- sitios_los_lagos %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_los_lagos <- sitios_los_lagos %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_los_lagos <- sitios_los_lagos %>% unique() # Borrar repetidos


#### Región de Aysén ####

# Carga de la url con el lista de sitios de la región
url_aysen <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/xi-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_aysen <- read_html(url_aysen)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_aysen <- data.frame(sitios = web_aysen %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_aysen %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_aysen$link

sitios_aysen$sitios <- gsub("[^[:alnum:]]", " ", sitios_aysen$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/aysen/",y,".html"))
}, sitios_aysen$link, sitios_aysen$sitios)

links <- list.files("webs/aysen", full.names = T)

sitios_aysen <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_aysen$region <- "Región de Aysén" # Asignación del nombre de la región
sitios_aysen <- sitios_aysen %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_aysen <- sitios_aysen %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_aysen <- sitios_aysen %>% unique() # Borrar repetidos


#### Región de Magallanes ####

# Carga de la url con el lista de sitios de la región
url_magallanes <- "https://memoriaviva.com/nuevaweb/category/centros-de-detencion/xii-region/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_magallanes <- read_html(url_magallanes)

# Construcción de base de datos con el nombre y el link de los sitios de la región
sitios_magallanes <- data.frame(sitios = web_magallanes %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                           link = web_magallanes %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los sitios de la región
links <- sitios_magallanes$link

sitios_magallanes$sitios <- gsub("[^[:alnum:]]", " ", sitios_magallanes$sitios)

Map(function(x, y) {
  download_html(url = x,
                file = paste0("webs/magallanes/",y,".html"))
}, sitios_magallanes$link[23:30], sitios_magallanes$sitios[23:30])

links <- list.files("webs/magallanes", full.names = T)

sitios_magallanes <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

sitios_magallanes <- sitios_magallanes %>%
  group_by(sitio) %>%
  summarise(coordenadas = first(coordenadas))

sitios_magallanes$region <- "Región de Magallanes" # Asignación del nombre de la región
sitios_magallanes <- sitios_magallanes %>% separate(col=coordenadas, into=c('direccion', 'coordenadas'), sep='=') #Extracción de coordenadas parte 1
sitios_magallanes <- sitios_magallanes %>% separate(col=coordenadas, into=c('lat', 'long'), sep=',') %>% select(region, sitio, lat, long) # Extracción de coordenadas parte 2
sitios_magallanes <- sitios_magallanes %>% unique() # Borrar repetidos

#### Sitios a nivel nacional ####

sitios <- sitios_tarapaca %>%
  full_join(sitios_antofagasta) %>%
  full_join(sitios_atacama) %>%
  full_join(sitios_coquimbo) %>%
  full_join(sitios_valparaiso) %>%
  full_join(sitios_metropolitana) %>%
  full_join(sitios_ohiggins) %>%
  full_join(sitios_maule) %>%
  full_join(sitios_biobio) %>%
  full_join(sitios_araucania) %>%
  full_join(sitios_los_lagos) %>%
  full_join(sitios_aysen) %>%
  full_join(sitios_magallanes)

openxlsx::write.xlsx(sitios, "resultados/sitios.xlsx")
