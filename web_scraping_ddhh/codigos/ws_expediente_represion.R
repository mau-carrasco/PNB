#### Obtención de listado de sitios con link ####

setwd("C:/Users/mauricio.carrasco/OneDrive - Ministerio de Justicia/Datos PNB/")

#### Carga de paquetes ####
library(tidyverse)
library(rvest)

#### Directorio de trabajo ####
setwd("C:/Users/mauricio.carrasco/OneDrive - Ministerio de Justicia/Datos PNB/web_scraping_ddhh/")

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
  tabla <- webpage %>% html_elements(".page-header+ .tabla_box")  %>% html_table()
  data.frame(tabla)
}

#### Base de datos con información de las sentencias ####
info_condenas <- purrr::map_df(links, get_data_causas)

info_condenas <- info_condenas %>%
  select(variable = 1, valor = 2) %>%
  mutate(
    numero = if_else(variable == "Nombre de la causa", 1, 0),
    id = cumsum(numero)) %>%
  select(!numero)

bd_condenas <- info_condenas %>% pivot_wider(names_from = variable, values_from = valor)

#### Vectores con información relevantes de sentencias ####

victimas_condena <- strsplit(bd_condenas$`Víctima(s)`, split = ", ")
victimas_condena <- data.frame(id = rep(bd_condenas$id, sapply(victimas_condena, length)), victimas = unlist(victimas_condena))

criminales_condena <- strsplit(bd_condenas$`Victimarios con condena confirmada`, split = ", ")
criminales_condena <- data.frame(id = rep(bd_condenas$id, sapply(criminales_condena, length)), criminales = unlist(criminales_condena))

sitios_condena <- strsplit(bd_condenas$`Centro de detención asociado`, split = ", ")
sitios_condena <- data.frame(id = rep(bd_condenas$id, sapply(sitios_condena, length)), sitio = unlist(sitios_condena))
sitios_condena <- sitios_condena %>% drop_na()

#### Guardado de base de datos con setencias ####

openxlsx::write.xlsx(bd_condenas, "resultados/base_condenas.xlsx")

#### Guardado de vectores con información relevante de las sentencias ####

openxlsx::write.xlsx(victimas_condena, "resultados/victimas_condena.xlsx")

openxlsx::write.xlsx(criminales_condena, "resultados/criminales_condena.xlsx")

openxlsx::write.xlsx(sitios_condena, "resultados/sitios_condena.xlsx")

