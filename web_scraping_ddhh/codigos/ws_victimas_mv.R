##############################################
#### Web scraping víctimas - Memoria viva ####
##############################################

#### Carga de paquetes ####
library(tidyverse)
library(rvest)

#### Detenidos desaparecidos ####

# Carga de la url con el lista de sitios de la región
url <- "https://memoriaviva.com/nuevaweb/category/detenidos-desaparecidos/"

# Creación de objeto flotante con la página de la lista de sitios de la región
web_desaparecidos <- read_html(url)

# Construcción de base de datos con el nombre y el link de los sitios de la región
listado_desaparecidos <- data.frame(listado = web_desaparecidos %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                              link = web_desaparecidos %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los detenidos desaparecidos
links <- listado_desaparecidos$link

# Función que extrae el link del sitio web de las víctimas en MV
get_web_data <- function(url) {
  webpage <- url %>% read_html() # Lectura de la página web de cada víctima
  victima <- webpage %>% html_nodes("#lista-desaparecidos a") %>%html_text() %>% trimws() # Nombre completo de la víctima
  web_victima <- webpage %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href") # Link de la página web de cada víctima en MV
  data.frame(victima, web_victima) # Devuelve un set de datos con el nombre de la víctima y el link de su página web en MV
}

sitios_victimas <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio


Map(function(x, y) {
  download.file(url = x,
                paste0("~/GitHub/PNB/web_scraping_ddhh/documentos/victimas/detenidos_desaparecidos/",y,".html"),
                mode="wb")
}, sitios_victimas$web_victima, sitios_victimas$victima)

# Construcciòn de la base de datos de detenidos desaparecidos
# Lista de links con los sitios web de las víctimas
links <- paste0("~/GitHub/PNB/web_scraping_ddhh/documentos/victimas/detenidos_desaparecidos/",sitios_victimas$victima,".html")

# Función que extrae el link del sitio web de las víctimas en MV
get_web_data <- function(url) {
  webpage <- url %>% read_html() # Lectura de la página web de cada víctima
  victima <- webpage %>% html_nodes(".page-title") %>%html_text() %>% trimws() # Nombre completo de la víctima
  contenido <- webpage %>% html_nodes("#main") %>% html_text() %>% trimws() # Contenido en bruto del sitio web de la víctima
  data.frame(victima, contenido) # Devuelve un set de datos con el nombre de la víctima y el contenido de su página web en MV
}

victimas_mv <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

victimas_mv[1,2]

victimas_mv <- victimas_mv %>%
  group_by(victima) %>%
  separate_longer_delim(contenido, delim = "\n") %>%
  mutate(parrafos = str_detect(contenido, "[aeiou]")) %>%
  ungroup() %>%
  filter(parrafos == TRUE) %>%
  mutate(contenido = str_trim(contenido))

victimas_mv <- victimas_mv %>%
  group_by(victima) %>%
  mutate(
    fuentes = if_else(grepl("Fuente :", contenido),1,0),
    fuentes = lead(fuentes,1),
    n_fuentes = cumsum(fuentes),
    categoria = case_when(contenido == "Categoría : Antecedentes del Caso" ~ "Antecedentes del Caso",
                          contenido == "Categoría : Judicial" ~ "Judicial",
                          contenido == "Categoría : Mensaje" ~ "Mensaje",
                          contenido == "Categoría : Otra Información" ~ "Otra información",
                          contenido == "Categoría : Prensa" ~ "Prensa",
                          contenido == "Categoría : Testimonio" ~ "Testimonio"),
    categoria = lead(categoria, 2),
      ) %>%
  select(!parrafos)

victimas_mv <- victimas_mv %>%
  group_by(victima, n_fuentes) %>%
  mutate(
    categoria = replace_na(categoria, first(categoria)),
    titulo = first(contenido)
         ) %>%
  drop_na() %>%
  filter(!contenido == titulo,
         !grepl("Categoría :", contenido)) %>%
  mutate(fuente = first(contenido),
         fuente = sub('Fuente :', '', fuente)) %>%
  filter(!grepl("Fuente :", contenido)) %>%
  ungroup() %>%
  group_by(victima, titulo) %>%
  mutate(parrafo = row_number()) %>%
  select(victima, fuente, categoria, titulo, parrafo, contenido)

openxlsx::write.xlsx(victimas_mv, "~/GitHub/PNB/web_scraping_ddhh/resultados/victimas_mv.xlsx", asTable = T)
