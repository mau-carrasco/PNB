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

victimas_mv <- victimas_mv %>%
  group_by(victima) %>%
  separate_longer_delim(contenido, delim = "\n") %>%
  mutate(parrafos = str_detect(contenido, "[aeiou]")) %>%
  ungroup() %>%
  filter(parrafos == TRUE) %>%
  mutate(contenido = str_trim(contenido),
         contenido = str_replace_all(contenido,"[\\s]+", " "),
         victima = str_trim(victima),
         victima = str_replace_all(victima,"[\\s]+", " ")
         ) %>%
  select(!parrafos) %>%
  group_by(victima) %>%
  mutate(rut = if_else(grepl("Rut :", contenido), contenido, NA),
         rut = first(rut, na_rm = T),
         rut = gsub("[^[:digit:].-]", "", rut),
         fec_nacimiento = if_else(grepl("Fecha Nacimiento :", contenido), contenido, NA),
         fec_nacimiento = first(fec_nacimiento, na_rm = T),
         fec_nacimiento = gsub("[^[:digit:].-]", "", fec_nacimiento),
         fec_nacimiento = str_sub(fec_nacimiento, 1, 10),
         lugar_nacimiento = if_else(grepl("Lugar Nacimiento :", contenido), contenido, NA),
         lugar_nacimiento = first(lugar_nacimiento, na_rm = T),
         fec_detencion = if_else(grepl("Fecha Detención :", contenido), contenido, NA),
         fec_detencion = first(fec_detencion, na_rm = T),
         fec_detencion = gsub("[^[:digit:].-]", "", fec_detencion),
         fec_detencion = str_sub(fec_detencion, 1, 10),
         lugar_detencion = if_else(grepl("Lugar Detención :", contenido), contenido, NA),
         lugar_detencion = first(lugar_detencion, na_rm = T),
         edad = if_else(grepl("Edad :", contenido), contenido, NA),
         edad = first(edad, na_rm = T),
         actividad = if_else(grepl("Actividad :", contenido), contenido, NA),
         actividad = first(actividad, na_rm = T),
         actividad_politica = if_else(grepl("Actividad Política :", contenido), contenido, NA),
         actividad_politica = first(actividad_politica, na_rm = T))


victimas_mv <- victimas_mv %>%
  separate(lugar_nacimiento, c(NA, "lugar_nacimiento"), sep = "Lugar Nacimiento : ") %>%
  separate(lugar_detencion, c(NA, "lugar_detencion"), sep = "Lugar Detención : ") %>%
  separate(edad, c(NA, "edad"), sep = "Edad : ") %>%
  separate(actividad, c(NA, "actividad"), sep = "Actividad : ") %>%
  separate(actividad_politica, c(NA, "actividad_politica"), sep = "Actividad Política : ") %>%
  ungroup()


victimas_mv <- victimas_mv %>%
  group_by(victima) %>%
  mutate(estado_civil = if_else(grepl("Estado Civil e Hijos :", contenido), contenido, NA),
         estado_civil = first(estado_civil, na_rm = T),
         nacionalidad = if_else(grepl("Nacionalidad :", contenido), contenido, NA),
         nacionalidad = first(nacionalidad, na_rm = T)
         ) %>%
  separate(estado_civil, c(NA, "estado_civil"), sep = " :") %>%
  separate(nacionalidad, c(NA, "nacionalidad"), sep = ":") %>%
  mutate(estado_civil = str_trim(estado_civil),
         nacionalidad = str_trim(nacionalidad)) %>%
  ungroup()

listado_victimas <- victimas_mv %>%
  select(victima,
         rut,
         nacionalidad,
         fec_nacimiento,
         lugar_nacimiento,
         edad,
         estado_civil,
         actividad,
         actividad_politica,
         fec_detencion,
         lugar_detencion
         ) %>%
  unique()

listado_victimas$tipo_caso <- "Detenidos desaparecidos"

openxlsx::write.xlsx(listado_victimas, "~/GitHub/PNB/web_scraping_ddhh/resultados/victimas_mv.xlsx", asTable = T)



#### Ejecutados políticos ####

# Carga de la url con el lista de víctimas de la categoria
url <- "https://memoriaviva.com/nuevaweb/category/ejecutados-politicos/"

# Creación de objeto flotante con la página de la lista de víctimas de la categoría
web_ejecutados <- read_html(url)

# Construcción de base de datos con el nombre y el link de la víctima
listado_ejecutados <- data.frame(listado = web_ejecutados %>% html_nodes("#lista-desaparecidos a") %>% html_text(),
                                    link = web_ejecutados %>% html_nodes("#lista-desaparecidos a") %>% html_attr("href"))

# Lista de links con información de los ejecutados
links <- listado_ejecutados$link

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
                paste0("~/GitHub/PNB/web_scraping_ddhh/documentos/victimas/ejecutados_politicos/",y,".html"),
                mode="wb")
}, sitios_victimas$web_victima, sitios_victimas$victima)

# Construcciòn de la base de datos de detenidos desaparecidos
# Lista de links con los sitios web de las víctimas
links <- paste0("~/GitHub/PNB/web_scraping_ddhh/documentos/victimas/ejecutados_politicos/",sitios_victimas$victima,".html")

# Función que extrae el link del sitio web de las víctimas en MV
get_web_data <- function(url) {
  webpage <- url %>% read_html() # Lectura de la página web de cada víctima
  victima <- webpage %>% html_nodes(".page-title") %>%html_text() %>% trimws() # Nombre completo de la víctima
  contenido <- webpage %>% html_nodes("#main") %>% html_text() %>% trimws() # Contenido en bruto del sitio web de la víctima
  data.frame(victima, contenido) # Devuelve un set de datos con el nombre de la víctima y el contenido de su página web en MV
}

victimas_mv <- purrr::map_df(links, get_web_data) # Creación de base de datos con el nombre y el link de coordenadas de cada sitio

victimas_mv <- victimas_mv %>%
  group_by(victima) %>%
  separate_longer_delim(contenido, delim = "\n") %>%
  mutate(parrafos = str_detect(contenido, "[aeiou]")) %>%
  ungroup() %>%
  filter(parrafos == TRUE) %>%
  mutate(contenido = str_trim(contenido),
         contenido = str_replace_all(contenido,"[\\s]+", " "),
         victima = str_trim(victima),
         victima = str_replace_all(victima,"[\\s]+", " ")
  ) %>%
  select(!parrafos) %>%
  group_by(victima) %>%
  mutate(rut = if_else(grepl("Rut :", contenido), contenido, NA),
         rut = first(rut, na_rm = T),
         rut = gsub("[^[:digit:].-]", "", rut),
         fec_nacimiento = if_else(grepl("Fecha Nacimiento :", contenido), contenido, NA),
         fec_nacimiento = first(fec_nacimiento, na_rm = T),
         fec_nacimiento = gsub("[^[:digit:].-]", "", fec_nacimiento),
         fec_nacimiento = str_sub(fec_nacimiento, 1, 10),
         lugar_nacimiento = if_else(grepl("Lugar Nacimiento :", contenido), contenido, NA),
         lugar_nacimiento = first(lugar_nacimiento, na_rm = T),
         fec_detencion = if_else(grepl("Fecha Detención :", contenido), contenido, NA),
         fec_detencion = first(fec_detencion, na_rm = T),
         fec_detencion = gsub("[^[:digit:].-]", "", fec_detencion),
         fec_detencion = str_sub(fec_detencion, 1, 10),
         lugar_detencion = if_else(grepl("Lugar Detención :", contenido), contenido, NA),
         lugar_detencion = first(lugar_detencion, na_rm = T),
         fec_asesinato = if_else(grepl("Fecha Asesinato :", contenido), contenido, NA),
         fec_asesinato = first(fec_asesinato, na_rm = T),
         fec_asesinato = gsub("[^[:digit:].-]", "", fec_asesinato),
         fec_asesinato = str_sub(fec_asesinato, 1, 10),
         lugar_asesinato = if_else(grepl("Lugar Asesinato :", contenido), contenido, NA),
         lugar_asesinato = first(lugar_asesinato, na_rm = T),
         edad = if_else(grepl("Edad :", contenido), contenido, NA),
         edad = first(edad, na_rm = T),
         actividad = if_else(grepl("Actividad :", contenido), contenido, NA),
         actividad = first(actividad, na_rm = T),
         actividad_politica = if_else(grepl("Actividad Política :", contenido), contenido, NA),
         actividad_politica = first(actividad_politica, na_rm = T),
         estado_civil = if_else(grepl("Estado Civil", contenido), contenido, NA),
         estado_civil = first(estado_civil, na_rm = T),
         nacionalidad = if_else(grepl("Nacionalidad :", contenido), contenido, NA),
         nacionalidad = first(nacionalidad, na_rm = T)) %>%
  separate(lugar_nacimiento, c(NA, "lugar_nacimiento"), sep = "Lugar Nacimiento :") %>%
  separate(lugar_detencion, c(NA, "lugar_detencion"), sep = "Lugar Detención :") %>%
  separate(lugar_asesinato, c(NA, "lugar_asesinato"), sep = "Lugar Asesinato :") %>%
  separate(edad, c(NA, "edad"), sep = "Edad : ") %>%
  separate(actividad, c(NA, "actividad"), sep = "Actividad :") %>%
  separate(actividad_politica, c(NA, "actividad_politica"), sep = "Actividad Política :") %>%
  separate(estado_civil, c(NA, "estado_civil"), sep = " :") %>%
  separate(nacionalidad, c(NA, "nacionalidad"), sep = ":") %>%
  mutate(lugar_nacimiento = str_trim(lugar_nacimiento),
         lugar_detencion = str_trim(lugar_detencion),
         lugar_asesinato = str_trim(lugar_asesinato),
         actividad = str_trim(actividad),
         actividad_politica = str_trim(actividad_politica),
         estado_civil = str_trim(estado_civil),
         nacionalidad = str_trim(nacionalidad)) %>%
  ungroup()

listado_victimas_ep <- victimas_mv %>%
  select(!contenido) %>%
  unique()

listado_victimas_ep$tipo_caso <- "Ejecutados políticos"

openxlsx::write.xlsx(listado_victimas_ep, "~/GitHub/PNB/web_scraping_ddhh/resultados/victimas_ep.xlsx", asTable = T)
