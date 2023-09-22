################################################
#### BBDD de centros de detención y tortura ####
################################################

#### Carga de paquetes ####

library(tidyverse)
library(readxl)
library(tidygeocoder)
library(labelled)

#### Carga de de datos ####

centros_detencion <- read_excel("lugares/datos/centros_detencion.xlsx")

#### Obtención de información adicional de los lugares ####

lugares_detencion <- centros_detencion %>%
  reverse_geocode(lat = lat, long = long, method = 'osm',
                  address = address_found, full_results = TRUE)

#### Estandarizando códigos de regiones y provincias ####

# Códigos y nombres de regiones

regiones <- lugares_detencion %>%
  separate(col= state, into=c('eliminar', 'nombre_region'), sep= "Región de |Región del |Región ") %>%
  select(nombre_region) %>%
  mutate(nombre_region = chartr("ÁáÉéÍíÓóÚúÑñ", "AaEeIiOoUuNn", nombre_region),
         nombre_region = if_else(nombre_region == "Libertador General Bernardo O'Higgins", "Libertador General Bernardo OHiggins", nombre_region),
         nombre_region = if_else(nombre_region == "la Araucania", "La Araucania", nombre_region)) %>%
  left_join(unique(chilemapas::codigos_territoriales %>% select(nombre_region, codigo_region)))

# Códigos y nombres de provincias

provincias <- lugares_detencion %>%
  separate(col= county, into=c('eliminar', 'nombre_provincia'), sep= "Provincia de |Provincia del ") %>%
  select(nombre_provincia) %>%
  mutate(nombre_provincia = chartr("ÁáÉéÍíÓóÚúÑñ", "AaEeIiOoUuNn", nombre_provincia),
         nombre_provincia = if_else(nombre_provincia == "Bio-Bio", "Biobio", nombre_provincia),
         nombre_provincia = if_else(nombre_provincia == "Aysen", "Aisen", nombre_provincia),
         nombre_provincia = if_else(nombre_provincia == "Coyhaique", "Coihaique", nombre_provincia)) %>%
  left_join(unique(chilemapas::codigos_territoriales %>% select(nombre_provincia, codigo_provincia)))

# Códigos y nombres de comunas

direccion <- lugares_detencion %>%
  select(address_found) %>%
  separate(col = address_found, into = c("direccion", "eliminar"), sep = ", Provincia") %>%
  select(!eliminar)

for (i in direccion[1:969, 1]){
  nombre_comuna <- sapply(str_split(i, ", "),  tail, 1)
}

comunas <- tibble(nombre_comuna) %>%
  mutate(nombre_comuna = chartr("ÁáÉéÍíÓóÚú", "AaEeIiOoUu", nombre_comuna),
         nombre_comuna = chartr("Ññ", "Nn", nombre_comuna),
         nombre_comuna = if_else(grepl("Calera", nombre_comuna), "Calera", nombre_comuna),
         nombre_comuna = if_else(grepl("Llay", nombre_comuna), "Llaillay", nombre_comuna),
         nombre_comuna = if_else(grepl("Paihuano", nombre_comuna), "Paiguano", nombre_comuna),
         nombre_comuna = if_else(grepl("Batuco", nombre_comuna), "Lampa", nombre_comuna),
         nombre_comuna = if_else(grepl("San Vicente de", nombre_comuna), "San Vicente", nombre_comuna),
         nombre_comuna = if_else(grepl("Comuna de La Union", nombre_comuna), "La Union", nombre_comuna),
         nombre_comuna = if_else(grepl("Aysen", nombre_comuna), "Aisen", nombre_comuna),
         nombre_comuna = if_else(grepl("Coyhaique", nombre_comuna), "Coihaique", nombre_comuna),
         nombre_comuna = if_else(grepl("Padre Las Casas", nombre_comuna), "Padre las Casas", nombre_comuna),
         nombre_comuna = if_else(grepl("El Maiten", nombre_comuna), "Maipu", nombre_comuna)) %>%
  left_join(unique(chilemapas::codigos_territoriales %>% select(nombre_comuna, codigo_comuna)))

#### Construcción de base de datos de lugares de detención ####

# Vectores con información relante de los lugares de detención y tortura

lugares_detencion <- lugares_detencion %>% select(sitio, lat, long, direccion = address_found, localizacion = Exacto)

# Unión de los vectores

lugares_detencion <- cbind(regiones, provincias, comunas, lugares_detencion)

# Recodificación de variables

lugares_detencion <- lugares_detencion %>%
  rename(nombre_lugar = sitio,
         tipo_ubicacion = localizacion) %>%
  mutate(codigo_pais = "01",
         nombre_pais = "Chile",
         tipo_lugar = "Recinto de detención",
         tipo_ubicacion = case_when(tipo_ubicacion == 0 ~ "Aproximada",
                                    tipo_ubicacion == 1 ~ "Exacta",
                                    TRUE ~ "No conocida"))

# creacion de codigo de lugar

lugares_detencion <- lugares_detencion %>%
  group_by(codigo_region, codigo_provincia, codigo_comuna) %>%
  mutate(codigo_lugar = paste(codigo_comuna, row_number(nombre_lugar), sep = "-")) %>%
  ungroup()

lugares_detencion <- lugares_detencion %>%
  select(codigo_pais,
         nombre_pais,
         codigo_region,
         nombre_region,
         codigo_provincia,
         nombre_provincia,
         codigo_comuna,
         nombre_comuna,
         codigo_lugar,
         nombre_lugar,
         tipo_lugar,
         lat,
         long, 
         direccion,
         tipo_ubicacion)

# factorización de variables

lugares_detencion$tipo_ubicacion <- factor(lugares_detencion$tipo_ubicacion)
lugares_detencion$tipo_lugar <- factor(lugares_detencion$tipo_lugar)

# etiquetas

var_label(lugares_detencion) <- list(
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


#### Guardado de base de datos ####

libro_codigos <- look_for(lugares_detencion)

openxlsx::write.xlsx(lugares_detencion, "C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares_detencion.xlsx")
write_rds(lugares_detencion, "C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/lugares_detencion.rds")
openxlsx::write.xlsx(libro_codigos, "C:/Users/mauricio.carrasco/Desktop/PNB/lugares/resultados/libro_codigos.xlsx")

