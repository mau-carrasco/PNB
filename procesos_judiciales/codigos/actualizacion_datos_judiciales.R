##############################################
#### Procesamiento de datos de judiciales ####
##############################################

#### Carga de paquetes y datos ####

# carga de paquetes
library(tidyverse)
library(readxl)

# carga de datos
datos_judiciales <- read_excel("GitHub/PNB/procesos_judiciales/datos/20240311-Informe-PJP (1).xlsx")
victimas <- readRDS("~/GitHub/PNB/victimas/resultados/victimas.rds")

#### Extracción de información relevante y construcción de base de datos judiciales actualizados ####

# creación de data frame con panel de víctimas en procesos judiciales
datos_judiciales <- datos_judiciales %>%
  select(Victimas, Fecha_EstProc, Rol, EstadoProc) %>% # selección de variables de interes
  drop_na(Victimas) %>% # elimino las causas que no tienen víctimas
  group_by(victima = Victimas, # agrupo toda la información extraída por víctima
           fecha_actualizacion_causa = Fecha_EstProc) %>% # también la ordeno por fecha de actualización del estado procesal para generar orden cronológico
  summarise(rol_causa = last(Rol), # agrego los rol de las causas
            estado_causa = last(EstadoProc)) %>% # agrego los estados de las causas
  ungroup() # desagrupo

# creación de tibble con datos judiciales actualizados
datos_actualizados <- datos_judiciales %>%
  group_by(victima) %>% # agrupo por víctima
  summarise(across(everything(), last)) # creo tible con la información más reciente de los procesos judiciales


#### Recodificación de variables ####

# Dar formato a las fechas
datos_actualizados <- datos_actualizados %>%
  mutate(fecha_actualizacion_causa = date(fecha_actualizacion_causa))

# Limpiar y agrupas categorías de estados procesales
datos_actualizados <- datos_actualizados %>%
  mutate(
    estado_causa_rec = case_when(
      grepl("jecut", estado_causa) ~ "Sentencia Ejecutoriada",
      grepl("umario", estado_causa) ~ "Sumario",
      grepl("obresei|sobres", estado_causa) ~ "Sobreseimiento",
      grepl("lenar", estado_causa) ~ "Plenario",
      grepl("asación", estado_causa) ~ "Casación",
      grepl("1ª", estado_causa) ~ "Sentencia Primera Instancia",
      grepl("2ª", estado_causa) ~ "Sentencia Segunda Instancia",
      grepl("Apel", estado_causa) ~ "Apelación",
      grepl("Se ignora|s/información", estado_causa) ~ NA,
      TRUE ~ estado_causa
    )
    )

# Cruzar información judicial con la base de víctimas de desaparición forzada
victimas <- victimas %>%
  select(victima, rut, tipo_caso, identificacion)

# Limpiar categorias de estado procesal de las víctimas sin información
victimas <- victimas %>%
  left_join(datos_actualizados) %>%
  mutate(estado_causa_rec = replace_na(estado_causa_rec, "Sin información"))

# recodificar el tipo de víctimas con categorías académicas
victimas <- victimas %>%
  mutate(
    tipo_caso = case_when(
    tipo_caso == "DESAPARECIDO/A" ~ 1,
    tipo_caso == "ASESINADO/A SIN ENTREGA DE CUERPO" ~ 2,
    tipo_caso == "ASESINADO/A" ~ 3
  ),
  tipo_caso = factor(tipo_caso, labels = c("Desaparecido/a", "Desaparecido/a con información de muerte", "muerto")))

# recodificar situación de búsqueda
victimas <- victimas %>%
  mutate(identificacion = if_else(identificacion == "NO IDENTIFICADO/A", "No identificado/a", "Identificado/a"))


#### Obtención de tabla solicitada ####
tabla_resumen <- victimas %>%
  group_by(estado_causa_rec) %>%
  count(tipo_caso) %>%
  pivot_wider(names_from = tipo_caso, values_from = n) %>%
  janitor::adorn_totals(c("row", "col"))

tabla_detalle <- victimas %>%
  group_by(estado_causa_rec) %>%
  count(estado_causa)

#### Incorporación de gráfico extra ####
hist(semester(victimas$fecha_actualizacion_causa[victimas$estado_causa_rec=="Sentencia Ejecutoriada"], with_year = T))

victimas %>%
  filter(estado_causa_rec=="Sentencia Ejecutoriada") %>%
  group_by(tipo_caso) %>%
  count(fecha_sentencia = year(fecha_actualizacion_causa)) %>%
  ggplot(
    aes(x = fecha_sentencia,
        y = n,
        fill = tipo_caso)
  ) +
  geom_col() +
  labs(title = "Emisión de sentencias finales en casos de víctimas de desaparición forzada",
       subtitle = "Por año de sentencia (1990 - 2024)",
       y = "Número de víctimas",
       x = "Años")

# guardar datos
tablas <- list("Tabla resumen" = tabla_resumen, "Tabla detalle" = tabla_detalle, "Datos" = victimas)
openxlsx::write.xlsx(tablas, "~/GitHub/PNB/procesos_judiciales/resultados/victimas_procesos.xlsx", asTable = TRUE)
