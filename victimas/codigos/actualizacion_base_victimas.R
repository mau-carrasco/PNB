#--------------------------------------------#
# Actualización de base de datos de víctimas #
#--------------------------------------------#

#### Carga de paquetes y base de datos ####

library(tidyverse)
library(readxl)

victimas <- readRDS("~/GitHub/PNB/victimas/resultados/victimas.rds")
victimas_procesos <- read_excel("GitHub/PNB/procesos_judiciales/resultados/victimas_procesos.xlsx", sheet = 3)
criminales_condenados <- read_excel("GitHub/PNB/criminales/criminales_condenados.xlsx")

#### Unión de base de datos ####
victimas <- victimas %>%
  left_join(victimas_procesos %>% select(victima, contains("causa")))

#### Recodificación de variables ####

# recodificar el tipo de víctimas con categorías académicas
victimas <- victimas %>%
  mutate(
    tipo_caso = case_when(
      tipo_caso == "DESAPARECIDO/A" ~ 1,
      tipo_caso == "ASESINADO/A SIN ENTREGA DE CUERPO" ~ 2,
      tipo_caso == "ASESINADO/A" ~ 3
    ),
    tipo_caso = factor(tipo_caso, labels = c("Desaparecido/a", "Desaparecido/a con información de muerte", "Muerto")))

# recodificar situación de búsqueda
victimas <- victimas %>%
  mutate(identificacion = if_else(identificacion == "NO IDENTIFICADO/A", "No identificado/a", "Identificado/a"))

# recodificación de factores
victimas <- victimas %>%
  mutate(across(c(sexo, nacionalidad, tipo_actividad, cargo, ciudad, comuna, tipo_caso), str_to_title)) %>%
  mutate(across(c(sexo, nacionalidad, tipo_actividad, tipo_caso, estado_causa_rec, identificacion), factor))

# creacion de id caso comision
victimas <- tibble(
  id_caso_comision = 1:n_distinct(victimas$relato),
  relato = unique(victimas$relato)
  ) %>%
  right_join(victimas)

# creacion de id caso judicial
victimas <- tibble(rol_causa = unique(victimas$rol_causa)) %>%
  drop_na() %>%
  mutate(id_caso_judicial = 1:n_distinct(victimas$rol_causa, na.rm = T)) %>%
  right_join(victimas)

# creación de columnas con información de condenas

victimas <- criminales_condenados %>%
  mutate(Agente = gsub("[^[:alpha:] ]", "", Agente)) %>%
  mutate(Agente = gsub("  C", "", Agente),
         Agente = str_trim(Agente),
         condenado = paste0(Agente," (", Rama_Perteneciente, "), condenado por el delito de ", Delito, " a la pena de ", Condena)
  ) %>%
  group_by(rol_causa = Rol) %>%
  summarize(agentes_condena = paste(condenado, collapse = "; "),
            instituciones_agentes_condena = paste(unique(Rama_Perteneciente), collapse = ", ")) %>%
  right_join(victimas)

#### Orden y etiqueta de las variables ####

# Orden de las variables
victimas <- victimas %>%
  select(
    id_victima = id,
    id_caso_comision,
    id_caso_judicial,
    victima,
    sexo,
    nacionalidad,
    rut,
    fecha_nacimiento,
    edad,
    actividad,
    tipo_actividad,
    militancia,
    cargo,
    tipo_caso,
    fecha_desaparicion_muerte,
    region,
    ciudad,
    comuna,
    comision,
    calificacion_comision = calificacion,
    relato_comision = relato,
    rol_causa,
    fecha_actualizacion_causa,
    estado_causa,
    estado_causa_rec,
    agentes_condena,
    instituciones_agentes_condena,
    identificacion,
    identificacion_det
  )

# etiquetas de las variables
victimas <- labelled::set_variable_labels(victimas,
                              id_caso_comision = "Código único de caso en comisión de la verdad",
                              id_caso_judicial = "Código único de caso judicial",
                              victima = "Nombre completo de la víctima",
                              rut = "RUN de la víctima",
                              fecha_nacimiento = "Fecha de nacimiento de la víctima",
                              edad = "Edad de la víctima al momento del secuestro o asesinato",
                              actividad = "Actividad principal de la víctima (detalle)",
                              sexo = "Sexo de la víctima",
                              nacionalidad = "Nacionalidad de la víctima",
                              militancia = "Partido o movimiento político al que pertenecía o adhería la víctima",
                              tipo_actividad = "Tipo de actividad principal de la víctima",
                              cargo = "Cargo político de la víctima",
                              tipo_caso = "Descripción académica de los tipos de víctima",
                              ciudad = "Ciudad de desaparición o muerte",
                              comuna = "Comuna de desaparición o muerte",
                              fecha_desaparicion_muerte = "Fecha de desaparición o muerte de la víctima",
                              relato_comision = "Relato del caso producido por la comisisión de la verdad",
                              rol_causa = "Rol de la causa judicial",
                              fecha_actualizacion_causa = "Fecha de actualización de la información judicial",
                              estado_causa = "Estado de la causa judicial",
                              estado_causa_rec = "Tipo de estado de la causa judicial",
                              agentes_condena = "Agentes condenados",
                              instituciones_agentes_condena = "Rama de las FFAA a las que pertenecen los agentes condenados",
                              identificacion = "¿La víctima fue hallada e identificada?")

#### Libro de códigos ####

libro_codigos <- labelled::look_for(victimas)


#### Guardar archivo ####
write_rds(victimas, "~/GitHub/PNB/victimas/resultados/victimas.rds")
openxlsx::write.xlsx(victimas, "~/GitHub/PNB/victimas/resultados/victimas.xlsx")
openxlsx::write.xlsx(libro_codigos, "~/GitHub/PNB/victimas/resultados/libro_codigos.xlsx")
