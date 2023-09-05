###############################################################
#### Base de datos para actividad red de sitios de memoria ####
###############################################################

#### Carga de paquetes y datos ####

# paquetes
library(tidyverse)
library(readxl)

# datos
victimas_condena <- read_excel("resultados/victimas_condena.xlsx")
criminales_condena <- read_excel("resultados/criminales_condena.xlsx")
sitios_condena <- read_excel("resultados/sitios_condena.xlsx")
base_condenas <- read_excel("resultados/base_condenas.xlsx")

#### Proceamiento y análisis ####

# victimas en sitios
sitios_victimas <- sitios_condena %>%
  left_join(base_condenas %>%
              select(id, `Rol(es)`)) %>%
  left_join(victimas_condena) %>%
  select(id_sentencia = 1, roles = 3,2,4)
sitios_victimas<- sitios_victimas %>%
  group_by(sitio) %>%
  summarise(victimas, id_sentencia, roles)
sitios_victimas <- sitios_victimas %>% filter(victimas != "Caso Colonia Dignidad delito de asociación ilícita")
openxlsx::write.xlsx(sitios_victimas, "resultados/actividad_red_sitios/sitios_victima.xlsx")

# criminales en sitios
sitios_criminales <- sitios_condena %>%
  left_join(base_condenas %>%
              select(id, `Rol(es)`)) %>%
  left_join(criminales_condena)
sitios_criminales <- sitios_criminales %>%
  group_by(sitio) %>%
  reframe(criminales, id_sentencia = id, roles = `Rol(es)`)
sitios_criminales <- sitios_criminales %>% filter(!criminales %in% c("Absolución de todos los acusados"))
openxlsx::write.xlsx(sitios_criminales, "resultados/actividad_red_sitios/sitios_criminales.xlsx")
