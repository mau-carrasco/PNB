# Plan Nacional de Búsqueda
Repositorio con códigos y datos utilizados por el equipo de diseño e implementación del Plan Nacional de Búsqueda de Víctimas de Desaparición Forzada en Chile

## Víctimas
En repositorio de **[Víctimas](https://github.com/mau-carrasco/PNB/tree/main/victimas)** están disponibles los códigos y los datos utilizados para construir la nómina oficial de víctimas de asesinato y desaparición forzada durante la dictadura civil militar (1973 - 1990). Este repositorio contiene las siguientes carpetas:
- **Códigos**: Script de R con el trabajo de reconstrucción de la base de datos de víctimas de asesinato y desaparición forzada calificadas por las Comisiones de la Verdad (*N: 3.216*)
- **Datos**: Archivos excel con: a) El listado de víctimas calificadas por las Comisiones de la Verdad, obtenido del sistema de datos del *Programa de Derechos Humanos* (PDH) del Ministerio de Justicia y Derechos Humanos; b) El listado de víctimas de desaparición forzda elaborado por el *Área de Investigación Administrativa* del PDH
- **Resultados**: Archivos Excel y RDS con la base de datos de víctimas obtenida a través del procesamiento masivo de información

## Web scraping
En el repositorio **[web_scraping_ddhh](https://github.com/mau-carrasco/PNB/tree/main/web_scraping_ddhh)** encontrarás los códigos utilizados para la extracción de datos de tres sitios web especializados en crímenes de lesa humanidad:
- **Museo de la memoria y los DDHH**: La página web del Museo de la Memoria y los Derechos Humanos de Chile cuenta con una base de datos que contiene información de las víctimas de asesinato y desaparición forzada que fueron calificadas por la *Comisión Nacional de Verdad y Reconciliación* (CVR), la *Corporación Nacional de Reparación y Reconciliación* (CNRR) y la *Comisión Asesora Presidencial para la Calificación de Detenidos Desaparecidos, Ejecutados Políticos y Víctimas de Prisión, Política y Tortura* (CAPCDDEPVPPT)
- **Expedientes de la Represión**: La página web del proyecto académico Expediente de la Represión contiene las sentencias judiciales dictadas en los procesos penales ya concluidos por las violaciones sistemáticas de derechos humanos cometidas durante la dictadura civil militar que se impuso en Chile con el golpe de Estado de 1973 (1973-1990).
- **Memoria Viva**: La página web del proyecto Memoria Viva contiene información sistematizada sobre víctimas, sitios y criminales. En la carpeta de este proyecto se encuentran alojados los códigos y los datos obtenidos la sección de sitios. En las próximas semanas se subirá la información de los criminales y de las víctimas.

En este mismo repositorio, encontrarás también los **resultados** del web scrpaing, con los datos extraídos de los sitios web anteriormente mencionados, y los **documentos** con las setencias analizadas por el proyecto *Expediente de la Represión*
