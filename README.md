# Indicadores socioecológicos

Este proyecto contiene scripts desarrollados en R para calcular y visualizar indicadores relacionados con el uso de especies y la gobernanza. El análisis se organiza en dos componentes principales:

1. *Uso relativo de especies*: Este componente permite calcular y visualizar el indicador de uso relativo de especies por grupo biológico a partir de registros de uso. Incluye los siguientes análisis:
- Proporción de registros de uso por grupo biológico: mamíferos, peces y plantas;
- Indicador de uso relativo de especies por grupo biológico para cada periodo climático;
- Desagregación del indicador por grupo biológico y macrohábitat;
- Desagregación del indicador para plantas por categoría de uso;
- Dominancia acumulada del uso.

2. *Indicadores de gobernanza*: Este componente permite calcular y visualizar indicadores de gobernanza asociados a un actor central. Se organiza en dos líneas de análisis:
- Relaciones de gobernanza: caracteriza las relaciones del actor central con otros actores mediante el cálculo del número de actores, el índice de diversidad de actores, la distribución de las relaciones por tipo (colaborativa, neutra o conflictiva), el índice de intensidad relacional, la variación de la intensidad y sus desagregaciones por tipo de actor (Asociación, Cooperación internacional, Gremio, Institución educativa, Institución privada, Institución pública, Organización social).
- Acciones colectivas: analiza las acciones colectivas desarrolladas por el actor central mediante el cálculo del número de acciones activas, la variación del número de acciones, la composición de la participación por género y grupo etario, y los índices de equidad en la participación.

Los scripts utilizan datos de campo anonimizados del Instituto de Investigación de Recursos Biológicos Alexander von Humboldt y generan automáticamente archivos de resultados en formato Excel y gráficas en formato PNG.
Actualmente, el proyecto permite:
- Crear automáticamente las carpetas Datos, Resultados y Graficas cuando no existen;
- Calcular los indicadores principales y sus desagregaciones;
- Generar tablas de resultados;
- Exportar automáticamente los resultados a archivos de Excel;
- Elaborar y exportar las gráficas en formato PNG.
La carpeta Datos se crea automáticamente cuando no existe. Sin embargo, el archivo de entrada debe ser incorporado por la persona usuaria.


## Organización del proyecto

El repositorio se organiza en dos componentes principales: Indicadores de uso de especies e Indicadores de gobernanza. Cada componente cuenta con sus propias carpetas de datos, códigos, resultados y gráficas.


## Prerrequisitos

Se recomienda disponer de:
-	R 4.3 o posterior;
-	RStudio o un editor compatible con R;
-	Los siguientes paquetes de R:
  -	"readxl" versión 1.4.3
  -	"writexl" versión 1.5.4
  -	"dplyr" versión 1.1.4
  -	"lubridate" versión 1.9.4
  -	"ggplot2" versión 4.0.1
  -	"tidyr" versión 1.3.1
  -	"ggalluvial" versión 0.12.5
  -	"scales" versión 1.4.0
  -	"forcats" versión 1.0.0
  -	"this.path" versión 2.8.0
  -	"purrr" versión 1.2.2
  -	"colorspace" versión 2.1.1

## Archivos necesarios

El repositorio incluye archivos de ejemplo para mostrar la estructura, las hojas y las variables requeridas para ejecutar los scripts. Estos archivos contienen datos ficticios o anonimizados preparados únicamente con fines demostrativos y no corresponden a los datos originales utilizados durante el desarrollo y la validación del código.

Se pueden emplear estos archivos como referencia o plantilla para organizar sus propios datos. Antes de ejecutar los scripts, debe sustituir los registros de ejemplo por la información correspondiente a su proyecto, conservando los nombres de las hojas y de las columnas requeridos por el código.

Los nombres de las hojas y de las columnas deben conservarse exactamente como aparecen en las plantillas, salvo que el código incluya explícitamente una rutina de estandarización.

## Como ejecutar
Los scripts pertenecen al mismo proyecto, pero pueden ejecutarse de manera independiente. No existe una dependencia que obligue a ejecutar uno antes que otro.

Antes de ejecutar cualquier script, se debe:

Guardar el archivo de entrada correspondiente en la carpeta Datos/ del componente que se desea analizar.
Verificar que el libro de Excel contenga las hojas requeridas.
Confirmar que los nombres de las columnas coincidan con los utilizados en el código.
Ejecutar el script correspondiente desde RStudio o desde una terminal.

Antes de ejecutar el script de uso relativo de especies, se debe revisar la correspondencia establecida entre los meses y los periodos climáticos. Esta configuración debe ajustarse cuando el análisis se aplique en otro territorio o cuando cambie el calendario agroecológico del área de estudio.

También se debe verificar la fecha inicial utilizada para delimitar los registros incluidos en el cálculo, y hacer el ajuste en caso de necesitarlo.

## Resultados
Los archivos de resultados de cada componente se almacenan en la carpeta:
Resultados/
Los nombres de los archivos incluyen la fecha y la hora de ejecución. Cada archivo contiene diferentes hojas correspondientes a los indicadores, índices, tablas y desagregaciones calculadas.

##Gráficas
Las gráficas de cada componente se almacenan en la carpeta:
Graficas/
Los archivos se guardan en formato PNG, con una resolución de 300 dpi y una marca de fecha y hora en el nombre.

## Autores y contacto
Desarrollo del código:
* **[Leidy Marcela Cepeda Buitrago]**- *Investigadora asistente I.Humboldt* -  [Contacto](lcepeda@humboldt.org.co)