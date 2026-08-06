# Título: Indicador Uso relativo de especies por grupo biológico
# Autor: Marcela Cepeda
# Descripción: Este código permite calcular el indicador «Uso relativo de especies por grupo biológico» y generar gráficas para visualizar sus resultados, 
#tanto a nivel general como en sus desagregaciones por macrohábitat y, específicamente para las plantas, por categoría de uso.
# Fuentes: Datos de campo Instituto de Investigación de Recursos Biológicos Alexander von Humboldt, 2026

#*******************************
# librerías o dependencias ----
#*******************************
library(this.path)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(Polychrome)
#*****************************************
# Definir directorio(s) de trabajo ----
#*****************************************
dir_codigo_actual <- dirname(this.path::this.path())
dir_proyecto <- normalizePath(file.path(dir_codigo_actual, ".."))
dir_datos      <- file.path(dir_proyecto, "Datos")
dir_resultados <- file.path(dir_proyecto, "Resultados")
dir_graficas   <- file.path(dir_proyecto, "Graficas")

dirs <- c(dir_datos, dir_resultados, dir_graficas)
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)}}

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
FPVA_UsosBeneficios <- read_excel(file.path(dir_datos, "FPVA_UsosBeneficios_anonimizado_I2D-SE_2026_003_V2.1.xlsx"),sheet = "Practica de Uso")
colnames(FPVA_UsosBeneficios)

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************
#Creación de periodos climáticos de monitoreo de acuerdo con el calendario agroecológico construido en conjunto con la comunidad del lugar de estudio. Así como las marcas de clase
tabla_periodos <- tibble(
  mes = 1:12,
  meses_es = c("enero", "febrero", "marzo", "abril","mayo", "junio", "julio", "agosto","septiembre", "octubre", "noviembre", "diciembre"),
  periodo_climatico = c("Verano", "Verano","Lluvias esporádicas", "Lluvias esporádicas","Lluvia", "Lluvia","Lluvia intensa","Veranillo",
    "Alternancia de lluvias", "Alternancia de lluvias", "Alternancia de lluvias","Inicio de verano"), #este periodo climático corresponde al lugar de estudio, ajustar en caso de requerirlo 
  marca_mes = c("Enero-febrero","Enero-febrero","Marzo-abril","Marzo-abril","Mayo-junio","Mayo-junio","Julio","Agosto","Septiembre-noviembre","Septiembre-noviembre","Septiembre-noviembre","Diciembre"),
  mes_inicio_periodo = c(1, 1,3, 3,5, 5,7,8,9, 9, 9,12),
  mes_fin_periodo = c(2, 2,4, 4,6, 6,7,8,11, 11, 11,12))

fecha_inicio_analisis <- as.Date("2025-09-01") #Se define la fecha de inicio para el cálculo del indicador, ajustar en caso de requerirlo 

#Adición de variables a usar para el cálculo del indicador
datos_uso <- FPVA_UsosBeneficios %>%
  mutate(`Fecha práctica de uso` = as.Date(`Fecha práctica de uso`),anio = year(`Fecha práctica de uso`),mes = month(`Fecha práctica de uso`))%>%
  filter(`Fecha práctica de uso` >= fecha_inicio_analisis) %>%  # datos únicamente desde la fecha definida previamente
  left_join(tabla_periodos, by = "mes") %>%  # Asigna a cada registro el periodo climático correspondiente
  group_by(anio,periodo_climatico,marca_mes,mes_inicio_periodo,mes_fin_periodo) %>% # Verifica si se observaron todos los meses que integran cada periodo climático
  mutate(
    mes_inicio_observado = min(mes, na.rm = TRUE),mes_fin_observado = max(mes, na.rm = TRUE),
    meses_observados = n_distinct(mes),meses_esperados = mes_fin_periodo - mes_inicio_periodo + 1,
    periodo_completo =mes_inicio_observado == mes_inicio_periodo &mes_fin_observado == mes_fin_periodo &meses_observados == meses_esperados,
    marca_mes_observada = str_to_sentence(paste(tabla_periodos$meses_es[sort(unique(mes))],collapse = "-"))
  ) %>%
  ungroup() %>%
  mutate(
    fecha_marca_clase = make_date(year = anio,month = mes_inicio_periodo,day = 1), # Crea las etiquetas de los periodos e identifica los periodos parciales
    marca_clase = paste0(if_else(periodo_completo,marca_mes,marca_mes_observada)," ",anio),
    periodo_grafica = paste0(marca_clase,"\n",periodo_climatico,if_else(periodo_completo,""," (parcial)"))
  ) %>%
  arrange(fecha_marca_clase) %>% # Ordena cronológicamente los periodos en tablas y gráficas
  mutate(
    marca_clase = factor(marca_clase,levels = unique(marca_clase),ordered = TRUE),
    periodo_grafica = factor(periodo_grafica,levels = unique(periodo_grafica),ordered = TRUE)
  )
nombre_comun_especie <- datos_uso %>% #Crea el nombre común más frecuente por especie, para etiquetas de las gráficas
  filter(!is.na(`ID Especie`),!is.na(`Nombre Común`)) %>%
  count(`ID Especie`, `Nombre Común`, name = "n_nombre_comun") %>%
  arrange(`ID Especie`, desc(n_nombre_comun), `Nombre Común`) %>%
  group_by(`ID Especie`) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(`ID Especie`, `Nombre Común`)

#**************************
## Información adicional----
#**************************
###Proporción de registros de uso por cada grupo biológico, año y periodo climático----
proporcion_grupo_biologico <- datos_uso %>%
  filter(!is.na(fecha_marca_clase),!is.na(anio),!is.na(periodo_climatico),!is.na(marca_clase),!is.na(periodo_grafica),!is.na(`Grupo Biológico`)) %>% #No tiene en cuenta filas vacías
  count(fecha_marca_clase,anio,periodo_climatico,marca_clase,periodo_grafica,`Grupo Biológico`,name = "N_g") %>% # Cuenta los registros de uso por grupo biológico y periodo climático
  mutate( # Calcula la proporción de cada grupo respecto al total del periodo
    N_total = sum(N_g), #N_g es el número de registros de uso del grupo biológico g en el periodo climáico
    prop = N_g / N_total,
    prop_pct = 100 * prop,
    .by = fecha_marca_clase
  ) %>%
  arrange(fecha_marca_clase,desc(prop)) # Ordenar cronológicamente y de mayor a menor proporción
proporcion_grupo_biologico

#****************************************************************************
# Cálculo de indicador Uso relativo de especies por grupo biológico (IUR)----
#****************************************************************************
IUR_especie_grupo_biologico <- datos_uso %>%
  filter(!is.na(fecha_marca_clase),!is.na(periodo_climatico),!is.na(marca_clase),!is.na(periodo_grafica),!is.na(`Grupo Biológico`),!is.na(`ID Especie`),!is.na(`Nombre Científico`)) %>%  
  count(fecha_marca_clase,anio,periodo_climatico,marca_clase,periodo_grafica,`Grupo Biológico`,`ID Especie`,`Nombre Científico`,name = "U_s_g_t") %>% # Cuenta los registros de uso de cada especie por grupo y periodo climático
  left_join(nombre_comun_especie,by = "ID Especie") %>% #Agrega el nombre común de cada especie
  mutate( #Calcula el indicador
    U_g_t = sum(U_s_g_t), #U_s_g_t=número de registros de uso de la especie s, perteneciente al grupo g, durante el periodo t, U_g_t: total de registros de uso del grupo biológico g durante el periodo t
    IUR = U_s_g_t / U_g_t,
    IUR_pct = 100 * IUR,
    .by = c(
      fecha_marca_clase,
      `Grupo Biológico`
    )
  ) %>%
  relocate(`Nombre Común`,.after = `Nombre Científico`) %>% #organiza de mayor a menor IUR
  arrange(fecha_marca_clase,`Grupo Biológico`,desc(IUR))

IUR_especie_grupo_biologico

#********************************************************
## Desagregaciones temáticas opcionales del indicador ----
#********************************************************
### Desagregación general del IUR por grupo biológico y macrohábitat ----
unique(datos_uso$`Espacio de uso (Macrohábitat)`)
datos_uso_macrohabitat <- datos_uso %>%
  mutate( #Conserva valores originales
    id_registro_uso = row_number(),macrohabitat_original =`Espacio de uso (Macrohábitat)`,
    `Espacio de uso (Macrohábitat)` = str_split(as.character(`Espacio de uso (Macrohábitat)`),"\\s*[|,]\\s*")  # Separa las respuestas múltiples delimitadas por "|" o ","
  ) %>%
  unnest_longer(`Espacio de uso (Macrohábitat)`  # Crea una fila por cada macrohábitat reportado
  ) %>%
  mutate(`Espacio de uso (Macrohábitat)` = str_squish(`Espacio de uso (Macrohábitat)`), # Limpia y estandariza los nombres de los macrohábitats
    macrohabitat_minuscula = str_to_lower(`Espacio de uso (Macrohábitat)`),
    `Espacio de uso (Macrohábitat)` = case_when(
      macrohabitat_minuscula == "potrero" ~ "Potrero",
      macrohabitat_minuscula == "huerta o cultivo" ~ "Huerta o cultivo",
      macrohabitat_minuscula == "patio" ~ "Patio",
      macrohabitat_minuscula == "bosque" ~ "Bosque",
      macrohabitat_minuscula == "rastrojo" ~ "Rastrojo",
      macrohabitat_minuscula == "caño" ~ "Caño",
      macrohabitat_minuscula == "montaña" ~ "Montaña",
      macrohabitat_minuscula == "río guayabero" ~ "Río Guayabero",
      macrohabitat_minuscula == "sabana" ~ "Sabana",
      macrohabitat_minuscula == "represa" ~ "Represa",
      macrohabitat_minuscula == "otro" ~ "Otro",
      TRUE ~ `Espacio de uso (Macrohábitat)`
    )
  ) %>%
  select(-macrohabitat_minuscula)
datos_uso_macrohabitat %>%
  count(`Espacio de uso (Macrohábitat)`,sort = TRUE,name = "Numero_registros")

IUR_grupo_macrohabitat_general <- datos_uso_macrohabitat %>% ## Calcula el IUR para cada especie por grupo biológico y macrohábitat para todo el periodo de estudio
  filter(!is.na(`Grupo Biológico`),!is.na(`Espacio de uso (Macrohábitat)`),`Espacio de uso (Macrohábitat)` != "",!is.na(`ID Especie`),!is.na(`Nombre Científico`)
  ) %>%
  count(`Grupo Biológico`,`Espacio de uso (Macrohábitat)`,`ID Especie`,`Nombre Científico`,name = "U_s_g_m"
  ) %>%
  left_join(nombre_comun_especie,by = "ID Especie"
  ) %>%
  mutate(
    U_g_m = sum(U_s_g_m),
    IUR = U_s_g_m / U_g_m,
    IUR_pct = 100 * IUR,
    .by = c(`Grupo Biológico`,`Espacio de uso (Macrohábitat)`)
  ) %>%
  arrange(`Grupo Biológico`,`Espacio de uso (Macrohábitat)`,desc(IUR)
  ) %>%
  select(`Grupo Biológico`,`Espacio de uso (Macrohábitat)`,`ID Especie`,`Nombre Científico`,`Nombre Común`,U_s_g_m,U_g_m,IUR,IUR_pct)

IUR_grupo_macrohabitat_general

### Desagregación general del IUR de plantas por categoría de uso ----
IUR_plantas_categoria_uso_general <- datos_uso %>%
  filter(`Grupo Biológico` == "Plantas",!is.na(`Categorías de uso`),!is.na(`ID Especie`),!is.na(`Nombre Científico`)
  ) %>%
  mutate(`Categorías de uso` = case_when(tolower(trimws(as.character(`Categorías de uso`))) == "medicinal" ~ "Medicinal",TRUE ~ trimws(as.character(`Categorías de uso`))    )
  ) %>%
  count(`Grupo Biológico`,`Categorías de uso`,`ID Especie`,`Nombre Científico`, name = "U_s_g_c") %>%
  left_join(nombre_comun_especie,by = "ID Especie"
  ) %>%
  mutate(
    U_g_c = sum(U_s_g_c),
    IUR = U_s_g_c / U_g_c,
    IUR_pct = 100 * IUR,
    .by = c(`Grupo Biológico`,`Categorías de uso`)
  ) %>%
  arrange(`Categorías de uso`,desc(IUR)
  ) %>%
  select(`Grupo Biológico`,`Categorías de uso`,`ID Especie`,`Nombre Científico`,`Nombre Común`,U_s_g_c,U_g_c,IUR,IUR_pct  )

IUR_plantas_categoria_uso_general

#**************************
# Guardar resultados-----
#**************************
nombres_resultados <- ls(pattern = "^(IUR_|proporcion_)") # Identifica los objetos con nombre IUR o proporción
resultados_excel <- mget(nombres_resultados)
resultados_excel <- resultados_excel[sapply(resultados_excel, is.data.frame)]
archivo_salida <- file.path(dir_resultados,paste0("Resultados_IUR_FPVA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
write_xlsx(resultados_excel,path = archivo_salida) # Exporta cada tabla en una hoja independiente del archivo Excel

#***********************************
# Propuesta de salidas gráficas ----
#***********************************
## Gráfica de la proporción de registros de uso por cada grupo biológico, año y periodo climático----
datos_grafica_grupos <- proporcion_grupo_biologico %>% #Prepara los datos
  arrange(fecha_marca_clase) %>%
  mutate(periodo_grafica = factor(as.character(periodo_grafica),levels = unique(as.character(periodo_grafica)),ordered = TRUE))

grafica_proporcion_grupos <- ggplot( #genera la gráfica 
  datos_grafica_grupos,aes(x = periodo_grafica,y = prop_pct,fill = `Grupo Biológico`)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = if_else(prop_pct >= 4,paste0(round(prop_pct, 1), "%"),"")),
    position = position_stack(vjust = 0.5),
    color = "white",fontface = "bold",size = 3.2) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(
    values = c("Plantas"= "#2d5a27","Peces"= "#1b365d","Mamíferos" = "#a0522d"    ),
    breaks = c("Plantas","Peces","Mamíferos")) +
  labs(
    title = "Proporción de registros de uso por grupo biológico y periodo climático",
    x = "Periodo climático",
    y = "Proporción de registros (%)",
    fill = "Grupo biológico"
  ) +
  guides(fill = guide_legend(nrow = 1,byrow = TRUE)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold",size = 14,hjust = 0,margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45,hjust = 1),
    panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 10,r = 10,b = 10,l = 10)
  )

grafica_proporcion_grupos

##Gráfica del top 10 del indicador Uso relativo de especies por grupo biológico (IUR)----
graficar_top10_IUR_periodo <- function(datos_iur,grupo,top_n = 10) 
  {
    datos_base <- datos_iur %>%
    filter(`Grupo Biológico` == grupo,!is.na(IUR_pct),!is.na(`Nombre Científico`)) %>%
    mutate(
      especie_grafica = if_else(
        is.na(`Nombre Común`) | str_squish(as.character(`Nombre Común`)) == "",
        as.character(`Nombre Científico`),
        paste0(str_squish(as.character(`Nombre Común`)),"\n",`Nombre Científico`))
    )
  top_especies <- datos_base %>% #Identifica el Top 10 por periodo
    group_by(fecha_marca_clase, periodo_grafica) %>%
    arrange(desc(IUR_pct), `Nombre Científico`) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    select(fecha_marca_clase, periodo_grafica, especie_grafica, IUR_pct)
    residuos <- datos_base %>%   # Calcula "Otras especies" para alcanzar el 100%
    anti_join(top_especies, by = c("fecha_marca_clase", "periodo_grafica", "especie_grafica")) %>%
    group_by(fecha_marca_clase, periodo_grafica) %>%
    summarise(IUR_pct = sum(IUR_pct, na.rm = TRUE), .groups = 'drop') %>%
    filter(IUR_pct > 0) %>%
    mutate(especie_grafica = "Otras especies")
  datos_grafica <- bind_rows(top_especies, residuos) %>% #Consolida datos para la gráfica
    arrange(fecha_marca_clase) %>%
    mutate(periodo_grafica = factor(as.character(periodo_grafica),levels = rev(unique(as.character(periodo_grafica))),ordered = TRUE))
  especies_unicas <- unique(datos_grafica$especie_grafica[datos_grafica$especie_grafica != "Otras especies"])
  n_especies <- length(especies_unicas)
  colores_base <- colorRampPalette(c("#1b365d", "#4b6b94", "#2d5a27", "#556b2f", "#8b7355", "#a0522d", "#4a2c5a"))(n_especies)
  names(colores_base) <- especies_unicas
  if ("Otras especies" %in% datos_grafica$especie_grafica) {
    niveles_especies <- c(especies_unicas, "Otras especies")
    colores_completos <- c(colores_base, "Otras especies" = "#D9D9D9") 
  } else {
    niveles_especies <- especies_unicas
    colores_completos <- colores_base
  }
  datos_grafica <- datos_grafica %>%
    mutate(especie_grafica = factor(especie_grafica, levels = niveles_especies))
  
  ggplot(datos_grafica,
    aes(x = periodo_grafica,y = IUR_pct,fill = especie_grafica)) +
    geom_col(width = 0.7,position = position_stack(reverse = TRUE) ) +
    geom_text(
      aes(label = if_else(IUR_pct >= 2.5,scales::number(IUR_pct,accuracy = 0.1,decimal.mark = ",",suffix = "%"),"")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = if_else(datos_grafica$especie_grafica == "Otras especies", "black", "white"), 
      size = 3
    ) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 100, 20),limits = c(0, 100.1), labels = scales::label_number(suffix = "%"),expand = expansion(mult = c(0, 0.03))
    ) +
    scale_fill_manual(values = colores_completos,breaks = niveles_especies) +
    labs(
      title = paste0("Uso relativo de especies por grupo biológico (IUR) - ", grupo),
      x = "Periodo climático",
      y = "IUR (%)",
      caption = paste("Se muestran las diez especies con mayor IUR en cada periodo climático.","\nLas especies restantes se agrupan en la categoría 'Otras especies'."
      ),
      fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(color = "grey30",size = 8.5,hjust = 0))
}
grafica_mamiferos <- graficar_top10_IUR_periodo(IUR_especie_grupo_biologico, "Mamíferos")
grafica_peces     <- graficar_top10_IUR_periodo(IUR_especie_grupo_biologico, "Peces")
grafica_plantas   <- graficar_top10_IUR_periodo(IUR_especie_grupo_biologico, "Plantas")

grafica_mamiferos
grafica_peces
grafica_plantas

##Gráfica del top 10 del IUR por grupo biológico en paneles ----
graficar_top10_IUR_facet <- function(datos_iur,grupo,top_n = 10) 
  {
  datos_base <- datos_iur %>%
    filter(`Grupo Biológico` == grupo,!is.na(IUR_pct),!is.na(`Nombre Científico`))
  if (nrow(datos_base) == 0) {stop(paste0("No se encontraron datos para el grupo biológico '", grupo, "'."))}
  datos_grafica <- datos_base %>% #Selección de las diez especies con mayor IUR por periodo y formatear etiquetas
    mutate(
      especie_grafica = if_else(is.na(`Nombre Común`) | str_squish(as.character(`Nombre Común`)) == "",as.character(`Nombre Científico`),
        paste0(str_squish(as.character(`Nombre Común`)),"\n", `Nombre Científico`))
    ) %>%
    arrange(fecha_marca_clase,desc(IUR_pct),`Nombre Científico`
    ) %>%
    group_by(fecha_marca_clase, periodo_grafica) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    arrange(fecha_marca_clase) %>%
    mutate(periodo_grafica = factor(periodo_grafica,levels = unique(periodo_grafica)),
      especie_panel = paste0(especie_grafica, "___", periodo_grafica),
      especie_panel = reorder(especie_panel, IUR_pct)
    )
  
  especies_color <- sort(unique(datos_grafica$especie_grafica))
  n_especies <- length(especies_color)
  paleta_especies <- colorRampPalette(c("#1b365d", "#4b6b94", "#2d5a27", "#556b2f", "#8b7355", "#a0522d", "#4a2c5a"))(n_especies)
  names(paleta_especies) <- especies_color
  
  grafica <- ggplot(datos_grafica,
    aes(x = IUR_pct,y = especie_panel,fill = especie_grafica)) +
    geom_col(width = 0.75,show.legend = FALSE) +
    geom_text(aes(label = paste0(round(IUR_pct, 1), "%")),hjust = -0.15,color = "grey20",size = 3,show.legend = FALSE) +
    facet_wrap(~ periodo_grafica,scales = "free_y",ncol = 3) +
    scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = scales::label_number(suffix = "%"),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_y_discrete(labels = ~ sub("___.*$", "", .x)) +
    scale_fill_manual(values = paleta_especies) +
    labs(
      title = paste("Uso relativo de especies -", grupo),
      x = "IUR (%)",
      y = NULL,
      caption = paste("Se muestran únicamente las diez especies con mayor IUR en cada periodo climático.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(1.2, "lines"),
      legend.position = "none",
      plot.caption = element_text(color = "grey30", hjust = 0)
    )
  return(grafica)
}

grafica_mamiferos2<- graficar_top10_IUR_facet(IUR_especie_grupo_biologico,"Mamíferos")
grafica_peces2 <- graficar_top10_IUR_facet(IUR_especie_grupo_biologico,"Peces")
grafica_plantas2 <- graficar_top10_IUR_facet(IUR_especie_grupo_biologico,"Plantas")
grafica_mamiferos2
grafica_peces2 
grafica_plantas2

##curva de dominancia acumulada para indicador Uso relativo de especies por grupo biológico (IUR)----
dominancia_acum_periodo <- IUR_especie_grupo_biologico %>%
  arrange(`Grupo Biológico`,fecha_marca_clase,desc(IUR_pct),`Nombre Científico`
  ) %>%
  group_by(`Grupo Biológico`,fecha_marca_clase,periodo_grafica
  ) %>%
  mutate(rango = row_number(),IUR_acum = cumsum(IUR_pct)
  ) %>%
  ungroup() %>%
  arrange(fecha_marca_clase) %>%
  mutate(periodo_grafica = factor(as.character(periodo_grafica),levels = unique(as.character(periodo_grafica)),ordered = TRUE))

grafica_dominancia_acum_periodo <- ggplot(dominancia_acum_periodo,
  aes(
    x = rango,
    y = IUR_acum,
    color = periodo_grafica,
    group = interaction(`Grupo Biológico`, periodo_grafica)
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ `Grupo Biológico`, ncol = 1, scales = "free_x") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100)
  ) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D", begin = 0.1, end = 0.85) +
  labs(
    x = "Número de especies",
    y = "IUR acumulado (%)",
    color = "Periodo de monitoreo",
    title = "Curva de dominancia acumulada de uso",
    subtitle = "Por grupo biológico y periodo de monitoreo"
  ) +
  guides(color = guide_legend(byrow = TRUE,override.aes = list(linewidth = 1.2,size = 2))) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(lineheight = 0.75,margin = margin(b = 6)),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
grafica_dominancia_acum_periodo

##Grafica del top 10 desagregación general del IUR por grupo biológico y macrohábitat ----

graficar_top10_IUR_macrohabitat <- function(datos_iur,grupo_sel,top_n = 10,tamano_eje_y = 8,tamano_porcentaje = 2.8)
  {
    grupos_disponibles <- datos_iur %>%
    distinct(`Grupo Biológico`) %>%
    filter(!is.na(`Grupo Biológico`)) %>%
    pull(`Grupo Biológico`)
    if (!grupo_sel %in% grupos_disponibles) {
    stop(paste0("El grupo biológico '",grupo_sel,"' no se encuentra en los datos. ","Grupos disponibles: ",paste(grupos_disponibles, collapse = ", ")))
  }
  
datos_grafica <- datos_iur %>%
    filter(`Grupo Biológico` == grupo_sel
    ) %>%
    mutate(`Espacio de uso (Macrohábitat)` = trimws(as.character(`Espacio de uso (Macrohábitat)`))
    ) %>%
    filter(!is.na(`Espacio de uso (Macrohábitat)`),`Espacio de uso (Macrohábitat)` != "",tolower(`Espacio de uso (Macrohábitat)`) != "otro",
      !is.na(IUR_pct),!is.na(`Nombre Científico`)) %>%
    mutate(especie_grafica = if_else(is.na(`Nombre Común`) |trimws(as.character(`Nombre Común`)) == "",as.character(`Nombre Científico`),
        paste0(trimws(as.character(`Nombre Común`)),"\n",trimws(as.character(`Nombre Científico`))))
    ) %>%
    arrange(`Espacio de uso (Macrohábitat)`,desc(IUR_pct),`Nombre Científico`
    ) %>%
    group_by(`Espacio de uso (Macrohábitat)`) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(especie_panel = paste0(especie_grafica,"___",`Espacio de uso (Macrohábitat)`),
      especie_panel = reorder(especie_panel,IUR_pct))
  
  if (nrow(datos_grafica) == 0) {stop(paste0("No hay datos válidos de macrohábitat para el grupo '",grupo_sel,"'."))}
  
  especies_unicas <- sort(unique(datos_grafica$especie_grafica))
  n_especies <- length(especies_unicas)
  colores_base <- colorRampPalette(c("#1b365d","#4b6b94","#2d5a27","#556b2f","#8b7355","#a0522d","#4a2c5a"))(n_especies)
  names(colores_base) <- especies_unicas
  
  ggplot(datos_grafica,
    aes(
      x = IUR_pct,
      y = especie_panel,
      fill = especie_grafica
    )
  ) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(IUR_pct, 1),"%")),
      hjust = -0.15,color = "grey20",size = tamano_porcentaje,show.legend = FALSE) +
    facet_wrap(~ `Espacio de uso (Macrohábitat)`,scales = "free_y",
      ncol = 2,labeller = labeller(.default = ggplot2::label_wrap_gen(width = 22))) +
    scale_x_continuous(breaks = seq(0, 100, 20),labels = scales::label_number(suffix = "%"),
      limits = c(0, NA),expand = expansion(mult = c(0, 0.18))) +
    scale_y_discrete(labels = ~ sub("___.*$","",.x)) +
    scale_fill_manual(values = colores_base) +
    labs(
      title = paste0("Uso relativo de especies de ",tolower(grupo_sel)," por macrohábitat"),
      subtitle = "Cálculo acumulado para todo el periodo de monitoreo",
      caption = "Se muestran únicamente las diez especies con mayor IUR.",
      x = "IUR (%)",
      y = NULL,
      fill = "Especie"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold",size = 14,hjust = 0),
      plot.subtitle = element_text(color = "grey30",hjust = 0),
      strip.text = element_text(face = "bold",size = 10),
      strip.background = element_blank(),
      axis.text.y = element_text(size = tamano_eje_y,color = "grey25",lineheight = 0.85,margin = margin(r = 6)),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.x = grid::unit(1.5,"lines"),
      panel.spacing.y = grid::unit(1,"lines"),
      legend.position = "none",
      plot.margin = margin(t = 10,r = 25,b = 10,l = 10),
      plot.caption = element_text(color = "grey30",size = 8.5,hjust = 0)
    )
}


grafica_macrohabitat_mamiferos <-graficar_top10_IUR_macrohabitat(datos_iur = IUR_grupo_macrohabitat_general,grupo_sel = "Mamíferos")
grafica_macrohabitat_peces <-graficar_top10_IUR_macrohabitat(datos_iur = IUR_grupo_macrohabitat_general,grupo_sel = "Peces")
plantilla_macrohabitat_plantas <-graficar_top10_IUR_macrohabitat(datos_iur = IUR_grupo_macrohabitat_general,grupo_sel = "Plantas",tamano_eje_y = 9,tamano_porcentaje = 3)

macrohabitats_plantas_1 <- c("Bosque","Caño","Huerta o cultivo","Montaña")
macrohabitats_plantas_2 <- c("Patio","Potrero","Rastrojo","Represa","Sabana")
datos_macrohabitat_plantas_1 <-plantilla_macrohabitat_plantas$data %>%
  filter(`Espacio de uso (Macrohábitat)` %in%macrohabitats_plantas_1) %>%
  mutate(`Espacio de uso (Macrohábitat)` = factor(`Espacio de uso (Macrohábitat)`,levels = macrohabitats_plantas_1))
datos_macrohabitat_plantas_2 <-plantilla_macrohabitat_plantas$data %>%
  filter(`Espacio de uso (Macrohábitat)` %in%macrohabitats_plantas_2) %>%
  mutate(`Espacio de uso (Macrohábitat)` = factor(`Espacio de uso (Macrohábitat)`,levels = macrohabitats_plantas_2))
grafica_macrohabitat_plantas_1 <-plantilla_macrohabitat_plantas %+%datos_macrohabitat_plantas_1
grafica_macrohabitat_plantas_1 <-grafica_macrohabitat_plantas_1 +labs(title = paste0("Uso relativo de especies de plantas ","por macrohábitat (1 de 2)"))
grafica_macrohabitat_plantas_2 <-plantilla_macrohabitat_plantas %+%datos_macrohabitat_plantas_2
grafica_macrohabitat_plantas_2 <-grafica_macrohabitat_plantas_2 +labs(title = paste0("Uso relativo de especies de plantas ","por macrohábitat (2 de 2)"))

grafica_macrohabitat_mamiferos
grafica_macrohabitat_peces
grafica_macrohabitat_plantas_1
grafica_macrohabitat_plantas_2

## Gráfica del IUR general de plantas por categoría de uso ----

if (!exists("IUR_plantas_categoria_uso_general")) {stop(paste0("No existe el objeto 'IUR_plantas_categoria_uso_general'. ","Ejecuta primero la sección donde se calcula el IUR de plantas ","por categoría de uso."))}

datos_grafica_plantas_categoria_general <- IUR_plantas_categoria_uso_general %>%
  mutate(`Categorías de uso` = trimws(as.character(`Categorías de uso`)),
    especie_grafica = if_else(is.na(`Nombre Común`) | trimws(as.character(`Nombre Común`)) == "",trimws(as.character(`Nombre Científico`)),
      paste0(trimws(as.character(`Nombre Común`)),"\n",trimws(as.character(`Nombre Científico`))))
  ) %>%
  filter(!is.na(`Categorías de uso`),`Categorías de uso` != "",!is.na(`Nombre Científico`),trimws(as.character(`Nombre Científico`)) != "",!is.na(IUR_pct)) %>%
  arrange(`Categorías de uso`,desc(IUR_pct),`Nombre Científico`) %>%
  group_by(`Categorías de uso`) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(especie_panel = paste0(especie_grafica,"___",`Categorías de uso`),
    especie_panel = reorder(especie_panel,IUR_pct))
  especies_unicas <- sort(unique(datos_grafica_plantas_categoria_general$especie_grafica))
  n_especies <- length(especies_unicas)

  if (n_especies == 0) {stop("No hay especies válidas para construir las gráficas.")}
  colores_categorias_plantas <- colorRampPalette(c("#1b365d","#4b6b94","#2d5a27","#556b2f","#8b7355","#a0522d","#4a2c5a"))(n_especies)
  names(colores_categorias_plantas) <- especies_unicas
  categorias_1 <- c("Alimento para animales","Alimento para humanos","Artesanal","Combustible")
  categorias_2 <- c("Cuidados Personales","Cultural","Material","Medicinal","Ornamental")

crear_grafica_categorias <- function(datos,categorias,parte,paleta) 
  {
  datos_parte <- datos %>%
    filter(`Categorías de uso` %in% categorias) %>%
    mutate(`Categorías de uso` = factor(`Categorías de uso`,levels = categorias))
  if (nrow(datos_parte) == 0) {stop(paste0("No se encontraron datos para las categorías de la parte ",parte,"."))}
  ggplot(datos_parte,aes(x = IUR_pct,y = especie_panel,fill = especie_grafica)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = paste0(round(IUR_pct, 1),"%")),
      hjust = -0.15,color = "grey20",size = 3,show.legend = FALSE) +
    facet_wrap(~ `Categorías de uso`,scales = "free_y",ncol = 2,labeller = labeller(.default = ggplot2::label_wrap_gen(width = 20))) +
    scale_x_continuous(breaks = seq(0, 100, 20),labels = scales::label_number(suffix = "%"),
      limits = c(0, NA),expand = expansion(mult = c(0, 0.18))) +
    scale_y_discrete(labels = ~ sub("___.*$","",.x)) +
    scale_fill_manual(values = paleta) +
    labs(title = paste0("Uso relativo de especies de plantas ","por categoría de uso (",parte," de 2)"),
      subtitle = paste0("Cálculo acumulado para todo el ","periodo de monitoreo"),x = "IUR (%)",y = NULL,fill = "Especie",
      caption = paste0("Se muestran únicamente las diez ","especies con mayor IUR.")) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold",size = 14,hjust = 0),
      plot.subtitle = element_text(color = "grey30",hjust = 0),
      strip.text = element_text(face = "bold",size = 10),
      strip.background = element_blank(),
      axis.text.y = element_text(size = 8,color = "grey25",lineheight = 0.85,margin = margin(r = 8)),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.x = grid::unit(1.5,"lines"),
      panel.spacing.y = grid::unit(1.5,"lines"),
      legend.position = "none",
      plot.caption = element_text(color = "grey30",size = 8.5,hjust = 0),
      plot.margin = margin(t = 10,r = 20,b = 10,l = 10))
  }
grafica_plantas_categoria_general_1 <-crear_grafica_categorias(datos = datos_grafica_plantas_categoria_general,categorias = categorias_1,parte = 1,paleta = colores_categorias_plantas)
grafica_plantas_categoria_general_2 <-crear_grafica_categorias(datos = datos_grafica_plantas_categoria_general,categorias = categorias_2,parte = 2,paleta = colores_categorias_plantas)

grafica_plantas_categoria_general_1
grafica_plantas_categoria_general_2

#***********************************
# Guardar salidas gráficas ----
#***********************************

nombres_graficas <- ls(pattern = "^grafica_") #Identifica los objetos llamados grafica
graficas <- mget(nombres_graficas)
graficas <- graficas[sapply(graficas, function(x) inherits(x, "ggplot"))]

dimensiones_especiales <- list( #Define las dimensiones específicas para gráficas específicas, para las demás el tamaño por defecto de 9 x 5.5
  "grafica_plantas2" = list(w = 9, h = 9),
  "grafica_macrohabitat_plantas_1"= list(w = 9, h = 7),
  "grafica_macrohabitat_plantas_2"= list(w = 9, h = 10),
  "grafica_macrohabitat_peces"= list(w = 9, h = 7),
  "grafica_plantas_categoria_general_1" = list(w = 9, h = 7),
  "grafica_plantas_categoria_general_2" = list(w = 9, h = 10),
  "grafica_plantas" = list(w = 10, h = 6.5)
)

for (nombre in names(graficas)) 
  {
  if (nombre %in% names(dimensiones_especiales)) {
    dims <- dimensiones_especiales[[nombre]]} 
  else {dims <- list(w = 9, h = 5.5)}
  ggsave(filename = file.path(dir_graficas,paste0(nombre,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".png")),
    plot = graficas[[nombre]],width = dims$w,height = dims$h,units = "in",dpi = 300,bg = "white")
}

