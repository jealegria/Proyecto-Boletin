# Filtrar las semanas de la 10 a la 15 del año 2023
tabla.egresos <- base %>%
  filter(Semana >= 10, Semana <= 15, lubridate::year(Fecha) == 2023) %>%
  mutate(Semana_Servicio = paste0("Semana ", Semana, " ", Servicio)) %>%
  group_by(`Tipo de egreso`, Semana_Servicio) %>%
  summarize(Conteo = n(), .groups = "drop") %>%
  pivot_wider(names_from = Semana_Servicio, values_from = Conteo, values_fill = 0)

# Mostrar el resultado
#print(tabla.egresos)




################################################

#Crear tabla de tipos de egresos por semana

# Crear la tabla gt
tabla_gt_2 <- tabla.egresos %>%
  gt() %>%
  tab_header(title = "Egresos por Semana")  # Título de la tabla

# Extraer las semanas disponibles y asegurarse de que están en el formato correcto
semanas <- unique(gsub("Semana (\\d+).*", "\\1", colnames(tabla.egresos)[-1]))  # Extraer solo los números de semana

# Agregar tab_spanner de forma dinámica y configurar etiquetas para las columnas Adultos y Pediatria directamente
for (semana in semanas) {
  cols_to_include <- c(paste("Semana", semana, "Adultos"), paste("Semana", semana, "Pediatria"))
  
  # Agregar el spanner con el nombre de la semana
  tabla_gt_2 <- tabla_gt_2 %>%
    tab_spanner(
      label = paste("Semana", semana), 
      columns = cols_to_include  # Seleccionamos las columnas Adultos y Pediatria de la semana
    )
  
  # Renombrar las columnas Adultos y Pediatria
  tabla_gt_2 <- tabla_gt_2 %>%
    cols_label(
      !!sym(cols_to_include[1]) := "Adultos",  # Renombrar la columna de Adultos
      !!sym(cols_to_include[2]) := "Pediatria"   # Renombrar la columna de Pediatria
    )
}

# Mostrar la tabla formateada
#tabla_gt_2



#########################################

tabla_edit <- tabla_gt_2 %>% 
  tab_options(
    table.font.size = px(10), # Tamaño de la fuente de la tabla
    column_labels.font.size = px(10), # Tamaño de la fuente de las etiquetas de las columnas
    heading.title.font.size = px(10), # Tamaño de la fuente del título
    heading.subtitle.font.size = px(8) # Tamaño de la fuente del subtítulo
  ) %>%
  tab_options(
    data_row.padding = px(2), # Espaciado de las filas de datos
    heading.padding = px(2), # Espaciado del encabezado
    column_labels.padding = px(2) # Espaciado de las etiquetas de las columnas
  )  %>%
  
  # Alinear la primera columna a la izquierda
  cols_align(align = "left", columns = `Tipo de egreso`) %>%
  # Alinear todas las demás columnas a la derecha
  cols_align(align = "center", columns = c(contains("1"), contains("2"), contains("3"), contains("4"), contains("5"))) %>%
  # Aplicar color de fondo a las columnas `A` y `P`
  tab_style(
    style = list(
      cell_fill(color = "#D9EAD3")                   # Color de fondo verde claro para las columnas `Adultos`
    ),
    locations = cells_body(columns = contains("Adultos"))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#CCE5FF")                   # Color de fondo celeste claro para las columnas `Pediatria`
    ),
    locations = cells_body(columns = contains("Pediatria"))
  )

