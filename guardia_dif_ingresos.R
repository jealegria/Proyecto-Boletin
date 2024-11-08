
# Filtrar las semanas de interés y los años 2023 y 2024
semanas_interesadas <- 10:15

base_filtrada <- base %>%
  filter(Semana %in% semanas_interesadas & (year(Fecha) == 2024 | year(Fecha) == 2023))

# Contar la cantidad de ingresos por Semana y Año
ingresos_por_semana <- base_filtrada %>%
  mutate(Año = year(Fecha)) %>%  # Aseguramos que el año se extraiga correctamente de la columna Fecha
  group_by(Año, Semana) %>%
  summarise(Cantidad_ingresos = n(), .groups = "drop")  # Contamos los ingresos (filas)

# Ver el resultado de los ingresos por semana y año
#ingresos_por_semana



# Separar los ingresos de 2024 y 2023
ingresos_2024 <- ingresos_por_semana %>% filter(Año == 2024)
ingresos_2023 <- ingresos_por_semana %>% filter(Año == 2023)

# Unir los datos de 2024 y 2023 por la columna 'Semana'
diferencia_ingresos <- ingresos_2024 %>%
  left_join(ingresos_2023, by = "Semana", suffix = c("_2024", "_2023")) %>%
  mutate(Diferencia = Cantidad_ingresos_2024 - Cantidad_ingresos_2023)


# Calcular la diferencia en porcentaje
diferencia_ingresos <- diferencia_ingresos %>%
  mutate(Porcentaje_cambio = (Diferencia / Cantidad_ingresos_2023) * 100)

# Ver el resultado de la diferencia en porcentaje
#diferencia_ingresos


############################################

# Eliminar las columnas Año_2024 y Año_2023
diferencia_ingresos <- diferencia_ingresos %>%
  select(-Año_2024, -Año_2023)

diferencia_ingresos <- diferencia_ingresos %>%
  mutate(Porcentaje_cambio = paste0(round(Porcentaje_cambio, 0), "%"))  # Redondear y agregar el símbolo %

tabla_resultado <- diferencia_ingresos %>%
  gt() %>%
  tab_header(
    title = "Comparación de Ingresos entre 2024 y 2023 por Semana"
  ) %>%
  cols_label(
    Semana = "Semana",
    Cantidad_ingresos_2024 = "2024",
    Cantidad_ingresos_2023 = "2023",
    Diferencia = "Diferencia",
    Porcentaje_cambio = "Cambio (%)"
  ) %>%
  tab_spanner(
    label = "Ingresos", 
    columns = c("Cantidad_ingresos_2024", "Cantidad_ingresos_2023")
  ) %>%
  tab_spanner(
    label = "Diferencia y Cambio",  
    columns = c("Diferencia", "Porcentaje_cambio")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),  # Color de fondo gris claro para todas las celdas
      cell_text(weight = "bold")  # Poner en negrita el texto
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(
    align = "center", 
    columns = c("Semana", "Cantidad_ingresos_2024", "Cantidad_ingresos_2023", "Diferencia", "Porcentaje_cambio")
  ) %>%
  # Establecer color verde claro para valores negativos y rojo claro para valores positivos
  tab_style(
    style = list(
      cell_text(color = "green")  # Color verde claro para los números negativos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio < 0  # Filtrar solo los negativos
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")  # Color rojo claro para los números positivos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio > 0  # Filtrar solo los positivos
    )
  )

# Mostrar la tabla formateada
#tabla_resultado



##############################################################

# Paso 1: Crear las columnas "Mes" y "Año" a partir de la columna "Fecha"
base$Mes <- format(base$Fecha, "%m")  # Extraer el mes como número
base$Año <- format(base$Fecha, "%Y")  # Extraer el año

# Paso 2: Reemplazar los números de mes por los nombres de los meses en español
meses_espanol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

base$Mes <- meses_espanol[as.numeric(base$Mes)]  # Reemplazar los números por los nombres en español

# Paso 3: Convertir la columna Mes a factor para garantizar el orden correcto
base$Mes <- factor(base$Mes, levels = meses_espanol)  # Ordenar los meses de enero a diciembre

# Paso 4: Agrupar los datos por "Mes" y "Año" y calcular la cantidad de ingresos por cada grupo
ingresos_mes <- base %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_ingresos = n()) %>%
  ungroup()

# Paso 5: Separar los datos para 2023 y 2024
ingresos_2023 <- ingresos_mes %>%
  filter(Año == "2023") %>%
  select(Mes, Cantidad_ingresos) %>%
  rename(Cantidad_ingresos_2023 = Cantidad_ingresos)

ingresos_2024 <- ingresos_mes %>%
  filter(Año == "2024") %>%
  select(Mes, Cantidad_ingresos) %>%
  rename(Cantidad_ingresos_2024 = Cantidad_ingresos)

# Paso 6: Unir los datos de 2023 y 2024
comparacion_mensual <- left_join(ingresos_2024, ingresos_2023, by = "Mes")

# Paso 7: Calcular la diferencia y el porcentaje de cambio
comparacion_mensual <- comparacion_mensual %>%
  mutate(
    Diferencia = Cantidad_ingresos_2024 - Cantidad_ingresos_2023,
    Porcentaje_cambio = (Diferencia / Cantidad_ingresos_2023) * 100
  )

# Paso 8: Redondear el porcentaje y agregar el símbolo de porcentaje
comparacion_mensual <- comparacion_mensual %>%
  mutate(Porcentaje_cambio = paste0(round(Porcentaje_cambio, 0), "%"))

# Paso 9: Crear la tabla con `gt`
tabla_resultado_mensual <- comparacion_mensual %>%
  gt() %>%
  tab_header(
    title = "Comparación de Ingresos entre 2024 y 2023 por Mes"
  ) %>%
  cols_label(
    Mes = "Mes",
    Cantidad_ingresos_2024 = "2024",
    Cantidad_ingresos_2023 = "2023",
    Diferencia = "Diferencia",
    Porcentaje_cambio = "Cambio (%)"
  ) %>%
  tab_spanner(
    label = "Ingresos", 
    columns = c("Cantidad_ingresos_2024", "Cantidad_ingresos_2023")
  ) %>%
  tab_spanner(
    label = "Diferencia y Cambio",  
    columns = c("Diferencia", "Porcentaje_cambio")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),  # Color de fondo gris claro para todas las celdas
      cell_text(weight = "bold")  # Poner en negrita el texto
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(
    align = "center", 
    columns = c("Mes", "Cantidad_ingresos_2024", "Cantidad_ingresos_2023", "Diferencia", "Porcentaje_cambio")
  ) %>%
  # Establecer color verde claro para valores negativos y rojo claro para valores positivos
  tab_style(
    style = list(
      cell_text(color = "green")  # Color verde claro para los números negativos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio < 0  # Filtrar solo los negativos
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")  # Color rojo claro para los números positivos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio > 0  # Filtrar solo los positivos
    )
  )

#tabla_resultado_mensual


##############################

# Suponiendo que tu dataset base tiene las columnas `Semana` y `Fecha` 
# y que `base` contiene los datos de ambos años (2023 y 2024)

# Agregar una columna para el Año (2023 o 2024)
base_con_year <- base %>%
  mutate(Año = year(Fecha))

# Contar la cantidad de ingresos por Semana y Año
ingresos_por_semana2 <- base_con_year %>%
  group_by(Año, Semana) %>%
  summarise(Cantidad_ingresos = n(), .groups = "drop")

# Crear gráfico con líneas de tendencia para 2023 y 2024
egresos_tendencia_semanal <- ggplot(ingresos_por_semana2, aes(x = Semana, y = Cantidad_ingresos, color = factor(Año))) +
  geom_line(size = 1) +  # Dibujar las líneas de tendencia
  labs(
    title = "Comparación de Ingresos por Semana Epidemiológica",
    x = "Semana Epidemiológica",
    y = "Cantidad de Ingresos",
    color = "Año"
  ) +
  theme_minimal() +  # Tema minimalista
  scale_color_manual(values = c("2023" = "blue", "2024" = "red"))  # Colores para las líneas
