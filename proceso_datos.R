# Agregar columna "Servicio"
datos2023.a$Servicio <- "Adultos"
datos2024.a$Servicio <- "Adultos"
datos2023.p$Servicio <- "Pediatria"
datos2024.p$Servicio <- "Pediatria"

# Unir los dataframes en uno solo llamado base
base <- rbind(datos2023.a, datos2024.a, datos2023.p, datos2024.p)


# Cambiar el nombre de la columna "Fecha de ingreso" a "Fecha"
colnames(base)[colnames(base) == "Fecha de ingreso"] <- "Fecha"


# Convertir la columna fecha a solo la fecha (sin la hora)
base <- base %>%
  mutate(Fecha = as.Date(Fecha))

tabla.se <- tabla.se %>% 
  mutate(Fecha = as.Date(Fecha))

#Unir Semana Epidemiologica a la base usando left join con pk Fecha

base <- left_join(base, tabla.se[, c("Fecha", "Semana")], by = "Fecha")


