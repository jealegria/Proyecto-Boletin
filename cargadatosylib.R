#Carga de librerias

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(gt)
library(ggplot2)

#Carga de bases raw

datos2023.a <- read_excel("bases_raw/guardia_adultos_2023.xlsx")
datos2024.a <- read_excel("bases_raw/guardia_adultos_2024.xlsx")
datos2023.p <- read_excel("bases_raw/guardia_pediatria_2023.xlsx")
datos2024.p <- read_excel("bases_raw/guardia_pediatria_2024.xlsx")

#Carga de tabla relacional

tabla.se <- read_excel("rtable/Tabla Calendario Semana Epidemiologica.xlsx")

