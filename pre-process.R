# cargar librerias para el preprocesamiento de datos
library(tidyverse)
library(lubridate)

# cargamos la lista de datos y la convertimos en un dataframe
archivos <- list.files("data/modis", full.names = T)
incendios_bol <- list()
incendios_bol <- lapply(archivos, read_csv)
incendios_bol <- bind_rows(incendios_bol, .id = "column_label")

# filtramos el dataframe
incendios_bol <- incendios_bol %>% 
  filter(satellite == "Aqua" # se usa solo un satelite para evitar sobre-escritura de datos
         & daynight == "D"  #se trabaja con los registros de dia solamente
         & confidence >= 50 # se trabaja con los registros superiores al 50% de seguridad
         & type == 0) %>% # trabajamos solamente con aquellos que el satelite asume como incendios forestales
  mutate(year = year(acq_date),
         celsius = (brightness - 273.15),
         mes = month(acq_date),
         decil = ntile(celsius, 10)) # # temperatura de kelvin a celsius
  

# guardamos la informacion para la app
save(incendios_bol, file = "output/incendios_bol.RData")
write.csv(incendios_bol, file = "output/incendios_2000_2021.csv")

