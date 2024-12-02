# Cargar paquetes
library(readxl)
library(lubridate)
library(tidyverse)
library(scales)
library(xtable)

#Ejercicio 2

proyección <- read_excel("Pregunta2/pregunta2_Proyección.xlsx") %>% as.data.frame()

proyección$Fecha <- proyección$Fecha %>% as.Date() %>% format("%d/%m/%Y") %>% as.character()
proyección$`Predicción tipo de cambio` <- proyección$`Predicción tipo de cambio` %>% as.numeric()

proyección <- proyección %>%
  mutate(across(where(is.numeric), 
                ~ label_dollar(prefix = "₡", 
                               big.mark = ",", 
                               decimal.mark = ".",
                               accuracy = 0.01)(.)))

print(xtable(proyección), 
      include.rownames = FALSE,  # No incluir los nombres de las filas
      tabular.environment = "tabular",  # Usar 'tabular' en lugar de 'longtable'
      floating = TRUE,  # Permitir que la tabla flote
      hline.after = c(-1, 0, nrow(proyección)),  # Hacer líneas horizontales
      booktabs = TRUE,  # Usar líneas de estilo 'booktabs'
      title = "Proyección del Tipo de Cambio")  # Título de la tabla

