library(readr)
library(tidyverse)
library(janitor)

curvas_cero_cupon <- read_delim(
  "datos/curvas-cero-cupon-csv.csv",
  delim = ";",          # Especifica el delimitador como punto y coma
  locale = locale(decimal_mark = ",")  # Define la coma como separador decimal
)

curvas_cero_cupon <- curvas_cero_cupon %>% clean_names()

