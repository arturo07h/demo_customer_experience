library(collapse)


options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")


# Rutas -------------------------------------------------------------------
ruta_data_proc <- "./data_raw.xlsx"

# Data a procesar ---------------------------------------------------------

data_proc <- openxlsx2::read_xlsx(file = ruta_data_proc,detect_dates = T) |> 
  tibble::as_tibble(.name_repair = "unique") |> janitor::clean_names()

# Modificación de data ----------------------------------------------------

data_proc |> dplyr::glimpse()
 












