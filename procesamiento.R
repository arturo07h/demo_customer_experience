library(collapse)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Rutas de bases ----------------------------------------------------------
ruta_dataRaw <- "./data_raw.xlsx"

# Cargar dataRaw ----------------------------------------------------------
data_raw <- openxlsx2::read_xlsx(file = ruta_dataRaw) |> 
  tibble::tibble() |> janitor::clean_names()

# Procesar data -----------------------------------------------------------
ultimo_registro <- fmax(data_raw$fechas_registro)




set.seed(123)
## Simulaciones

### Fechas de registro
seq_reg <- seq(
  as.POSIXct("2024-06-01 23:00:00"), 
  as.POSIXct(Sys.Date(), format = "%m/%d/%y", tz = "EST"),
  by="hour"
)

no_registros <- length(seq_reg)

## ID sucursal
sucursales_reg <- sample(
  x = paste0("sucursal_",seq(1:10)),
  size = no_registros,
  replace = TRUE
)

## NPS
nps_reg <- sample(
  x = 0:10,
  size = no_registros,
  replace = TRUE,
  prob = c(
    .1, # 0
    .1, # 1
    .1, # 2
    .1, # 3
    .1, # 4
    .15, # 5
    .15, # 6
    .15, # 7
    .8, # 8
    .8, # 9
    .8 # 10
  )
)

## CSAT
csat_reg <- sample(
  x = 0:5,
  size = no_registros,
  replace = TRUE,
  prob = c(
    .1, # 0
    .2, # 1
    .3, # 2
    .3, # 3
    .8, # 4
    .8 # 5
  )
)

## CES
ces_reg <- sample(
  x = 1:7,
  size = no_registros,
  replace = TRUE,
  prob = c(
    .8, # 1
    .8, # 2
    .5, # 3
    .5, # 4
    .7, # 5
    .4, # 6
    .2 # 7
  )
)

## Tiempo promdeio de respuesta (minutos)
tiempo_res_reg <- sample(
  x = 10:30,
  size = no_registros,
  replace = TRUE,
)

## Tasa de retención
tsr_reg <- sample(
  x = 87:97,
  size = no_registros,
  replace = TRUE,
  prob = c(
    .1, # 0
    .1, # 1
    .1, # 2
    .1, # 3
    .1, # 4
    .15, # 5
    .15, # 6
    .15, # 7
    .8, # 8
    .8, # 9
    .8 # 10
  )
)

## Creación de data frame
data_real <- data.frame(
  
  id_sucursal = sucursales_reg,
  fechas_registro = seq_reg,
  nps = nps_reg,
  csat = csat_reg,
  ces = ces_reg,
  tiempo_respuesta = tiempo_res_reg,
  tasa_retencion = tsr_reg
)

openxlsx2::write_xlsx(x = data_real,file = ruta_dataRaw)