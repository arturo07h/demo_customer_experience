library(collapse)
library(shiny)
library(bslib)
library(rlang)
library(plotly)
library(echarts4r)
library(leaflet)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Cargar base de datos ----------------------------------------------------
rutaData <- "./data_customer_experience_demo.xlsx"
rutaSF <- "./insumos/México_Estados/mexico_drv.shp"

# Parámetros --------------------------------------------------------------
col_lineVB_nac <- "#c1121f"
col_lineVB_sub <- "#31572c"
col_polmap <- "#3c0663"

# Leer bases --------------------------------------------------------------
wb <- openxlsx2::wb_load(rutaData)
names_sheet_dataRaw <- openxlsx2::wb_get_sheet_names(wb)

## Se cargan las bases por medio de las hojas del xlsx
list_sheets_dataRaw <- names_sheet_dataRaw  |>  set_names() |>  
  purrr::map(~ openxlsx2::read_xlsx(rutaData, sheet = .x) |>
               tibble::as_tibble() |> janitor::clean_names())

dataMetricas <- list_sheets_dataRaw$`Sheet 1`

## Vectores de aplicación
vect_input <- dataMetricas |> fcount(id_region,id_subregion,sort = T) |>
  rowbind(data.frame(id_region = "Nacional",id_subregion = "R1-S1", N = NA)) |> 
  fselect(-N)

## Cálculos por DRV y Subregion y por mes

calcular_metricas_drv <- function(data = NULL, 
                                  var_id = NULL, 
                                  nacional = F){
  
  if(!is.null(data)){
    
    if(nacional == T){
      data_nivel <- data |> dplyr::group_by(fecha)
    }else{
      data_nivel <- data |> dplyr::group_by({{var_id}},fecha)
    }
    
    data_sum <- data_nivel |> 
      fsummarise(across(promotores_pct:tasa_retencion,list(mu = fmean))) |> 
      fmutate(nps = promotores_pct - detractores_pct) |> 
      fungroup()
  }
  
  return(data_sum)
}

data_nacional <- calcular_metricas_drv(data = dataMetricas,nacional = T) |> fmutate(id_region = "Nacional")
data_regiones <- calcular_metricas_drv(data = dataMetricas,var_id = id_region)
data_subregiones <- calcular_metricas_drv(data = dataMetricas,var_id = id_subregion)

## Data para visualizaciones
data_gen <- rowbind(data_nacional,data_regiones)

## Valores de última fecha de levantamiento
list_data_gen <- split(data_gen,data_gen$id_region)

list_data_gen_ultimo_valor <- lapply(list_data_gen,function(x){
  
  x |> fsubset(fecha == fmax(x$fecha))
  
})

data_ultimo_valor <- rowbind(list_data_gen_ultimo_valor) |> 
  fselect(-c(promotores_pct:pasivos_pct)) 

## Cargar SF para información local
shp_mexico <- sf::read_sf("insumos/México_Estados/mexico_drv.shp") |> 
  fmutate(ESTADO = ESTADO |> stringr::str_replace_all(
    c("México" = "Estado de México",
    "Distrito Federal" = "Ciudad de México")
  ))

data_sucursal <- dataMetricas |> fmutate(estado = estado |> stringr::str_squish())
rel_reg_edo <- data_sucursal |> fcount(id_region,estado,sort = T)

shp_mexico <- join(x = shp_mexico,y = rel_reg_edo, on = c("ESTADO" = "estado"),how = "left")

# ui ----------------------------------------------------------------------

## Tema 
theme <- bs_theme(
  bg = "#003049", fg = "white",
  "input-border-color" = "white"
)

## UI
ui <- fluidPage(
  
  theme = theme,
  
  tags$h2("Indicadores de experiencia del cliente"),
  
  tabsetPanel(
   
    tabPanel(
      "Info. Nacional",
      
      ## Estilo de values box
      tags$style(HTML("
    .card {
      background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
      border: none; /* Opcional: quitar bordes */
      box-shadow: none; /* Opcional: quitar sombra */
      }")),
      
      ## Título y filtro regional
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "vect_drv", 
            choices = funique(vect_input$id_region), 
            label = "Selecciona tu región:", 
            selected = "Nacional"
          )
        )
      ),
      
      ## Columns de values box
      fluidRow(
        column(
          4 ,
          value_box(
            title = tags$p("NPS", style = "font-size: 200%;font-weight: bold;"),
            value = uiOutput("value_nps"),
            showcase = plotlyOutput("graf_nps"),
            showcase_layout = "bottom"
          )
        ),
        column(
          4,
          value_box(
            title = tags$p("CSAT", style = "font-size: 200%;font-weight: bold;"),
            value = uiOutput("value_csat"),
            showcase = plotlyOutput("graf_csat"),
            showcase_layout = "bottom"
          )
        ),
        column(
          4,
          value_box(
            title = tags$p("CES", style = "font-size: 200%;font-weight: bold;"),
            value = uiOutput("value_ces"),
            showcase = plotlyOutput("graf_ces"),
            showcase_layout = "bottom"
          )
        )
      ),
      fluidRow(
        column(
          4,
          div(
            style = "
        background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
        border: none; /* Opcional: quitar bordes */
        box-shadow: none; /* Opcional: quitar sombra */
        border-radius: 15px; /* Esquinas redondeadas */
        padding: 10px; /* Opcional: espacio interno */
        height: 430px;
        width:100%; 
        ",
            echarts4rOutput("graf_nps_comp", height = "400px", width = "100%")
          )
        ),
        column(
          4,
          div(
            # class = "small-graph", 
            style = "
        background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
        border: none; /* Opcional: quitar bordes */
        box-shadow: none; /* Opcional: quitar sombra */
        border-radius: 15px; /* Esquinas redondeadas */
        padding: 10px; /* Opcional: espacio interno */
        height: 430px;
        width:100%; 
        ",
            echarts4rOutput("graf_evo_fcr", height = "240px", width = "100%"),
            echarts4rOutput("graf_fcr_gauge", height = "250px", width = "100%")
          )
        ),
        column(
          4,
          div(
            style = "
            display: flex; 
            flex-direction: column; 
            align-items: left; 
            height: 450px;
            width:100%; 
            ",
            value_box(
              title = tags$p("Tiempo promedio de respuesta", style = "font-size: 120%;font-weight: bold;"),
              value = uiOutput("value_tiempo_resp"),
              showcase = plotlyOutput("graf_tiempo_res"),
              showcase_layout = "bottom"
            ),
            value_box(
              title = tags$p("Tasa de retención", style = "font-size: 120%;font-weight: bold;"),
              value = uiOutput("value_tasa_ret"),
              showcase = plotlyOutput("graf_tasa_ret"),
              showcase_layout = "bottom"
            )
          )
        )
      )
      
    ),
    
    tabPanel(
      "Local",
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "vect_sdrv", 
            choices = funique(vect_input$id_subregion), 
            label = "Selecciona tu subregión:", 
            selected = "R1-S1"
          )
        )
      ),
      fluidRow(
        column(
          4,
          value_box(
            title = tags$p("NPS", style = "font-size: 120%;font-weight: bold;"),
            value = uiOutput("nps_subreg"),
            showcase = plotlyOutput("graf_nps_sub"),
            showcase_layout = "bottom"
          )
        ),
        column(
          4,
          value_box(
            title = tags$p("CSAT", style = "font-size: 120%;font-weight: bold;"),
            value = uiOutput("csat_subreg"),
            showcase = plotlyOutput("graf_csat_sub"),
            showcase_layout = "bottom"
          )
        ),
        column(
          4,
          value_box(
            title = tags$p("CES", style = "font-size: 120%;font-weight: bold;"),
            value = uiOutput("ces_subreg"),
            showcase = plotlyOutput("graf_ces_sub"),
            showcase_layout = "bottom"
          )
        )
      ),
      fluidRow(
        column(
          12,
          leafletOutput("map_reg")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session){
  
  ## Filtros update
  observe({
    updateSelectInput(
      session,
      "vect_drv",
      choices = sort(funique(vect_input$id_region))
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "vect_sdrv",
      choices = vect_input |> 
        fsubset(id_region == input$vect_drv) |> 
        select(id_subregion) %>%
        .[[1]] %>% 
        sort(.)
    )
  })
  
  ## Sección de data reactiva
  
  ### Valor actual de componentes
  data_componentes <- reactive({
    data_ultimo_valor |> fsubset(id_region == input$vect_drv)
  })
  
  ### Data de evolución de componentes
  data_evo_componetes <- reactive({
    data_gen |> fsubset(id_region == input$vect_drv)
  })
  
  ## SHP reactivo para info regional
  shp_reactivo <- reactive({
    
    if(input$vect_drv == "Nacional"){
      shp_mexico
    }else{
      shp_mexico |> fsubset(id_region == input$vect_drv)
    }
  })
  
  ## Creación de objetos
  
  ### Valor actual de componentes
  output$value_nps <- renderText({
    round(funique(data_componentes()$nps),2)
  })
  
  output$value_csat <- renderText({
    round(funique(data_componentes()$csat),2)
  })
  
  output$value_ces <- renderText({
    round(funique(data_componentes()$ces),2)
  })
  
  output$value_tiempo_resp <- renderText({
    round(funique(data_componentes()$tiempo_respuesta),2)
  })
  
  output$value_tasa_ret <- renderText({
    round(funique(data_componentes()$tasa_retencion),2)
  })
  
  ## Componentes por subregion 
  data_subreg <- reactive({
    data_subregiones |> fsubset(id_subregion == input$vect_sdrv)
  })
  
  ## Value box con echaerts4r para Subregiones
  
  ### NPS 
  output$nps_subreg <- renderText({
    data <- data_subreg() |> fsubset(fecha == fmax(fecha))
    round(funique(data$nps),2)
  })
  
  ### CSAT
  output$csat_subreg <- renderText({
    data <- data_subreg() |> fsubset(fecha == fmax(fecha))
    round(funique(data$csat),2)
  })
  
  ### CES
  output$ces_subreg <- renderText({
    data <- data_subreg() |> fsubset(fecha == fmax(fecha))
    round(funique(data$ces),2)
  })
  
  ## Gráficos de evolución de componentes
  
  ## NACIONAL - REGIONAL
  ### Formato de gráficos de tipo line para value box
  layuout_Value_box_graf <- function(x){
    x |> 
      layout(
        xaxis = list(title = F,visible = T, showgrid = FALSE, color = "white"),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(displayModeBar = F)
  }
  
  ### Formato de gráficos de tipo bar para FCR
  layout_graf_bar <- function(x){
    
    data <- data_evo_componetes() |> fselect(fecha)
    
    fecha_max <- max(data$fecha)
    fecha_min <- fecha_max - months(13)
    
    x |> 
      layout(
        yaxis = list(
          color = "white",
          showticklabels = FALSE
        ),
        xaxis = list(
          color = "white",
          range = c(fecha_min, fecha_max),
          rangeselector = list(
            buttons = list(
              list(count = 6, label = "6 meses", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 año", step = "year", stepmode = "backward"),
              list(step = "all", label = "Todo")
            )
          ),
          rangeslider = list(visible = F, thickness = .1)
        ),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
    config(locale = "es")
  }
  
  ## NPS
  output$graf_nps <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,nps)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~nps,
        color = I(col_lineVB_nac),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CSAT :", round(data$nps,2))) |> 
      layuout_Value_box_graf()
      
  })
  
  ## CSAT
  output$graf_csat <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,csat)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~csat,
        color = I(col_lineVB_nac),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CSAT :", round(data$csat,2))) |> 
      layuout_Value_box_graf()
  })
  
  ## CES
  output$graf_ces <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,ces)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~ces,
        color = I(col_lineVB_nac),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CES :", round(data$ces,2))) |> 
      layuout_Value_box_graf()
  })
  
  ## Evolución Tiempo promedio de respuesta
  output$graf_tiempo_res <- renderEcharts4r({
    
    data <- data_evo_componetes() |> fselect(fecha,tiempo_respuesta)
    fecha_max <- max(data$fecha)
    
    plot_ly(x = data$fecha, 
            y = data$tiempo_respuesta,
            type = "bar",
            marker = list(color = "#73d2de"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                              "<br>TPR :", round(data$tiempo_respuesta,2))) |> 
      layout_graf_bar()
    
  })
  
  ## Evolución de tasa de retención
  output$graf_tasa_ret <- renderEcharts4r({
    
    data <- data_evo_componetes() |> fselect(fecha,tasa_retencion)
    fecha_max <- max(data$fecha)
    
    plot_ly(x = data$fecha, 
            y = data$tasa_retencion,
            type = "bar",
            marker = list(color = "#02c39a"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                              "<br>TPR :", round(data$tasa_retencion,2))) |> 
      layout_graf_bar()
  })
  
  ### Componentes NPS
  output$graf_nps_comp <- renderEcharts4r({
    
    data <- data_evo_componetes() |> 
      fselect(fecha,promotores_pct,detractores_pct,pasivos_pct) |> 
      frename(Promotores = promotores_pct,
              Detractores = detractores_pct,
              Pasivos = pasivos_pct)
    
    data |> 
      e_chart(x = fecha) |> 
      
      e_bar(serie = Detractores, stack = "grp", color = "#f95738") |> 
      e_bar(serie = Pasivos, stack = "grp", color = "#ffa62b") |> 
      e_bar(serie = Promotores, stack = "grp", color = "#2ec4b6") |> 
      
      e_title("Evolución de componentes NPS",
              textStyle = list(color = "white",fontSize = 20)) |> 
      e_tooltip(trigger = "axis") |> 
      e_legend(orient = "horizontal", 
               left = "center", 
               top = "bottom",
               textStyle = list(color = "white",fontSize = 16)) |> 
      e_x_axis(axisLabel = list(color = "white",fontSize = 16)) |> 
      e_y_axis(axisLabel = list(color = "white",fontSize = 16), max = 100 ) |> 
      
      e_datazoom(
        type = "slider",
        startValue = "2024-01-01",
        bottom = "8%"  
      ) |> 
      e_grid(
        bottom = "20%"  
      )
    
  })
  
  ## Gráfico gauge FCR
  output$graf_fcr_gauge <- renderEcharts4r({
    
    value_fil <- data_evo_componetes() |> fsubset(fecha == fmax(fecha))
    value <- funique(value_fil$fcr)
    
    e_charts() |> 
      e_gauge(
        round(value, 1),  # Valor del gauge
        name = round(value, 1),
        radius = "100%",
        startAngle = 180,
        endAngle = 0,
        itemStyle = list(color = "#a7c957"),
        axisLine = list(
          lineStyle = list(
            color = list(c(0.33, "#f95738"), c(0.67, "#c77dff"), c(1, "#2ec4b6")),
            width = 10
          )
        ),
        axisTick = list(lineStyle = list(width = 2, color = "white", fontSize = 16)),
        axisLabel = list(
          show = TRUE,
          color = "white",
          fontWeight = "bold",
          borderRadius = 5,
          fontSize = 15
        ),
        pointer = list(show = TRUE, icon = "triangle", length = "100%"),
        itemStyle = list(color = "white"),
        detail = list(
          show = TRUE,
          color = "white",
          fontSize = 0,
          fontWeight = "bold"
        ),
        title = list(
          show = TRUE,
          color = "white", 
          textStyle = list(
            color = "white",  
            fontSize = 22,    
            fontWeight = "bolder"
          )
        )
      ) |> 
      e_title("Enero 2025",
              textStyle = list(color = "white",fontSize = 16))
  })
  
  ### Gráfico de evolución de FCR
  output$graf_evo_fcr <- renderEcharts4r({
    
    data_evo_componetes() |> 
      fselect(fecha,fcr) |> 
      e_charts(x = fecha) |> 
      e_area(serie = fcr, symbol = "none", color = "red") |> 
      e_title("Resolución en el primer contacto (FCR)",
              textStyle = list(color = "white",fontSize = 20)) |> 
      e_legend(show = F) |> 
      e_x_axis(axisLabel = list(color = "white",fontSize = 14)) |> 
      e_y_axis(axisLabel = list(color = "white",fontSize = 14), max = 100)
    
  })
  
  ## SUBREGION
  ## NPS
  output$graf_nps_sub <- renderPlotly({
    
    data <- data_subreg() |> fselect(fecha,nps)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~nps,
        color = I(col_lineVB_sub),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CSAT :", round(data$nps,2))) |> 
      layuout_Value_box_graf()
    
  })
  
  ## CSAT
  output$graf_csat_sub <- renderPlotly({
    
    data <- data_subreg() |> fselect(fecha,csat)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~csat,
        color = I(col_lineVB_sub),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CSAT :", round(data$csat,2))) |> 
      layuout_Value_box_graf()
  })
  
  ## CES
  output$graf_ces_sub <- renderPlotly({
    
    data <- data_subreg() |> fselect(fecha,ces)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~ces,
        color = I(col_lineVB_sub),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CES :", round(data$ces,2))) |> 
      layuout_Value_box_graf()
  })
  
  ## Información regional
  output$map_reg <- renderLeaflet({
    
    leaflet() |> 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>")
      ) |>
      setView(lng = -102.5528, lat = 23.6345, zoom = 5) |> 
      addPolygons(
        data = shp_reactivo(),
        fillOpacity = 0.4,
        weight = 2,
        color = col_lineVB_sub,
      )
  })
  
  observeEvent(input$vect_drv,{

    if (input$vect_drv == "Nacional") {return()}

    ## Polígonos
    zona_lat <- sf::st_coordinates(shp_reactivo()) |> tibble::as_tibble()

    flng1 <- fmin(zona_lat$X)
    flng2 <- fmax(zona_lat$X)

    flat1 <- fmin(zona_lat$Y)
    flat2 <- fmax(zona_lat$Y)

    # browser()
    # data_suc_resumen <- data_suc() %>%
    #   fgroup_by(sucursal, fecha, longitud, latitud) %>%
    #   fsummarise(
    #     suma_total = sum(N, na.rm = TRUE),
    #     Alta = scales::percent(sum(por[gravedad == "Alta"], na.rm = TRUE), accuracy = 1),
    #     Media = scales::percent(sum(por[gravedad == "Media"], na.rm = TRUE), accuracy = 1),
    #     Baja = scales::percent(sum(por[gravedad == "Baja"], na.rm = TRUE), accuracy = 1)) |>
    #   fungroup()

    leafletProxy("map_reg") |>
      clearShapes() |>
      addPolygons(data = shp_reactivo(),
                  color = col_polmap,
                  label = ~ ESTADO,
                  fillOpacity = 0.5,
                  weight = 2,
                  popup = ~ paste("Estado:", ESTADO)) |>
      # addMarkers(
      #   data = data_suc_resumen,
      #   lng = ~longitud,
      #   lat = ~latitud,
      #   popup = ~paste0(
      #     "<b>Sucursal: </b>", sucursal, "<br>",
      #     "<b>Mes de consulta: </b>", fecha, "<br>",
      #     "<b>Total de quejas mensuales: </b>", suma_total, "<br>",
      #     "<b>Gravedad</b><br>",
      #     fifelse(Alta != "0%", paste0("<b>Alta: </b>", Alta, "<br>"), paste0("<b>Alta: </b>","0%", "<br>")),
      #     fifelse(Media != "0%", paste0("<b>Media: </b>", Media, "<br>"), paste0("<b>Media: </b>","0%", "<br>")),
      #     fifelse(Baja != "0%", paste0("<b>Baja: </b>", Baja), paste0("<b>Baja: </b>","0%", "<br>"))
      #   )) |>
      flyToBounds(
        lng1 = flng1,
        lng2 = flng2,
        lat1 = flat1,
        lat2 = flat2
      )
  })
  
}

shinyApp(ui,server)