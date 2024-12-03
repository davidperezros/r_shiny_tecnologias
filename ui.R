# Libraries used
library(htmltools)
library(stringr)

library(shiny)
library(shinydashboard)
library(bslib)
library(ineapir)
library(dplyr)
library(sf) # read_sf
library(leaflet) # map
library(plotly)
# ineapir -------------------------------------------------------------------







# Selector variables ------------------

variables <- c(
  "viviendasconalguntipodeordenador",
  "viviendasquedisponendeaccesoainternet",
  "viviendascontelefonofijo",
  "viviendascontelefonomovil"
)
names(variables) <- c(
  "Viviendas con algún tipo de ordenador",
  "Viviendas que disponen de acceso a Internet",
  "Viviendas con teléfono fijo",
  "Viviendas con teléfono móvil"
)



# UI ----------------------------------------------------------------------
ui <- page_sidebar(
  includeCSS("www/style.css"),
  title = tags$a(href = "https://www.ine.es/", target = "_blank", tags$img(src = "ine_logo.svg", alt = "Logo", class = "logo")),
  tags$div(
    class = "logo-container2", tags$a(
      href = "https://github.com/davidperezros/r_shiny_tecnologias", # Cambia por tu enlace a GitHub
      target = "_blank", # Se abrirá en una nueva pestaña
      icon("github"), # Utiliza un ícono de Font Awesome
      class = "logo2"
    )
  ),
  sidebar = sidebar(
    selectizeInput(
      "x", "Variables",
      variables,
      selected = "viviendasconalguntipodeordenador"
    ),
    selectizeInput(
      "anyo", "Año",
      choices = as.character(seq(2006, 2024, 1)),
      selected = 2024
    ),
    tooltip(
      span(
        "Fuente de datos: INE",
        bsicons::bs_icon("info-circle")
      ),
      "Para más información que la mostrada debajo, visitar la web del INE.",
      placement = "bottom"
    ),
    tags$div(
      class = "fuentes",
      tags$span("Fuentes de datos:"), # Título de la sección
      tags$ul(
        # Lista con los hipervínculos
        tags$li("Datos viviendas, ", tags$a(href = "https://www.ine.es/jaxi/Tabla.htm?tpx=70470&L=0", "https://www.ine.es/jaxi/Tabla.htm?tpx=70470&L=0")),
        tags$li("Contornos comunidades, ", tags$a(href = "https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70", "https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70")),
        tags$li("Fuente principal: Encuesta sobre equipamiento y uso de tecnologías de información y comunicación en los hogares. ", tags$a(href = "https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176741&menu=resultados&idp=1254735576692", "https://www.ine.es/dyngs/INEbase/es"))
      )
    )
  ),
  layout_column_wrap(
    width = 3, # Ajustar el ancho total para las columnas
    height = 400,
    layout_column_wrap(
      width = 2, # Ancho de la columna del mapa
      card(
        full_screen = TRUE,
        card_header("Ambito geográfico"),
        leafletOutput("map")
      )
    ),
    layout_column_wrap(
      width = 1, # Ancho de la columna del gráfico de líneas
      heights_equal = "row",
      card(
        full_screen = TRUE,
        card_header(textOutput("variable")),
        plotOutput("lineChart")
      ),
      card(
        full_screen = TRUE,
        card_header("Otro Gráfico de Línea"),
        plotOutput("lineChart2") # Nuevo gráfico debajo del primer gráfico de líneas
      )
    )
  ),
  layout_column_wrap(
    width = 1, # Fila que ocupa todo el ancho
    height = 200,
    card(
      full_screen = TRUE,
      card_header(textOutput("variable2")),
      plotOutput("bubbleplot") # Gráfico que ocupa todo el ancho
    )
  )
)
