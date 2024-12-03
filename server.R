# [DATOS] Códigos y varibale.id de las ccaa ---------------------------

# get_metadata_table_varval(2853)
# Filtramos todas las ccaa
filter2 <- list(
  "18" = "451" # Todas ccaa
)

esp2 <- get_data_table(
  idTable = 2853, filter = filter2, nlast = 1, unnest = TRUE,
  metacodes = TRUE, metanames = TRUE, tip = "AM", validate = FALSE
)

# Seleccionamos columnas de interés
esp2 <- subset(esp2, select = c("Comunidades.y.Ciudades.Autónomas.Id", "Comunidades.y.Ciudades.Autónomas.Codigo"))


# [DATOS] Contornos de las ccaa ------------
ccaa2 <- read_sf("https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70")



# [DATOS] Mostrar dashboard -----------------------
filter_tech <- list(
  "CCAA" = "",
  "tipodeequipamiento" = c(
    "viviendascontelefonofijo",
    "viviendasquedisponendeaccesoainternet",
    "viviendasconalguntipodeordenador",
    "viviendascontelefonomovil"
  )
)

aa <- get_metadata_table_varval(70470)

data_tech <- get_data_table(idTable = "70470", filter = filter_tech, tip = "AM", unnest = TRUE, metanames = TRUE, metacodes = TRUE)


# [MERGE] todos datos anteriores ----------

ccaa2 <- merge(
  merge(ccaa2, esp2,
    by.x = "id_region",
    by.y = "Comunidades.y.Ciudades.Autónomas.Id"
  ),
  data_tech,
  by.x = "Comunidades.y.Ciudades.Autónomas.Codigo",
  by.y = "CCAA.Codigo"
)







# Para mostrar el Total Nacional
filter_tech2 <- list(
  "NAC" = "",
  "tipodeequipamiento" = c(
    "viviendascontelefonofijo",
    "viviendasquedisponendeaccesoainternet",
    "viviendasconalguntipodeordenador",
    "viviendascontelefonomovil"
  )
)

data_tech2 <- get_data_table(idTable = "70470", filter = filter_tech2, tip = "AM", unnest = TRUE, metanames = TRUE, metacodes = TRUE)
data_tech2$CCAA.Codigo <- 16473
data_tech2$CCAA <- "Total Nacional"

ccaa22 <- read_sf("https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70")

ccaa22 <- merge(ccaa22, data_tech2,
  by.x = "id_region",
  by.y = "CCAA.Codigo"
)














# Server ------------------------------------------------------------------

server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) +
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  })













  # Datos reactivos para pintar el mapa
  # Filtramos por Variable y por Año

  data_tech_2_reactive <- reactive({
    df <- ccaa2 %>%
      filter(`tipodeequipamiento.Codigo` == as.character(input$x)) %>%
      filter(`NombrePeriodo` == input$anyo)
  })






  # GRÁFICO MAPA LEAFLET
  output$map <- renderLeaflet({
    data <- data_tech_2_reactive()
    pal <- colorNumeric(
      palette = colorRampPalette(c("#f5a9be", "#881333"))(100),
      domain = data$Valor # Dominio explícito de 0 a 100
    )

    m2 <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-4, 40, zoom = 5.1) %>%
      addPolygons(
        fillOpacity = 0.8,
        fillColor = ~ pal(Valor), # Aplicar la paleta de colores
        weight = 1,
        label = ~CCAA,
        color = "white",
        highlightOptions = highlightOptions(
          fillOpacity = 1, bringToFront = TRUE,
          weight = 2, color = "white"
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        popup = ~ paste(
          "<strong> CCAA: </strong>", CCAA, "<br>",
          "<strong> Porcentaje: </strong>", Valor, " %<br>"
        ),
        layerId = ~id_region # Asignamos el id_region a cada polígono
      ) %>%
      addLegend(
        pal = pal,
        values = ~Valor,
        opacity = 0.7,
        title = "Valor",
        position = "bottomright"
      )
  })





  # Datos reactivos para captar la selección del mapa

  final_reactive <- reactive({
    # Obtener el clic y el valor de input$x
    click <- input$map_shape_click
    selected_x <- as.character(input$x)

    if (is.null(click)) {
      # Si no hay clic, solo filtra por variable
      df <- ccaa22 %>%
        filter(`tipodeequipamiento.Codigo` == selected_x)
    } else {
      # Si hay clic, filtra tanto por input$x como por el id de la ccaa seleccionada
      df <- ccaa2 %>%
        filter(`tipodeequipamiento.Codigo` == selected_x) %>%
        filter(`id_region` == click$id)
    }

    return(df) # Devolver el dataframe filtrado
  })


  # GRÁFICO GGPLOT
  output$lineChart <- renderPlot({
    df <- final_reactive() # Obtener los datos filtrados
    df <- df[order(df$NombrePeriodo), ] # Asegurarse de que los datos están ordenados por NombrePeriodo

    if (nrow(df) == 0) {
      return(NULL)
    } # Si no hay datos, no hacer nada

    # Extraer los dos últimos dígitos del año

    # Crear el gráfico de línea
    ggplot(df, aes(x = NombrePeriodo, y = Valor)) + # Usar NombrePeriodo y Valor
      geom_line(color = "#881333", size = 1, group = 1) + # Línea roja con grosor 1
      geom_point(color = "#881333", size = 2) + # Puntos de color azul y tamaño 4
      labs(
        title = paste0(max(df$CCAA)),
        x = "Periodo",
        y = "Valor"
      ) +
      theme_minimal() + # Usar un tema minimalista
      scale_x_discrete(
        breaks = df$NombrePeriodo[seq(1, length(df$NombrePeriodo), by = 2)],
        labels = function(x) substr(x, nchar(x) - 1, nchar(x)) # Mostrar solo los 2 últimos dígitos
      ) # Mostrar cada 2 etiquetas
  })



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





  output$variable <- renderText({
    aux <- input$x
    variable_name <- names(variables)[variables == aux]

    paste0(variable_name)
  })
  
  
  
  glosario_ccaa <- as.data.frame(read_sf("https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70") )%>%select(id_region,nom_region)
  
  output$variable2 <- renderText({
    aux <- input$anyo
    click <- input$map_shape_click

    if (is.null(click)) {cod_ccaa=data.frame(id <-"16473")}else{
    cod_ccaa <- click}

    paste0("Equipamiento en las viviendas en el ", aux, " (%). ",glosario_ccaa$nom_region[glosario_ccaa$id_region==cod_ccaa$id])
  })
  
  

  
  
  

  
  
  
  # Datos reactivos para captar la selección del mapa
  
  final_reactive_bubble <- reactive({
    # Obtener el clic y el valor de input$x
    click <- input$map_shape_click
    selected_anyo <- as.character(input$anyo)
    
    if (is.null(click)) {
      # Si no hay clic, solo filtra por variable
      df <- ccaa22 %>%
        filter(`NombrePeriodo` == selected_anyo)
    } else {
      # Si hay clic, filtra tanto por input$x como por el id de la ccaa seleccionada
      df <- ccaa2 %>%
        filter(`NombrePeriodo` == selected_anyo) %>%
        filter(`id_region` == click$id)
    }
    
    return(df) # Devolver el dataframe filtrado
  })
  
  
  
  
  # GRÁFICO GGPLOT
  output$bubbleplot <- renderPlot({
    df <- final_reactive_bubble() # Obtener los datos filtrados

    if (nrow(df) == 0) {
      return(NULL)
    } # Si no hay datos, no hacer nada
    
    # Extraer los dos últimos dígitos del año
    
    # Crear el gráfico de línea
    # Crear el gráfico de burbujas
    # Definir el ancho de la envoltura del texto

    # Crear el gráfico de burbujas
    ggplot(df, aes(x = tipodeequipamiento, y = 1, size = Valor, label = paste0(Valor, "%"), fill = Valor)) +
      geom_point(shape = 21, color = "black",stroke = 0) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
      geom_text(vjust = 0.5, hjust = 0.5, size = 4, color = "white") +
      scale_size(range = c(10, 30)) +  # Ajusta el rango del tamaño de las burbujas
      scale_fill_gradient(low = "pink", high = "#881333") +  # Gama de colores de rojo
      theme_minimal() +
      labs(        x = NULL,
        y = NULL
      ) +
      theme(
        axis.text.x = element_text(size = 10, color = "#457e76",face="bold"),  # Ajusta el tamaño y color del texto del eje X
        axis.text.y = element_blank(),  # Elimina los textos de los ejes
        axis.ticks = element_blank(),  # Elimina las marcas de los ejes
        panel.grid = element_blank(),  # Elimina las líneas de la cuadrícula
        panel.background = element_blank(),  # Elimina el fondo del panel
        legend.position = "none"
      )
  })
  
  
  
  
  
  
  
  
  
  
  
}
