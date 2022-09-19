library(shiny)
library(mapdeck)
library(tidyverse)
library(DT)
library(bslib)

# Requisitos:
# 1. un cuadro de inputs
# 2. un slider de meses
# 3. un mapa que cambia

# listas de selecciones
years <- c(2003:2021)
meses <- c(1:12)
load("output/incendios_bol.RData")

# ui
ui <- navbarPage("Dashboard de incendios en Bolivia 2003-2021",
                 theme = bs_theme(bootswatch = "cyborg"),
                 tabPanel("Mapa",
                          fluidPage(
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                            ),
                            fluidRow(
                              column(12,
                                     sliderInput(inputId = "year", label = "Año",min = 2003, max = 2021, value = 2003, animate = T, width = "100%", sep = "")
                                     )
                            ),
                            fluidRow(
                              column(8,
                                     mapdeckOutput(outputId = "mapa_incendios", height = "75vh")
                                     ),
                              column(4,
                                     h6("Temperaturas registradas en incendios en °C"),
                                     plotOutput(outputId = "plot_calor", height = "250px"),
                                     h6("Poder radiativo del fuego, en megawatts"),
                                     plotOutput(outputId = "plot_energia", height = "250px")
                                     )
                            )
                          )),
                 tabPanel("Datos",
                          DTOutput("tabla"),
                          downloadButton("descargar", "Descargar CSV")
                          )
)
  

# server
server <- function(input, output, session) {
  
  thematic::thematic_shiny()
    
    # hacemos los datos reactivos
    #datos <- reactive({
     #   incendios_bol %>% 
      #      filter(year == year_incendio$input)
    #})
    
    # cargamos datos necesarios para el mapa
    #bolivia_base <- read_file("data/bolivia-departamentos.geojson")
    paleta_incendios <- c("#5A1846", "#900C3F", "#C70039", "#E3611C", "#F1920E", "#FFC300")
    key <- "pk.eyJ1IjoibGFidGVjbm9zb2NpYWwiLCJhIjoiY2ttaHJ2N2FwMGE4NjJ5cXVneHN2cWRzYiJ9.MT3xcDnYAz2m1LvjBHRQwQ"
    
    
    # el mapa
    output$mapa_incendios <- renderMapdeck({
        mapdeck(token = key,
                max_zoom = 8,
                min_zoom = 5,
                style = "mapbox://styles/labtecnosocial/cl87dy62v000r15lalgia22lg",
                zoom = 5,
                # pitch = 20,
                location = c(-66.24375678696428, -15.940670400011369))
    })
    
    observeEvent(input$year, {
      datofilt <- incendios_bol %>% filter(year == input$year)
      
      mapdeck_update(map_id = "mapa_incendios") %>%
        add_hexagon(data = datofilt,
                    lon = "longitude",
                    lat = "latitude",
                    radius = 5000,
                    colour = "decil",
                    colour_function = "mean",
                    colour_range = paleta_incendios,
                    # elevation = "celsius",
                    # elevation_function = "mean",
                    # elevation_scale = 40,
                    legend = T,
                    update_view = FALSE)
    })

    output$plot_calor <- renderPlot({
      incendios_bol %>% filter(year == input$year) %>%
        ggplot(aes(x = celsius)) +
        geom_histogram(bins = 15, fill = "#2c4f68", alpha = 0.8) +
        theme_minimal(base_size = 20) + 
        theme(panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "#09101d"),
              axis.text.x = element_text(color="#8CAEBA", 
                                         size=12),
              axis.text.y = element_blank()) +
        labs(x = NULL, y = NULL)
    })
    
    output$plot_energia <- renderPlot({
      incendios_bol %>% filter(year == input$year) %>%
        ggplot(aes(x = frp)) +
        scale_x_log10() +
        geom_histogram(bins = 15, fill = "#2c4f68", alpha = 0.8) +
        theme_minimal(base_size = 20) + 
        theme(panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "#09101d"),
              axis.text.x = element_text(color="#8CAEBA", 
                                         size=12 ),
              axis.text.y = element_blank()) +
        labs(x = NULL, y = NULL)
    })
    
    output$tabla <- renderDT({
      incendios_bol %>% filter(year == input$year) %>%
        datatable(style = "bootstrap", extensions = 'Responsive', options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          searching = F,
          dom = "lti"
        ))
    })
    output$descargar <- downloadHandler(
      filename = function() {
        paste0(input$year, ".csv")
      },
      content = function(file) {
        datos_filt <- incendios_bol %>% filter(year == input$year) 
        write.csv(datos_filt, file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
