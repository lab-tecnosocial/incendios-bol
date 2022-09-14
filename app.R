library(shiny)
library(mapdeck)
library(tidyverse)

# Requisitos:
# 1. un cuadro de inputs
# 2. un slider de meses
# 3. un mapa que cambia

# listas de selecciones
years <- c(2003:2021)
meses <- c(1:12)
load("output/incendios_bol.RData")

# ui
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("Dashboard de incendios en Bolivia 2003-2021"),
    
    sidebarLayout(
      
      sidebarPanel(sliderInput(inputId = "year", label = "año",min = 2003, max = 2021, value = 2003, sep = ""),
                   plotOutput(outputId = "plot_calor"),
                   plotOutput(outputId = "plot_energia")
                   ),
      
      mainPanel(mapdeckOutput(outputId = "mapa_incendios", height = "600px"))
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
                style = mapdeck_style("dark"),
                zoom = 5,
                pitch = 20,
                location = c(-66.24375678696428, -15.940670400011369))
    })
    
    observeEvent(input$year, {
      datofilt <- incendios_bol %>% filter(year == input$year)
      
      mapdeck_update(map_id = "mapa_incendios") %>%
        add_hexagon(data = datofilt,
                    lon = "longitude",
                    lat = "latitude",
                    radius = 5000,
                    colour = "quartil",
                    colour_function = "mean",
                    colour_range = paleta_incendios,
                    elevation = "celsius",
                    elevation_function = "mean",
                    elevation_scale = 40,
                    update_view = FALSE)
    })

    output$plot_calor <- renderPlot({
      incendios_bol %>% filter(year == input$year) %>%
        ggplot(aes(x = celsius)) +
        geom_histogram(bins = 15) +
        theme_minimal()
    })
    
    output$plot_energia <- renderPlot({
      incendios_bol %>% filter(year == input$year) %>%
        ggplot(aes(x = frp)) +
        scale_x_log10() +
        geom_histogram(bins = 15) +
        theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
