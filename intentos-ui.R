library(shiny)
library(semantic.dashboard)
data("mtcars")

ui <- dashboardPage(
  dashboardHeader(title = "Incendios en Bolivia 2003-2021", right = TRUE, titleWidth = "wide", color = "black", inverted = TRUE),
  dashboardSidebar(size = "wide", color = "black", inverted = TRUE,
                   menuItem(tabName = "main", "Main"),
                   menuItem(tabName = "extra", "Extra"),
                   box(
                     title = "Histograma de calor", 
                     color = "yellow", 
                     ribbon = FALSE, 
                     title_side = "top", 
                     column(
                       width = 8, 
                       plotOutput("plot_calor") 
                     ) 
                   )),
  dashboardBody()
  )

server <- shinyServer(function(input, output, session) {
})

shinyApp(ui, server)
