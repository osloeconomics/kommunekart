library(mapview)
library(shiny)

server <- function(input, output) {
  output$test <- renderLeaflet({
    req(input$obs)
    rws <- input$obs
    mapview(breweries91[1:rws,])@map
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 1, max = nrow(breweries91@data), value = nrow(breweries91@data))
    ),
    mainPanel(
      leafletOutput('test')
    )
  )
)

shinyApp(ui = ui, server = server)