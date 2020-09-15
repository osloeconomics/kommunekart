library(dplyr)
library(mapview)
library(sf)
library(shiny)
library(tools)
library(readxl)
library(DT)
library(readr)
library(haven)
library(leaflet)
library(waiter)

# Define UI for application that draws a histogram
ui <- fluidPage(

    use_waiter(), # include dependencie
    
    # Application title
    titlePanel("Kommunekart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("kommune_fil", 
                      label = "Last opp kommunedata",
                      buttonLabel = "Bla igjennom..."),
            selectInput("keyvar", label = "Velg variabelen som inneholder kommunenummer",
                        choices = NULL),
            selectInput("kommuneår", label = "Velg hvilket år kommuneinndelingen er fra",
                        choices = c("2020", "2019", "2018", "2017", "2016"),
                        selected = NULL),
            selectInput("fillvar", label = "Velg hvilken variabel du vil vise på kartet",
                        choices = NULL),
            actionButton("plot", label = "Generer kart")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    kommunedata <- eventReactive(input$kommune_fil, ignoreNULL = TRUE, ignoreInit = TRUE, {
        
        infile <- input$kommune_fil
        if (tolower(file_ext(infile$datapath)) == "xlsx") {
            data <- read_xlsx(infile$datapath)
        } else if (tolower(file_ext(infile$datapath)) == "csv") {
            data <- read_csv(infile$datapath)    
        } else if (tolower(file_ext(infile$datapath)) == "dta") {
            data <- read_dta(infile$datapath) %>%
                mutate(across(where(is.labelled), as_factor))
        }
        
        return(data)
    })
    
    observe({
        vars <- names(kommunedata())
        updateSelectInput(session, "keyvar", "Velg variabelen som inneholder kommunenummer", choices = vars)
        updateSelectInput(session, "fillvar", "Velg hvilken variabel du vil vise på kartet", choices = vars)
    })
    
    kart <- eventReactive(input$kommuneår, {
        filnavn <- paste0("kommuner_", input$kommuneår, ".rds")
        readRDS(filnavn)
    })
    
    kart_kommunedata <- eventReactive(input$plot, {
        req(input$keyvar)
        left_join(kart(), kommunedata(), by = c("kommunenummer" = input$keyvar)) %>%
            st_transform(4326)
    })
    
    fillvar <- reactive({
        input$fillvar
    })
    
    fillvar_min <- reactive({
        min(kart_kommunedata()[[fillvar()]], na.rm = TRUE)
    })
    
    fillvar_max <- reactive({
        max(kart_kommunedata()[[fillvar()]], na.rm = TRUE)
    })
    
    output$map <- renderLeaflet({
        pal_fun <- colorNumeric("YlOrRd", domain = c(fillvar_min(), fillvar_max()))
        
        leaflet(kart_kommunedata()) %>%
            addPolygons(stroke = TRUE, weight = 0.5, color = "black", smoothFactor = 0, 
                        fillColor = ~pal_fun(kart_kommunedata()[[fillvar()]]),
                        fillOpacity = 0.7) %>%
            addProviderTiles(provider = "Stamen.TonerLite")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
