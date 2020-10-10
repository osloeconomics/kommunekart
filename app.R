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

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

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
            textInput("fillvar_lab", label = "Fyll inn etikett for variabel (valgfritt)"),
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
            st_transform(4326) %>%
            mutate(fillvar = .[[input$fillvar]])
    })
    
    fillvar_lab <- reactive({
        if (input$fillvar_lab == "") 
            input$fillvar
        else
            input$fillvar_lab
    })
    
    output$map <- renderLeaflet({
        pal_fun <- colorNumeric("GnBu", kart_kommunedata()$fillvar)
        
        leaflet(kart_kommunedata()) %>%
            addPolygons(stroke = TRUE, weight = 0.5, color = "black", smoothFactor = 0, 
                        fillColor = ~pal_fun(fillvar),
                        fillOpacity = 0.7) %>%
            addProviderTiles(provider = "Stamen.TonerLite") %>%
            addLegend("bottomright", pal = pal_fun, values = ~fillvar,
                      title = fillvar_lab(),
                      opacity = 1
            ) %>%
            htmlwidgets::prependContent(html_fix)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
