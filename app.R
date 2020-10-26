library(dplyr)
library(sf)
library(shiny)
library(tools)
library(readxl)
library(readr)
library(haven)
library(leaflet)
library(waiter)
library(shinycssloaders)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(classInt)
library(shinyjs)
library(leafpop)

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))
options("scipen" = 999)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    HTML(html_fix),
    
    use_waiter(),
    useShinyjs(),
    
    # Application title
    titlePanel("Kommunekart"),

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
            radioButtons("fillvar_fmt", label = "Velg hvordan dataene skal vises på kartet",
                        choices = c("Rådata", "Naturlig inndeling ('Jenks')", "Persentiler", "Diskrét")),
            sliderInput("fillvar_numcat", label = "Velg antall klasser", 
                        min = 1, max = 20, step = 1, value = 5),
            radioButtons("show_which", label = "Velg hvilke kommuner som skal vises",
                         choices = c("Alle", "Kun kommuner med data")),
            textInput("fillvar_lab", label = "Fyll inn etikett for variabel (valgfritt)"),
            actionButton("plot", label = "Generer kart"),
            downloadButton("downloadPlot", "Last ned plot")
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Dynamisk kart", withSpinner(leafletOutput("map", height = 700))),
                        tabPanel("Statisk kart", withSpinner(plotOutput("map_static")))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    kommunedata <- eventReactive(input$kommune_fil, ignoreNULL = TRUE, ignoreInit = TRUE, {
        
        infile <- input$kommune_fil
        if (tolower(file_ext(infile$datapath)) == "xlsx") {
            df <- read_xlsx(infile$datapath, col_types = "text")
        } else if (tolower(file_ext(infile$datapath)) == "csv") {
            df <- read_csv2(infile$datapath, col_types = cols(.default = "c"))    
        } else if (tolower(file_ext(infile$datapath)) == "dta") {
            df <- read_dta(infile$datapath) %>%
                mutate(across(where(is.labelled), as_factor)) %>%
                mutate(across(where(is.numeric), as.character))
        }
        
        return(df)
    })
    
    observe({
        vars <- names(kommunedata())
        updateSelectInput(session, "keyvar", "Velg variabelen som inneholder kommunenummer", choices = vars)
        updateSelectInput(session, "fillvar", "Velg hvilken variabel du vil vise på kartet", choices = vars)
    })
    
    kart <- eventReactive(input$kommuneår, {
        filnavn <- paste0("kommuner_", input$kommuneår, "_simplest.rds")
        readRDS(filnavn) %>%
            st_set_crs(25833)
    })
    
    kart_kommunedata <- eventReactive(input$plot, {
        req(input$keyvar)
        if(input$show_which == "Alle") {
            d <- left_join(kart(), kommunedata(), by = c("kommunenummer" = input$keyvar)) %>%
             st_transform(4326) # Need EPSG:4326 for leaflet
        } else {
            d <- inner_join(kart(), kommunedata(), by = c("kommunenummer" = input$keyvar)) %>%
                st_transform(4326) # Need EPSG:4326 for leaflet
        }
        if (input$fillvar_fmt != "Diskrét") {
            d <- d %>%
                mutate(fillvar = as.numeric(.[[input$fillvar]]))
        } else {
            d <- d %>%
                mutate(fillvar = .[[input$fillvar]])
        }

        if (input$fillvar_fmt %in% c("Naturlig inndeling ('Jenks')", "Persentiler")) {
            if (input$fillvar_fmt == "Naturlig inndeling ('Jenks')") {
                breaks <- classIntervals(d$fillvar, style = "jenks", n = input$fillvar_numcat)$brks
            } else if (input$fillvar_fmt == "Persentiler") {
                breaks <- quantile(d$fillvar, probs = seq(0, 1, by = (1 / input$fillvar_numcat)), na.rm = TRUE)
            } 
            d <- d %>%
                mutate(fillvar = cut(fillvar, breaks = breaks, include.lowest = TRUE, dig.lab = 10))
        }
        
        return(d)
    })
    
    fillvar_lab <- reactive({
        if (input$fillvar_lab == "") 
            input$fillvar
        else
            input$fillvar_lab
    })
    
    observe({
        if (input$fillvar_fmt %in% c("Rådata", "Diskrét")) {
            disable("fillvar_numcat")
        } else {
            enable("fillvar_numcat")
        }
    })
    
    output$map <- renderLeaflet({
        if (input$fillvar_fmt %in% c("Naturlig inndeling ('Jenks')", "Persentiler")) {
            pal_fun <- colorFactor("GnBu", kart_kommunedata()$fillvar)
        } else if (input$fillvar_fmt == "Diskrét") {
            pal_fun <- colorFactor("Set2", kart_kommunedata()$fillvar)
        } else if (input$fillvar_fmt == "Rådata") {
            pal_fun <- colorNumeric("GnBu", kart_kommunedata()$fillvar)   
        }
        
        leaflet(kart_kommunedata()) %>%
            addPolygons(stroke = TRUE, weight = 0.5, color = "black", smoothFactor = 0, 
                        fillColor = ~pal_fun(fillvar),
                        fillOpacity = 0.7, 
                        popup = popupTable(kart_kommunedata())) %>%
            addProviderTiles(provider = "Stamen.TonerLite")  %>%
            addLegend("bottomright", pal = pal_fun, values = ~fillvar,
                      title = fillvar_lab(),
                      opacity = 1
        )
    })

    data <- reactiveValues()
    
    data$map_static <- reactive({
        
        if (input$fillvar_fmt %in% c("Naturlig inndeling ('Jenks')", "Persentiler")) {
            kart_kommunedata() %>% 
                st_transform(25833) %>% 
                ggplot() +
                geom_sf(aes(fill = fillvar)) +
                scale_fill_brewer(palette = "GnBu", na.value = "#808080") +
                labs(fill = fillvar_lab()) +
                theme_nothing(legend = TRUE) +
                theme(legend.title = element_text(size = 14),
                      legend.text = element_text(size = 10))
        } else if (input$fillvar_fmt == "Diskrét") {
            kart_kommunedata() %>% 
                st_transform(25833) %>% 
                ggplot() +
                geom_sf(aes(fill = fillvar)) +
                scale_fill_brewer(palette = "Set2", na.value = "#808080") +
                labs(fill = fillvar_lab()) +
                theme_nothing(legend = TRUE) +
                theme(legend.title = element_text(size = 14),
                      legend.text = element_text(size = 10))
        } else {
            kart_kommunedata() %>%
                st_transform(25833) %>%
                ggplot() +
                geom_sf(aes(fill = fillvar)) +
                scale_fill_gradientn(colours = brewer.pal(7, "GnBu"),
                                     name = fillvar_lab(),
                                     guide = guide_colorbar(reverse = TRUE)) +
                labs(fill = fillvar_lab()) +
                theme_nothing(legend = TRUE) +
                theme(legend.title = element_text(size = 14),
                      legend.text = element_text(size = 10))
        }
    })
    
    output$map_static <- renderPlot(width = 700, height = 700, res = 96, {
        data$map_static()
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("kommunekart_", input$fillvar, ".png" , sep= "")},
        content = function(file) {
            p <- data$map_static() +
                theme(legend.title = element_text(size = 28),
                      legend.text = element_text(size = 20),
                      legend.key.size = unit(0.5, "in"))
            ggsave(file, plot = p, width = 14, height = 14, dpi = 192, units = "in")
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
