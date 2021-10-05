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
library(htmltools)
library(colourpicker)

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(tags$style(type = "text/css", css_fix))
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
                label = "Last opp kommunedata (.csv, .xlsx, .dta)",
                buttonLabel = "Bla igjennom..."),
      selectInput("keyvar", label = "Variabel med kommunenummer",
                  choices = NULL),
      selectInput("kommuneår", label = "År for kommuneinndeling",
                  choices = c("2020"),
                  selected = NULL),
      selectInput("fillvar", label = "Variabel som skal plottes",
                  choices = NULL),
      radioButtons("fillvar_type", label = "Kontinuerlig eller diskret variabel?",
                   choices = c("Kontinuerlig", "Diskret")),
      radioButtons("fillvar_fmt", label = "Format for plotting",
                   choices = c("Rådata", "Naturlig inndeling ('Jenks')", "Persentiler")),
      sliderInput("fillvar_numcat", label = "Antall klasser/kategorier", 
                  min = 1, max = 20, step = 1, value = 5),
      radioButtons("show_which", label = "Kommuner som skal vises",
                   choices = c("Alle", "Kun kommuner med data")),
      checkboxInput("fylke", label = "Vis fylkeomriss (kun 2020-inndeling per nå)", value = TRUE),
      colourInput("border", label = "Farge på kommunegrenser", value = "gray", allowTransparent = TRUE),
      selectInput("palette", 
                  label = a("Fargepalett", href = "https://colorbrewer2.org/", target = "_blank"),
                  choices = NULL),
      checkboxInput("palette_rev", label = "Omvendt rekkefølge på farger", value = FALSE),
      colourInput("na_color", label = "Farge for manglende data", value = "#cccccc"),
      textInput("fillvar_lab", label = "Overskrift for legenden (valgfritt)"),
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
      df <- read_xlsx(infile$datapath)
    } else if (tolower(file_ext(infile$datapath)) == "csv") {
      df <- read_csv2(infile$datapath)
    } else if (tolower(file_ext(infile$datapath)) == "dta") {
      df <- read_dta(infile$datapath) %>%
        mutate(across(where(is.labelled), as_factor))
    }
    
    return(df)
  })
  
  observe({
    vars <- names(kommunedata())
    updateSelectInput(session, "keyvar", "Variabel med kommunenummer", choices = vars)
    updateSelectInput(session, "fillvar", "Variabel som skal plottes", choices = vars)
  })
  
  kart <- eventReactive(input$kommuneår, {
    filnavn <- paste0("kommuner_", input$kommuneår, "_simplest_2.rds")
    readRDS(filnavn) %>%
      st_set_crs(25833) %>%
      mutate(kommunenummer = as.numeric(kommunenummer))
  })
  
  kart_kommunedata <- eventReactive(input$plot, {
    req(input$keyvar)
    kdata <- kommunedata() %>%
      mutate(kommunenummer = as.numeric(.[[input$keyvar]]),
             fillvar = .[[input$fillvar]])
    # Kommuner som skal vises
    if(input$show_which == "Alle") {
      d <- left_join(kart(), kdata, by = "kommunenummer") %>%
        st_transform(4326) # Need EPSG:4326 for leaflet
    } else {
      d <- inner_join(kart(), kdata, by = "kommunenummer") %>%
        st_transform(4326) # Need EPSG:4326 for leaflet
    }

    # Lag klasser hvis format for plotting er satt til jenks eller persentiler
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
    if (input$fillvar_fmt == "Rådata" | input$fillvar_type == "Diskret") {
      disable("fillvar_numcat")
    } else {
      enable("fillvar_numcat")
    }
  })
  
  # Diskrete variabler vises som de er
  observe({
    if (input$fillvar_type == "Diskret") {
      disable("fillvar_fmt")
    } else {
      enable("fillvar_fmt")
    }
  })
  
  # Liste med paletter
  observe({
    if (input$fillvar_type == "Diskret") {
      pals <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
      updateSelectInput(session, "palette", choices = sort(pals), selected = "Set2")
    } else {
      pals <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", 
                "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
                "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
      updateSelectInput(session, "palette", choices = sort(pals), selected = "GnBu")
    }
  })
  
  output$map <- renderLeaflet({
    if (input$fillvar_fmt %in% c("Naturlig inndeling ('Jenks')", "Persentiler")) {
      pal_fun <- colorFactor(input$palette, kart_kommunedata()$fillvar,
                             reverse = input$palette_rev)
    }
    if (input$fillvar_type == "Diskret") {
      pal_fun <- colorFactor(input$palette, kart_kommunedata()$fillvar,
                             reverse = input$palette_rev)
    }
    if (input$fillvar_type == "Kontinuerlig" & input$fillvar_fmt == "Rådata") {
      pal_fun <- colorNumeric(input$palette, kart_kommunedata()$fillvar, 
                              reverse = input$palette_rev)   
    }
    
    lmap <- leaflet(kart_kommunedata()) %>%
      addPolygons(stroke = TRUE, weight = 0.5, color = "black", smoothFactor = 0, 
                  fillColor = ~pal_fun(fillvar),
                  fillOpacity = 0.7, 
                  popup = popupTable(kart_kommunedata())) %>%
      addProviderTiles(provider = "Stamen.TonerLite")  %>%
      addLegend("bottomright", pal = pal_fun, values = ~fillvar,
                title = fillvar_lab(),
                opacity = 1
      )
    
    lmap <- if (input$fylke == TRUE) {
      filnavn <- paste0("fylker_", input$kommuneår, "_simplest_2.rds")
      fylker <- readRDS(filnavn) %>%
        st_set_crs(25833) %>%
        st_transform(4326)
      
      lmap %>%
        addPolygons(data = fylker, stroke = TRUE, weight = 1.2, color = "black")
    } else {
      lmap
    }
  })
  
  data <- reactiveValues()
  
  data$map_static <- reactive({
    
    palette_dir <- ifelse(input$palette_rev == TRUE, -1, 1)
    
    bordercol <- case_when(input$border == "Grå" ~ "gray",
                           input$border == "Svart" ~ "black",
                           input$border == "Hvit" ~ "white")
    
    if (input$fillvar_type == "Kontinuerlig" & input$fillvar_fmt == "Rådata") {
      kart_kommunedata() %>%
        st_transform(25833) %>%
        ggplot() +
        geom_sf(aes(fill = fillvar), color = input$border) +
        scale_fill_distiller(type = "div",
                             palette = input$palette,
                             direction = palette_dir,
                             name = fillvar_lab(),
                             guide = guide_colorbar(reverse = TRUE),
                             na.value = input$na_color) +
        labs(fill = fillvar_lab()) +
        theme_nothing(legend = TRUE) +
        theme(legend.title = element_text(size = 14),
              legend.text = element_text(size = 10),
              legend.position = c(0.55, 0.5),
              legend.justification = c("left", "top"))
      } else {
      kart_kommunedata() %>% 
        st_transform(25833) %>% 
        ggplot() +
        geom_sf(aes(fill = factor(fillvar)), color = input$border) +
        scale_fill_brewer(palette = input$palette, direction = palette_dir, 
                          na.value = input$na_color) +
        labs(fill = fillvar_lab()) +
        theme_nothing(legend = TRUE) +
        theme(legend.title = element_text(size = 14),
              legend.text = element_text(size = 10),
              legend.position = c(0.55, 0.5),
              legend.justification = c("left", "top"))
      }
  })
  
  output$map_static <- renderPlot(width = 700, height = 700, res = 96, {
    if (input$fylke == TRUE) {
      filnavn <- paste0("fylker_", input$kommuneår, "_simplest_2.rds")
      fylker <- readRDS(filnavn) %>%
        st_set_crs(25833)
      
      data$map_static() + 
        geom_sf(data = fylker, aes(), size = 0.8, fill = alpha(1, 0))
    } else {
      data$map_static()
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("kommunekart_", input$fillvar, ".png" , sep= "")},
    content = function(file) {
      p <- data$map_static() +
        theme(legend.title = element_text(size = 28),
              legend.text = element_text(size = 20),
              legend.key.size = unit(0.5, "in"),
              legend.position = c(0.55, 0.5),
              legend.justification = c("left", "top"))  
      
      p <- if (input$fylke == TRUE) {
        filnavn <- paste0("fylker_", input$kommuneår, "_simplest_2.rds")
        fylker <- readRDS(filnavn) %>%
          st_set_crs(25833)
        
        p + 
          geom_sf(data = fylker, aes(), size = 0.8, fill = alpha(1, 0))
      } else {
        p
      }
      
      ggsave(file, plot = p, width = 14, height = 14, dpi = 192, units = "in")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
