library(shinydashboard)
library(shiny)
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(plotly)
library(ggplot2)
#library(readr)
library(DT)
library(rgdal)
library(leaflet)
library(geojsonio)
library(datasets)
library(lubridate)

GA.data <- read_csv("data/GAdata_week.csv")

GA.data$X1 <- NULL
GA.data$date <- gsub('^(.{4})(.*)$', '\\1-\\2', GA.data$date)
GA.data$date <- gsub('^(.{7})(.*)$', '\\1-\\2', GA.data$date)
GA.data$date <- ymd(GA.data$date)

#GA.data2  <- read_csv("data/GAdata3.csv")

cities <- read_csv("data/UScities.csv")

state.names <- read_csv("data/state_names.csv")

states <- geojson_read( 
  x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
  what = "sp")


header <- dashboardHeader(title = "NAAEE Web Analytics")

# Define UI for application
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sources", tabName = "map", icon = icon("osi")),
    menuItem("Pageviews", tabName = "pageviews", icon = icon("chart-bar")),
    menuItem("Pages", tabName = "pages", icon = icon("osi")),
    dateRangeInput("date", 
                   label = "Date range",
                   start = as.Date("2019-09-01"), end = as.Date("2019-09-10"),
                   min = as.Date("2019-09-01"), max = as.Date("2019-09-30") ,
                   format = "mm-dd-yyyy", startview = 'month'),
    textInput("text",
              label = "URL search",
              value = "naaee"),
    checkboxInput(inputId = "showCities",
                  label = "Show cities"),
    downloadButton(outputId = "download",
                 label = "Download selected data"))
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
          fluidRow(
            leafletOutput("USmap")
            )
          ),
    tabItem(tabName = "pageviews",
            fluidRow(
              leafletOutput("USmap")
            )
    ),
    tabItem(tabName = "pages",
            fluidRow(
              box(title = "Page Information Table", DT::dataTableOutput(outputId = "datatable"), width = 6)
            )
          )
        )
      )

ui <- dashboardPage(header, sidebar, body, skin = "green")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  GAdata <- reactive({
    req(input$date, input$text)
    subset(GA.data, GA.data$date >= input$date[1] &
             GA.data$date <= input$date[2] &
             (str_detect(GA.data$pagePath, tolower(input$text), negate=FALSE) == TRUE))
  })
  
  GAdata.pageviews <- reactive ({
    req(input$date, input$text)
    
    # data.pageviews <- subset(GA.data, GA.data$date >= input$date[1] &
    #                                   GA.data$date <= input$date[2] &
    #                                   (str_detect(GA.data$pagePath, tolower(input$text), negate=FALSE) == TRUE))
    
    US.cities <- subset(GAdata(), country == "United States") %>%
      left_join(state.names, by = "region")
    US.cities$city.state <- paste(US.cities$city, US.cities$state.abb, sep = ", ")
    US.cities %<>% subset(city.state %in% cities$city.state)
    US.cities <- aggregate(list(US.cities$users, US.cities$pageviews), by = list(US.cities$city.state), sum)
    colnames(US.cities) <- c("city.state","users","pageviews")
    
    US.cities %<>% left_join(cities, by = "city.state")
    
    return(US.cities)
  })

  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste('NAAEE_WebsiteData.csv')
  #   },
  #   content = function(con) {
  #     write_csv(GAdata(), con)
  #   }
  # )
  
  output$USmap <- renderLeaflet({

    geo.states <- states

    geo.states <- subset(geo.states, name != "Puerto Rico")
    state.names <- unique(geo.states$name)

    GAstate <- aggregate(pageviews ~ region, data = GAdata(), sum) %>%
      subset(region %in% state.names)
    colnames(GAstate) <- c("name", "pageviews")

    geo.states@data <- left_join(geo.states@data, GAstate)

    pal <- colorBin(palette = "Oranges",
                    bins = c(0,
                             round((max(geo.states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                             round((2 * max(geo.states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                             round((3 * max(geo.states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                             round((4 * max(geo.states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                             max(geo.states$pageviews, na.rm = TRUE) + 1),
                    domain = geo.states$pageviews)

    return(
      leaflet(geo.states) %>%
        addProviderTiles("OpenStreetMap") %>%
        addPolygons(fillColor = ~pal(pageviews),
                    weight = 1,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 0.75,
                    popup = ~paste0("<b>", name, "</b> ",
                                    "<br>Pageviews: ", format(pageviews, big.mark = "," ))) %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = ~pageviews,
                  title = "Pageviews") %>%
        mapOptions(zoomToLimits = "first")
    )
    })
  
  
  
  observe({
    city.data <- GAdata.pageviews()

    if (input$showCities) {
      leafletProxy("USmap", data = city.data) %>%
        addMarkers(lng = ~lng,
                   lat = ~lat,
                   popup = ~paste0("<b>", city.state, "</b> ",
                                   "<br><b>", format(input$date[1], "%m/%d/%Y"), " - ", format(input$date[2], "%m/%d/%Y"), "</b>",
                                   "<br>Pageviews: ", format(pageviews, digits = 0, big.mark = "," ),
                                   "<br>Users: ", format(users, digits = 0, big.mark = "," )))  %>%
        mapOptions(zoomToLimits = "first")
    } else {
      leafletProxy("USmap", data = city.data) %>%
        clearMarkers() %>%
        mapOptions(zoomToLimits = "first")
    }
  })

  output$datatable <-
    DT::renderDataTable({
      DT::datatable(data = GAdata())
    })
  
  
  
  # output$datatable <- 
  #   DT::renderDataTable({
  #     DT::datatable(data = GA.data(), options = list(
  #       autoWidth = TRUE,
  #       scrollX = TRUE,
  #       scrollY = 400
  #     ), colnames = c("Date", "URL", "Page Title", "Users", "Pageviews", "Sessions", "Bounce Rate", "Exit Rate", "Avg Time on Page", "Entrances"))
  #   })
  

  
  
  
}

# Run the application 
shinyApp(ui, server)

