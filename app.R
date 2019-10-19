library(shinydashboard)
library(shiny)
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(plotly)
library(ggplot2)
library(DT)
library(rgdal)
library(leaflet)
library(geojsonio)
library(datasets)
library(lubridate)
library(scales)

GA.data <- read_csv("data/GAdata2018_2019.csv")

cities <- read_csv("data/UScities.csv")

state.names <- read_csv("data/state_names.csv")

states <- geojson_read(
  x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
  what = "sp")

header <- dashboardHeader(title = "NAAEE Web Analytics")

# Define sidebar for dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("map")),
    menuItem("Pageviews", tabName = "pageviews", icon = icon("chart-bar")),
    menuItem("Sources", tabName = "sources", icon = icon("arrows-alt-h")),
    menuItem("Pages", tabName = "pages", icon = icon("file")),
    
    dateRangeInput("date", 
                   label = "Select dates",
                   start = as.Date("2019-09-01"), end = as.Date("2019-09-30"),
                   format = "mm-dd-yyyy", startview = 'year'),
    textInput("text",
              label = "URL search",
              value = "naaee"),
    checkboxInput(inputId = "showCities",
                  label = "Show cities"),
    downloadButton(outputId = "download",
                 label = "Download selected data")
))

# create dashboard body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
          fluidRow(
            leafletOutput("USmap")
            )
          ),
    tabItem(tabName = "pageviews",
            fluidRow(
              box(width = 10, plotlyOutput(outputId = "pageviewsLine"))
            )
    ),
    tabItem(tabName = "sources",
            fluidRow(
              box(width = 10, plotlyOutput(outputId = "sourcesBar")))
          ),
    tabItem(tabName = "pages",
            fluidRow(
              box(title = "Webpage Information Table", 
                  DT::dataTableOutput(outputId = "datatable"), width = 12)
            )
          )
        )
      )

ui <- dashboardPage(header, sidebar, body, skin = "green")

# Define server
server <- function(input, output) {
  
  # get datasdet for inputs
  GAdata <- reactive({
    req(input$date)
    
    # ensure the text input is in the URLs
    shiny::validate(
      need(str_detect(GA.data$pagePath, tolower(input$text), negate=FALSE) == TRUE , "Please type a valid URL in the text box")
    )

    # ensure user hasn't entered blank test into the textbox
    shiny::validate(need(
      input$text != "", "Please type a valid URL in the text box"
    ))
    
    # subset data
    subset(GA.data, GA.data$date >= input$date[1] &
             GA.data$date <= input$date[2] &
             (str_detect(GA.data$pagePath, tolower(input$text), negate=FALSE) == TRUE))
  })
  
  # get dataset for cities map layer
  GAdata.pageviews <- reactive ({
    
    US.cities <- subset(GAdata(), country == "United States") %>%
      left_join(state.names, by = "region")
    US.cities$city.state <- paste(US.cities$city, US.cities$state.abb, sep = ", ")
    US.cities %<>% subset(city.state %in% cities$city.state)
    US.cities <- aggregate(list(US.cities$users, US.cities$pageviews), by = list(US.cities$city.state), sum)
    colnames(US.cities) <- c("city.state","users","pageviews")
    
    US.cities %<>% left_join(cities, by = "city.state")
    
    return(US.cities)
  })
  

# Download csv of data
  output$download <- downloadHandler(
    filename = function() {
      paste('NAAEE_WebsiteData.csv')
    },
    content = function(con) {
      write_csv(GAdata(), con)
    }
  )
  
  # create empty map
  output$USmap <- renderLeaflet({

    geo.states <- states
    
    leaflet(geo.states) %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(-98.5795, 39.8283, zoom = 4) %>%
      addControl("<b>Pageviews and Users in the United States</b>", position = "topleft")

    })
  
  # choropleth map of US states showing pageviews
  observe({
    
    geo.states <- states
    USmap.data <- GAdata()
    
    geo.states <- subset(geo.states, name != "Puerto Rico")
    state.names <- unique(geo.states$name)
    
    USmap.data <- aggregate(list(USmap.data$pageviews, USmap.data$users), by = list(USmap.data$region), sum) 
    colnames(USmap.data) <- c("name", "pageviews", "users")
    subset(USmap.data, name %in% state.names)
    
    geo.states@data <- left_join(geo.states@data, USmap.data)
    
    pal <- colorNumeric(palette = "YlOrBr",
                        alpha = .2, 
                        domain = c(0, max(geo.states$pageviews)))
    
    leafletProxy("USmap", data = geo.states) %>%
      clearGroup("polygon") %>%
      addPolygons(fillColor = ~pal(pageviews),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  fillOpacity = 0.75,
                  group = "polygon",
                  popup = ~paste0("<b>", name, "</b> ",
                                  "<br>Pageviews: ",
                                  format(pageviews, big.mark = "," ),
                                  "<br>Users: ",
                                  format(users, big.mark = "," )))
    })
  
  # add layer of cities to map
  observe({
    
    city.data <- GAdata.pageviews()

    if (input$showCities) {
      leafletProxy("USmap", data = city.data) %>%
        addMarkers(lng = ~lng,
                   lat = ~lat,
                   popup = ~paste0("<b>", city.state, "</b> ",
                                   "<br>Pageviews: ", format(pageviews, digits = 0, big.mark = "," ),
                                   "<br>Users: ", format(users, digits = 0, big.mark = "," )))  %>%
        mapOptions(zoomToLimits = "first")
    } else {
      leafletProxy("USmap", data = city.data) %>%
        clearMarkers()
    }
  })
  
  # Create bar graph of sources data
  # Reference:
  # https://stackoverflow.com/questions/34605919/formatting-mouse-over-labels-in-plotly-when-using-ggplotly
  output$sourcesBar <- renderPlotly({
    
    bar.data <- GAdata()
    
    bar.data <- aggregate(list(bar.data$pageviews, bar.data$users), by = list(bar.data$source), sum)
    colnames(bar.data) <- c("source", "pageviews" ,"users")
    
    # Top 10 sources by number of users
    bar.data <- top_n(bar.data, 10, users)
    
    ggplotly(
      ggplot(data=bar.data, aes(x=reorder(source, users), y=users, group=1)) +
        geom_bar(stat="identity", fill="#74A542", aes(text = paste0("<b>Number of users: </b>", 
                                                                    format(users, big.mark = "," )))) +
        coord_flip() +
        labs(x ="Source", 
             y = "Number of Users") + 
        theme(axis.title = element_text(color="#027E98", size=16, face="bold"),
              axis.text = element_text(face = "bold", size = 12),
              axis.text.x = element_text(angle = 15),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "gray60"),
              panel.grid.major.y = element_line(colour = "white"),
              panel.grid.minor = element_line(colour = "white")) + 
        scale_y_continuous(labels = comma, 
                           breaks = seq(0, max(bar.data$users), signif(max(bar.data$users), digits = 1) / 10)) +
        ggtitle(paste0("Top sources for URLs containing \"", input$text, "\"")),
      tooltip = "text")
  })
  
  # create line graph of pageviews
  output$pageviewsLine <- renderPlotly({
    
    line.data <- GAdata()
    
    line.data <- aggregate(list(line.data$pageviews, line.data$users), by = list(line.data$date), sum)
    
    colnames(line.data) <- c("date", "pageviews" ,"users")
  
    ggplotly(
      ggplot(data = line.data, aes(x = date, y = pageviews, group=1))+
        geom_line(color = "#74A542", size = 2, aes(text = paste0("<b>Date: </b>", format(date, "%m/%d/%Y"), 
                                                                 "<br><b>Number of pageviews: </b>", format(pageviews, digits = 0, big.mark = ",")))) +
        labs(x ="Date", 
             y = "Pageviews") + 
        theme(axis.title = element_text(color="#027E98", size=15, face="bold"),
              axis.text = element_text(size = 12, face = "bold"),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "gray60"),
              panel.grid.minor = element_line(colour = "white", 
                                              linetype = "dashed")) +
        scale_y_continuous(labels = comma) +
        ggtitle(paste0("Pageviews for URLs containing \"", input$text, "\"")),
      tooltip = "text"
    )
    })

  # create datatable of selected dataset
  output$datatable <-
    
    DT::renderDataTable({
      
      DTdata <- GAdata()
      
      DTdata$city <- NULL
      DTdata$entrances <- NULL
      
      DTdata<- DTdata[,c(1,2,6:11,3:5)]
      
      colnames(DTdata) <- c("Date","Page Path", "Users", "Pageviews","Sessions","Bounce Rate (%)",
                            "Exit Rate (%)","Avg Time on Page (sec)","Source","Region","Country")
      
      DTdata$`Avg Time on Page (sec)` <- format(DTdata$`Avg Time on Page (sec)`, digits = 0)
      DTdata$`Bounce Rate (%)` <- format(DTdata$`Bounce Rate (%)`, digits = 2)
      DTdata$`Exit Rate (%)` <- format(DTdata$`Exit Rate (%)`, digits = 2)
      
      DT::datatable(data = DTdata, 
                    class = 'cell-border stripe', 
                    rownames = FALSE,
                    filter = "top",
                    options = list(
                      order = list(list(0, 'asc')),
                      autoWidth = TRUE,
                      scrollX = TRUE,
                      scrollY = 400
                      # dom = 't'
      ))
    })
}

# Run the application 
shinyApp(ui, server)

