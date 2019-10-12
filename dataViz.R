library(readr)
library(rgdal)
library(leaflet)
library(geojsonio)
library(datasets)
library(dplyr)
library(sf)

setwd("C://RShiny//project2-mkennady")

GAdata <- read_csv("GAdata.csv")


# data from: https://github.com/datasets/geo-countries

countries <- readOGR(dsn = "data/geo-countries-master/data/countries.geojson")

test.countries <- countries@data

countries@data <- merge(countries@data, GAcountries, sort = FALSE, by.x = "ADMIN", by.y = "country")

test <- semi_join(countries@data, GAcountries)
test <- test[,c(1,4)]
colnames(test) <- c("country", "pageviews")

test2 <- rbind(test, GAcountries)



pal <- colorNumeric(palette = "Blues",
                domain = countries$pageviews)


leaflet(countries) %>%
  addProviderTiles("OpenStreetMap") %>% 
  addPolygons(fillColor = ~pal(pageviews),
              weight = 1,
              opacity = 1,
              color = "black")








# STATE GEOJSON

# transfrom .json file into a spatial polygons data frame
states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

# states <- readOGR("data//gz_2010_us_040_00_500k.json")

states <- subset(states, name != "Puerto Rico")
state.names <- unique(states$name)

mapping.data <- read_csv("data/GAdata.csv")

GAstate <- aggregate(pageviews ~ region, data = mapping.data, sum)

GAstate <- subset(GAstate, region %in% state.names)

colnames(GAstate) <- c("name", "pageviews")



# get state data and merge

# states@data <- merge(states@data, GAstate, sort = FALSE, by = "NAME")


states@data <- left_join(states@data, GAstate)



# Create leaflet map

pal <- colorBin(palette = "Oranges",
                bins = c(0, 
                         round((max(states$pageviews, na.rm = TRUE) + 1) / 5, 0), 
                         round((2 * max(states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                         round((3 * max(states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                         round((4 * max(states$pageviews, na.rm = TRUE) + 1) / 5, 0),
                         max(states$pageviews, na.rm = TRUE) + 1),
                domain = states$pageviews)



leaflet(states) %>%
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
            title = "Pageviews")



# LEAFLET POINTS

points <- read_csv("data/GAdata2.csv")
points$X1 <- NULL
points$metro <- NULL

state.names <- cbind(state.abb, state.name)
state.names <- rbind(state.names, c("DC", "District of Columbia")) %>%
  as.data.frame()

colnames(state.names)[2] <- "region"



points <- left_join(points, state.names, by = "region")



# %>%
#   subset(country == "United States")

points$city.state <- paste(points$city, points$state.abb, sep = ", ")

#write_csv(points, "data/GAdata3.csv")


points.latlong <- read_csv("data/us-cities-lat-long-pop2.csv")

points.latlong <- points.latlong[ c( "city", "state_id", "lat", "lng", "population") ]

points.latlong <- top_n(points.latlong, 100, population)

points.latlong$city.state <- paste(points.latlong$city, points.latlong$state_id, sep = ", ")


test <- merge(points, points.latlong)

test <- aggregate(list(test$users, test$pageviews), by = list(test$city.state), sum)

test <- merge(test, points.latlong)


leaflet(test) %>%
  addTiles() %>%
  addMarkers(lng = ~lng,
             lat = ~lat,
             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE),
             popup = ~paste0("<b>", city.state, "</b> ",
                             "<br>Pageviews: ", format(pageviews, digits = 0, big.mark = "," )))
  
  
  addCircleMarkers(
    radius = ~pageviews / 175,
    popup = ~paste0("<b>", city.state, "</b> ",
                                       "<br>Pageviews: ", format(pageviews, digits = 0, big.mark = "," )
  ))
  
  
  








points <- read_csv("data/GAdata2.csv")
points$X1 <- NULL

points.latlong <- read_csv("data/us-cities-lat-long.csv")

temp <- unique(points$metro)

temp2 <- subset(points, metro %in% temp)

random <- unique(points.latlong$city)

test <- aggregate(pageviews ~ city, data = points, sum)
test <- subset(test, city %in% unique(points.latlong$city))

test2 <- na.omit(left_join(points.latlong, test))


leaflet(test) %>% addTiles() %>% addMarkers(
  ~long, 
  ~lat,
  clusterOptions = markerClusterOptions()
)



temp3 <- paste(points.latlong$city, points.latlong$state_id, sep = ", ")

temp4 <- subset(test@data, as.character(test$NAME) %in% temp3)






