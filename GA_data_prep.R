library(readr)
library(rgdal)
library(leaflet)
library(geojsonio)
library(datasets)
library(dplyr)
library(sf)


# IMPORT

setwd("C://RShiny//project2-mkennady")

GAdata <- read_csv("data/GAdata_week.csv")
GAdata$X1 <- NULL




#DATES

GAdata$date <- gsub('^(.{4})(.*)$', '\\1-\\2', GAdata$date)
GAdata$date <- gsub('^(.{7})(.*)$', '\\1-\\2', GAdata$date)
GAdata$date <- ymd(GAdata$date)




# STATE MAPPING -- DO IN app.R
#
# states.geo <-
#   geojson_read(
#     x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
#     , what = "sp"
#   )
# 
# # states <- readOGR("data//gz_2010_us_040_00_500k.json")
# 
# states.geo <- subset(states.geo, name != "Puerto Rico")
# #state.names <- unique(states.geo$name)
# 
# GAstate <- aggregate(pageviews ~ region, data = GAdata, sum) %>%
#   subset(region %in% states.geo$name)
# 
# colnames(GAstate) <- c("name", "pageviews")
# 
# states.geo@data <- left_join(states.geo@data, GAstate)




