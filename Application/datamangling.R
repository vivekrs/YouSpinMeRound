library(rgdal)
library(plyr)
library(reshape2)

county_heatmap <- read.csv("data/countydata.csv", fileEncoding = "UTF-8-BOM")

uscounties <-  rgdal::readOGR("uscounties.geojson")