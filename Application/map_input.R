library(rgdal)
library(scales)
library(leaflet)
library(leaflet)    
library(shiny)
library(shinydashboard)
library(dplyr)
print(Sys.time())


data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")

county_heatmap <- read.csv("data/countydata.csv", fileEncoding = "UTF-8-BOM")


lines <-  rgdal::readOGR("data/alldata.geojson")

uscounties <-  rgdal::readOGR("uscounties.geojson")
# us <-  rgdal::readOGR("us-states.geojson")

usstates <-  rgdal::readOGR("usstates.geojson")

state_popup <- paste0("<strong>Name: </strong>", 
                      usstates$NAME)

uscounties<- merge(uscounties , county_heatmap , by.x="GEO_ID", by.y = "geoId")

uscounties[is.na(uscounties$inj),"inj"]<-0

counties_popup <- paste0("<strong>Name: </strong>", 
                      uscounties$NAME , " Injuries:",uscounties$inj)


geoid<- list(stf=vector() , ctf= vector())
group<-list(id=vector())

state <- FALSE
heatmap <- FALSE
heatmapby<- "inj"
#helper function to get ctf and stf from map input
code_get <- function(id ,  state = FALSE)
{
  string <- sub(".*US", "", id)
  code <- list(stf= -1 , ctf = -1)
  
  code$stf <- as.numeric(substr(string,0,2))
  
  if(!state)
  {
    code$ctf<-as.numeric(substr(string,3,5))
  }
  return (code)
  
}



draw_tracks <- function(map , df , heatmap , group )
{
  print(group)
 if(!heatmap){ 
  colorNumeric(c("#00FF15", "#faff00","#FF0000", "#17129e"), 
             domain = as.numeric(df$inj),
             alpha = FALSE) -> pal

   print(Sys.time())
  for(i in 1:2470){
        map <- addPolylines(map, 
      lat = as.numeric(df[i, c('slat','elat' )]), lng = as.numeric(df[i, c('slon', 'elon')])
      , color = pal(as.numeric(df[i,c('inj')]))
      )
    
  }}
  print(Sys.time())
  return (map)
}


ui <- fluidPage(
  # place the contents inside a box
  shinydashboard::box(
    width = 12
    , title = "Click on the map!"
    , column(
      width = 12
      , leaflet::leafletOutput( outputId = "myMap"
                                , height = 850
      )
    )
  ) # end of the box
) # end of fluid page

# create the server
server <- function( input, output, session ){
  
  # function to create foundational map
  foundational.map <- function(state ,heatmap , heatmapby){
    y <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Light') %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Minimal') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = 'Colorblind Safe') %>%
      addLayersControl(
        baseGroups = c("Dark","Light", "Satellite", "Minimal", "Colorblind Safe"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lat = 41.881832,
              lng = -87.623177,
              zoom = 4)
    if(state & !heatmap){
     y <- y%>%
      addPolygons( data = usstates
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = usstates$GEO_ID
                   , group = "click.list" , popup = state_popup)}
    else if( !state & !heatmap){

     y <- y%>%
      addPolygons( data = uscounties
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = uscounties$GEO_ID
                   , group = "click.list" , popup = counties_popup)

    }
    else if(heatmap){

      y <- y%>%
      addPolygons( data = uscounties
                   , fillOpacity = rescale(uscounties[[heatmapby]] , c(0,1)) 
                   , opacity = rescale(uscounties[[heatmapby]] , c(0,1)) 
                   , color = "#FF0000"
                   , weight = 2
                   , layerId = uscounties$GEO_ID
                   , group = "click.list" , popup = counties_popup)


    } 

      y 
  }
  
  # reactiveVal for the map object, and corresponding output object.
   myMap_reval <- reactiveVal(foundational.map(state , heatmap , heatmapby))
  output$myMap <- renderLeaflet({
     
    map<-  myMap_reval()
      
    draw_tracks(map , data , heatmap ,  group)

    #
  }) 
  
  # To hold the selected map region id use it to get the state or county name.
  
  
  
  shiny::observeEvent( input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
    
    click <- input$myMap_shape_click
    
    geoid$stf <- code_get(click$id,state)$stf
    geoid$ctf <- code_get(click$id,state)$ctf
    #lines.of.interest <- us[ which( us$GEO_ID %in% click.list$ids ) , ] # for later
    print(geoid)
 
  }) # end of shiny::observeEvent({})
   

  shiny::observeEvent( input$myMap_groups, ignoreNULL = T,ignoreInit = T, {
    
    click <- input$myMap_groups
    group$id <- click
    print(group)
  }) 


} 

shiny::shinyApp( ui = ui, server = server)


#https://www.r-bloggers.com/plotting-driving-routes-and-rental-data-for-houston-gepaf-gmap-plotly-leaflet/

print(Sys.time())
foundational.map(FALSE, FALSE, "") %>% addPolygons( data = lines )
print(Sys.time())
