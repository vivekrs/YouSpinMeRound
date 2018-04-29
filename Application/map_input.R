library(rgdal)
library(leaflet)
library(leaflet)    
library(shiny)
library(shinydashboard)



data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")


uscounties <-  rgdal::readOGR("uscounties.geojson")
# us <-  rgdal::readOGR("us-states.geojson")
usstates <-  rgdal::readOGR("usstates.geojson")

state_popup <- paste0("<strong>Name: </strong>", 
                      usstates$NAME)
counties_popup <- paste0("<strong>Name: </strong>", 
                      uscounties$NAME)


state <- TRUE

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

darkPalette <- c('#616161','#FFE0B2','#FFCC80','#FFA726','#FF9800','#FB8C00','#F57C00','#E65100')
# lightPalette <- c('#FFE0B2','#f2ba80','#ed955c','#df614c','#c24549','#703838')
lightPalette <- c('#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
colorBlindPalette <- c('#ffd59b','#ffa474','#f47461','#db4551','#b81b34','#8b0000','#3f0000')
satellitePalette <- c('#FFE0B2','#FFCC80','#FFA726','#FB8C00','#F57C00','#E65100','#702800')

palette = darkPalette

draw_tracks <- function(map, df)
{
  # print(palette)
  colorQuantile( 
              satellitePalette,
              domain = unique(as.numeric(df[1:2000,]$inj)),
              7,
               reverse = FALSE) -> pal

  for(i in 1:2000){
        map <- addPolylines(map, 
        lat = as.numeric(df[i, c('slat','elat' )]), lng = as.numeric(df[i, c('slon', 'elon')]),
        color = pal(as.numeric(df[i,c('inj')])), label = paste(df[i, c('inj')], ":injuries"), opacity = 0.7
      )
  }
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
  # palette <- reactiveValues(pal = darkPalette)
  
  observeEvent(input$myMap_groups, {
    mapType <- input$myMap_groups
    # if(mapType == 'Dark') {palette$pal <- darkPalette}
    # else if(mapType == 'Light') {palette$pal <- lightPalette}
    # # else if(mapType == 'Satellite') {palette$pal <- satellitePalette}
    # # else if(mapType == 'Minimal') {palette$pal <- lightPalette}
    # # else {palette$pal <- colorBlindPalette}
  })
  
  
  # function to create foundational map
  foundational.map <- function(bool){
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
      setView(lat = 37.0902,
              lng = -95.7129,
              zoom = 5)
    if(bool){
     y <- y%>%
      addPolygons( data = usstates
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = usstates$GEO_ID
                   , group = "click.list" , popup = state_popup)}
    else{

     y <- y%>%
      addPolygons( data = uscounties
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = uscounties$GEO_ID
                   , group = "click.list" , popup = counties_popup)

    } 

      y 
  }
  
  # reactiveVal for the map object, and corresponding output object.
  # myMap_reval <- reactiveVal(foundational.map())
  output$myMap <- renderLeaflet({
     
    map<- foundational.map(state) 
      
    # draw_tracks(map , data, palette$pal)
    draw_tracks(map , data)
    #myMap_reval()
  }) 
  
  
  # To hold the selected map region id use it to get the state or county name.
  click.list <- shiny::reactiveValues( ids = vector() )
  
  shiny::observeEvent( input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
    
    click <- input$myMap_shape_click
    click.list$ids <- click$id 
    #lines.of.interest <- us[ which( us$GEO_ID %in% click.list$ids ) , ] # for later
    print(code_get(click$id,state))
 
  }) # end of shiny::observeEvent({})
   


} 

shiny::shinyApp( ui = ui, server = server)


#https://www.r-bloggers.com/plotting-driving-routes-and-rental-data-for-houston-gepaf-gmap-plotly-leaflet/

