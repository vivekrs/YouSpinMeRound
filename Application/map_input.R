library(rgdal)
library(leaflet)
library(leaflet)    
library(shiny)
library(shinydashboard)


us <-  rgdal::readOGR("us-counties.geojson")
# us <-  rgdal::readOGR("us-states.geojson")


state_popup <- paste0("<strong>Name: </strong>", 
                      us$name)


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
  foundational.map <- function(){
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons( data = us
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = us$id
                   , group = "click.list" , popup = state_popup)
  }
  
  # reactiveVal for the map object, and corresponding output object.
  myMap_reval <- reactiveVal(foundational.map())
  output$myMap <- renderLeaflet({
    myMap_reval()
  }) 
  
  # To hold the selected map region id use it to get the state or county name.
  click.list <- shiny::reactiveValues( ids = vector() )
  
  shiny::observeEvent( input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
    
    click <- input$myMap_shape_click
    click.list$ids <- click$id 
    lines.of.interest <- us[ which( us$id %in% click.list$ids ) , ] # for later
    print(click)
 
  }) # end of shiny::observeEvent({})
   
} 

shiny::shinyApp( ui = ui, server = server)