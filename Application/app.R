library(shiny)
library(leaflet)

# Colors
arrivalColor <- '#6a819d'
departureColor <- '#e76e48'
colorScale <- c('#A7CAF2', '#262E38')

ui <- fluidPage(
  tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
  div(id="outer-container",
      div(id="inner-container",
          h1("You Spin Me Round"),
          radioButtons("hourRadio", "Time Format", c("AM/PM", "24 hr")),
          radioButtons("measurementRadio", "Measurements", c("Imperial", "Metric")),
          
          plotOutput("samplePlot", width="100%", height="60vh")),
      div(id="inner-container",
          leafletOutput("sampleMap1", width="100%", height="100%")),
      div(id="inner-container",
          leafletOutput("sampleMap2", width="100%", height="100%"))
  )
)

server <- function(input, output, session) {
  output$samplePlot <- renderPlot({hist(rnorm(100))})
  output$sampleMap1 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      )})
  output$sampleMap2 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      )})
}

shinyApp(ui = ui, server = server)