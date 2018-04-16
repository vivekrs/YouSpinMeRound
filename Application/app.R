library(shiny)
library(leaflet)

#creates button group for filtering based on color/width
createButtonGroup <- function(){
  div(class="buttonGroup",
      tags$button(
        id = "widthBtn",
        type = "button",
        class = "btn action_button",
        img(id= "widthBtnImg", src = "width_white.png", height = "20px")
      ),
      tags$button(
        id = "colorBtn",
        class = "btn action_button",
        img(id= "colorBtnImg", src = "color.png", height = "20px")
      )
  )
}

ui <- fluidPage(
  tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
  div(id="outer-container",
      div(id = "sidebar",
          selectInput("state1", "State 1", choices = list("Alaska","Illinois","California")),
          selectInput("state2", "State 2", choices = list("Alaska","Illinois","California","Utah")),
          selectInput("chartBy", "Chart By:", choices = list("Year","Month","Hour","Distance from Chicago","County")),
          
          hr(),
          
          h4('Filter By:'),
          h5(class="filterTitle", 'Magnitude'),
          br(),
          br(),
          h5(class="filterTitle", 'Width'),
          tags$button(
            id = "colorBtn",
            class = "btn action_button colorFilter",
            img(id= "colorBtnImg", src = "color.png", height = "20px")
          ),
          sliderInput("widthSlider",'',  min = 0, 
                      max = 5000, value = c(100, 900)),
          h5(class="filterTitle", 'Length'),
          tags$button(
            id = "colorBtn",
            class = "btn action_button colorFilter",
            img(id= "colorBtnImg", src = "color.png", height = "20px")
          ),
          sliderInput("lengthSlider",'',  min = 0, 
                      max = 250, value = c(100, 200)),
          
          hr(),
          
          h5(class="filterTitle", 'Loss'),
          createButtonGroup(),
          sliderInput("lossSlider",'',  min = 50, 
                      max = 1000000, value = c(30000, 400000)),
          h5(class="filterTitles", 'Fatalities'),
          createButtonGroup(),
          sliderInput("fatalitiesSlider",'',  min = 0, 
                      max = 160, value = c(50, 100)),
          h5(class="filterTitles", 'Injuries'),
          createButtonGroup(),
          sliderInput("injuriesSlider",'',  min = 0, 
                      max = 1800, value = c(20, 400)),
          
          hr(),
          
          h5(class = "filterTitle", 'Year'),
          sliderInput("year", "",
                      min = 1965, max = 2017,
                      value = 1, step = 1, width = '100%',
                      animate =
                        animationOptions(interval = 300, loop = FALSE)),
          
          h4('Preferences:'),
          radioButtons("hourRadio", "Time Format", c("AM/PM", "24 hr"), inline = TRUE),
          radioButtons("measurementRadio", "Measurements", c("Imperial", "Metric"), inline = TRUE),
          
          actionButton("about", "About")
          ),
      div(id = "header", h1("You Spin Me Round")),
      div(id="mapOne",
          leafletOutput("sampleMap1", width="100%", height="100%")),
      div(id="mapTwo",
          leafletOutput("sampleMap2", width="100%", height="100%")),
      div(id="plots",
          plotOutput("samplePlot", width="100%", height="60vh"))
  )
)

server <- function(input, output, session) {
  # output$samplePlot <- renderPlot({hist(rnorm(100))})
  output$sampleMap1 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat = 41.881832, lng = -87.623177, zoom = 4)
    })
  output$sampleMap2 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat = 41.881832, lng = -87.623177, zoom = 4)
    })
}

shinyApp(ui = ui, server = server)