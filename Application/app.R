library(shiny)
library(leaflet)
library(shinyjs)

magList <-
  list(
    "All" = 10,
    "Unknown" = -9,
    "Light" = 0,
    "Moderate" = 1,
    "Significant" = 2,
    "Severe" = 3,
    "Devastating" = 4,
    "Catastrophic" = 5
  )

#creates button group for filtering based on color/width
createButtonGroup <- function() {
  div(
    class = "buttonGroup",
    tags$button(class = "btn action_button",
                img(class = "colorBtnImg",
                    src = "images/color.png")),
    tags$button(
      type = "button",
      class = "btn action_button",
      img(class = "widthBtnImg",
          src = "images/width_white.png")
    )
  )
}
createColorButtonGroup <- function() {
  div(class = "buttonGroup",
      tags$button(class = "btn action_button",
                  img(class = "colorBtnImg",
                      src = "images/color.png")))
}

ui <- fluidPage(
  tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
  div(
    id = "container",
    div(
      id = "controls",
      h1("You Spin Me Round"),
      div(
        class = "filter-group",
        selectInput(
          "state1",
          "State 1",
          choices = list("Alaska", "Illinois", "California")
        ),
        selectInput(
          "state2",
          "State 2",
          choices = list("Alaska", "Illinois", "California", "Utah")
        )
      ),
      
      div(
        class = "filter-group",
        selectInput(
          "chartBy",
          "Chart By:",
          choices = list("Year", "Month", "Hour", "Distance from Chicago", "County")
        ),
        h3("Year"),
        div(
          id = 'yearDiv',
          class = 'filterContainer',
          sliderInput(
            "year",
            "",
            min = 1965,
            max = 2016,
            value = 1,
            step = 1,
            width = '100%',
            animate =
              animationOptions(interval = 300, loop = FALSE)
          )
        )
      ),
      
      div(
        class = "filter-group",
        h3("Magnitude"),
        div(
          id = 'magnitudeDiv',
          class = 'filterContainer',
          checkboxGroupInput(
            "magGroup",
            label = '',
            choices = magList,
            selected = 0
          )
        )
      ),
      
      div(
        class = "filter-group",
        createColorButtonGroup(),
        h3("Width"),
        div(
          id = 'widthDiv',
          class = 'filterContainer',
          sliderInput(
            "widthSlider",
            '',
            min = 0,
            max = 5000,
            value = c(100, 900)
          )
        ),
        
        createColorButtonGroup(),
        h3("Length"),
        div(
          class = 'filterContainer',
          sliderInput(
            "lengthSlider",
            '',
            min = 0,
            max = 250,
            value = c(100, 200)
          )
        )
      ),
      
      div(
        class = "filter-group",
        createButtonGroup(),
        h3("Injuries"),
        div(
          class = 'filterContainer',
          sliderInput(
            "injuriesSlider",
            '',
            min = 0,
            max = 1800,
            value = c(20, 400)
          )
        ),
        
        createButtonGroup(),
        h3("Fatalities"),
        div(
          class = 'filterContainer',
          sliderInput(
            "fatalitiesSlider",
            '',
            min = 0,
            max = 160,
            value = c(50, 100)
          )
        )
      ),
      
      div(
        class = "filter-group",
        createButtonGroup(),
        h3("Loss"),
        div(
          class = 'filterContainer',
          sliderInput(
            "lossSlider",
            '',
            min = 50,
            max = 1000000,
            value = c(30000, 400000)
          )
        )
      ),
      
      div(
        class = "filter-group",
        radioButtons("hourRadio", "Time Format", c("AM/PM", "24 hr"), inline = TRUE),
        radioButtons(
          "measurementRadio",
          "Measurements",
          c("Imperial", "Metric"),
          inline = TRUE
        )
      ),
      
      tags$button(class = "btn action_button",
                  img(class = "aboutBtnImg",
                      src = "images/about.png"))
    ),
    div(id = "mapOne",
        leafletOutput(
          "sampleMap1", width = "100%", height = "100%"
        )),
    div(id = "mapTwo",
        leafletOutput(
          "sampleMap2", width = "100%", height = "100%"
        )),
    div(id = "plotOne",
        plotOutput("samplePlot1", width = "100%")),
    div(id = "plotTwo",
        plotOutput("samplePlot2", width = "100%"))
  )
)

server <- function(input, output, session) {
  #show about page
  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = "You Spin Me Round",
        "You Spin Me Round is a geospatial visualization of tornadoes data from 1965-2016.",
        br(),
        br(),
        "The data for this project is from NOAA's National Weather Service:",
        a(
          href = 'http://www.spc.noaa.gov/wcm/index.html#data',
          target = '_blank',
          'Storm Prediction Center'
        ),
        br(),
        br(),
        "This visualization allows the users to view the relative strength of the tornadoes and damage caused by them across
        different states in USA. Additionally, users can filter data by the tornado width, length, injuries, fatalities and loss. Moreover, users
        can also choose the time frame(year, month, hour) for which they want to see the tornado data.",
        # a(href = '', target='_blank', 'More Details'),
        hr(),
        h5('Team: R You Shiny'),
        "Amey Barapatre | Sai Phaltankar | Jaspreet Kaur Sohal | Vivek R. Shivaprabhu",
        easyClose = TRUE
      )
    )
  })
  
  # output$samplePlot <- renderPlot({hist(rnorm(100))})
  output$sampleMap1 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lat = 41.881832,
              lng = -87.623177,
              zoom = 4)
  })
  output$sampleMap2 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lat = 41.881832,
              lng = -87.623177,
              zoom = 4)
  })
}

shinyApp(ui = ui, server = server)