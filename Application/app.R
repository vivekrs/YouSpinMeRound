library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)


getStates <- function() {
  data <- read.csv("data/statenames.csv", fileEncoding = "UTF-8-BOM")
  states <- setNames(as.list(data$Code), data$Name)
  return(states)
}
states <- getStates()

data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")
magnitudes <- c(levels(data$mag), "All")

#creates button group for filtering based on color/width
createButtonGroup <- function(filter_id) {
  div( 
    class = "buttonGroup",
    prettyToggle(
      inputId = paste(filter_id , "Width", sep=""),
      label_on = "", label_off="",
      # animation = 'pulse',
      icon_on = icon("bars",lib = "font-awesome"),
      icon_off = icon("bars", lib = "font-awesome"),
      status_on = "primary", status_off = "default",
      shape = "square", outline = TRUE
    ),
    prettyToggle(
      inputId = paste(filter_id , "Color", sep=""),
      label_on = "", label_off="",
      icon_on = icon("tint"),
      icon_off = icon("tint"),
      status_on = "primary", status_off = "default",
      shape = "round", outline = TRUE
    )
  )
}
createColorButtonGroup <- function(filter_id) {
  div(class = "colorButton",
      prettyToggle(
        inputId = paste(filter_id , "Color", sep=""), 
        label_on = "", label_off="",
        icon_on = icon("tint"),
        icon_off = icon("tint"),
        status_on = "primary", status_off = "default",
        shape = "round", outline = TRUE
      ) )
}

ui <- fluidPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  div(
    id = "container",
    div(
      id = "controls",
      div(
        class = "filter-group",
        h1("You Spin Me Round"),
        actionButton("aboutButton", class = "action_button about_button", label = img(src = "images/about.png"))
      ), 

      div(class = "spacer"),      
      div(
        class = "filter-group",
        selectInput("state1Select", "State 1", states, "IL"),
        selectInput("state2Select", "State 2", states, "TX")
      ),
      
      div(
        class = "filter-group",
        selectInput("chartBySelect", "Chart By:",
          choices = list("Year", "Month", "Hour", "Distance from Chicago", "County")
        ),
        h3("Year"),
        div(
          id = "yearDiv",
          class = "filterContainer",
          sliderInput("yearSlider", "", min = 1965, max = 2016, value = c(2000, 2016), step = 1,
            width = "100%", animate = animationOptions(interval = 300, loop = FALSE)
          )
        )
      ),
      
      div(class = "spacer"),    
      div(
        class = "filter-group",
        createButtonGroup('magnitudeFilter'),
        h3("Magnitude"),
        div(
          id = "magnitudeDiv",
          class = "filterContainer",
          checkboxGroupInput("magGroup", label = "", choices = magnitudes, selected = "All")
        )
      ),
      
      div(
        class = "filter-group",
        createColorButtonGroup("widthFilter"),
        h3("Width"),
        div(
          id = "widthDiv",
          class = "filterContainer",
          sliderInput("widthSlider", "", min = 0, max = 5000, value = c(0, 5000))
        ),
        
        createColorButtonGroup("lengthFilter"),
        h3("Length"),
        div(
          class = "filterContainer",
          sliderInput("lengthSlider", "", min = 0, max = 250, value = c(0, 250))
        )
      ),
      
      div(
        class = "filter-group",        
        createButtonGroup('distanceFilter'),
        h3("Distance from Chicago"),
        div(
          class = "filterContainer",
          sliderInput("distanceSlider", "", min = 0, max = 4500, value = c(0, 4500))
        ),

        createButtonGroup('loss'),
        h3("Loss"),
        div(
          class = "filterContainer",
          sliderInput("lossSlider", "", min = 0, max = 1000000, value = c(0, 1000000))
        )
      ),
      
      div(
        class = "filter-group",
        createButtonGroup('injuriesFilter'),
        h3("Injuries"),
        div(
          class = "filterContainer",
          sliderInput("injuriesSlider", "", min = 0, max = 1750, value = c(0, 1750)
          )
        ),
        
        createButtonGroup('fatalitiesFilter'),
        h3("Fatalities"),
        div(
          class = "filterContainer",
          sliderInput("fatalitiesSlider", "", min = 0, max = 160, value = c(0, 160))
        )
      ),
      
      div(class = "spacer"),    
      div(
        class = "filter-group",
        radioButtons("hourRadio", "Time Format", c("AM/PM", "24 hr"), inline = TRUE),
        radioButtons("measurementRadio", "Measurements", c("Imperial", "Metric"), inline = TRUE)
      )
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
  observeEvent(input$aboutButton, {
    showModal(
      modalDialog(
        title = "You Spin Me Round",
        p("You Spin Me Round is a geospatial visualization of tornadoes data from 1965-2016."),
        p("The data for this project is from NOAA's National Weather Service:",
        a(href = "http://www.spc.noaa.gov/wcm/index.html#data", target = "_blank", "Storm Prediction Center")),
        p("This visualization allows the users to view the relative strength of the tornadoes and damage caused by them across
        different states in USA. Additionally, users can filter data by the tornado width, length, injuries, fatalities and loss. Moreover, users
        can also choose the time frame(year, month, hour) for which they want to see the tornado data."),
        hr(),
        h5("Team: R You Shiny"),
        p("Amey Barapatre | Sai Phaltankar | Jaspreet Kaur Sohal | Vivek R. Shivaprabhu"),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$widthFilterColor, {
    print(input$widthFilterColor)
  })
  
  observeEvent(input$magnitudeFilterWidth, {
    print(input$magnitudeFilterWidth)
  })
  
  observeEvent(input$magGroup, {
    print(input$magGroup)
  })
  
  # observe({
  #   updateCheckboxGroupInput(
  #     session, 'magGroup', choices = magnitudes,
  #     selected = if (input$allNone) magnitudes
  #   )
  # })

  output$sampleMap1 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Light') %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Minimal') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = 'Colorblind Safe') %>%
      addLayersControl(
        baseGroups = c("Dark","Light", "Satellite", "Minimal", "Colorblind Safe"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lat = 41.881832,
              lng = -87.623177,
              zoom = 4)
  })
  output$sampleMap2 <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
      addProviderTiles(providers$CartoDB.Positron, group = 'Light') %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Minimal') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = 'Colorblind Safe') %>%
      addLayersControl(
        baseGroups = c("Dark","Light", "Satellite", "Minimal", "Colorblind Safe"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lat = 41.881832,
              lng = -87.623177,
              zoom = 4)
  })
}

shinyApp(ui = ui, server = server)