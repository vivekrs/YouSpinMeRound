library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(shiny)
library(shinyBS)

colorByArray <- c('magnitudeFilterColor','widthFilterColor', 'lengthFilterColor', 'lossFilterColor', 'distanceFilterColor', 'injuriesFilterColor',
                  'fatalitiesFilterColor')
widthByArray <- c('magnitudeFilterWidth', 'distanceFilterWidth','lossFilterWidth','injuriesFilterWidth','fatalitiesFilterWidth')

getStates <- function() {
  data <- read.csv("data/statenames.csv", fileEncoding = "UTF-8-BOM")
  states <- setNames(as.list(data$Code), data$Name)
  return(states)
}
states <- getStates()

data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")

allMag <- c("All")
magnitudes <- c(levels(data$mag), allMag)
magnitudesSelected <- c()
ignoreNextMag <- FALSE

#creates button group for filtering based on color/width
createButtonGroup <- function(filter_id) {
  if(filter_id == 'magnitudeFilter') { value = TRUE }
  else { value = FALSE }
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
    bsTooltip(paste(filter_id , "Width", sep=""),title = "Apply_Width", placement = "bottom", trigger = "hover", options = NULL),
    prettyToggle(
      inputId = paste(filter_id , "Color", sep=""),
      label_on = "", label_off="",
      icon_on = icon("tint"),
      icon_off = icon("tint"),
      status_on = "primary", status_off = "default",
      shape = "round", outline = TRUE, value = value
    ),
    bsTooltip(paste(filter_id , "Color", sep=""),title = "Apply_Color", placement = "bottom", trigger = "hover", options = NULL)
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
      ),
      bsTooltip(paste(filter_id , "Color", sep=""),title = "Apply_Color", placement = "bottom", trigger = "hover", options = NULL)
      )
}

getMagChart<- function() {
  # x<-switch (chartBy,
  #            'year' = 'yr',
  #            'month' = 'mo',
  #            'hour' = 'hr',
  #            'dist' = '~chidist'
  # )
  
  yrcount<-subset(data, st=='IL') %>% count(yr, mag) %>% group_by(yr) %>% mutate(percent = n/sum(n))
  
  updatemenus <-list(
    list(
      type = 'buttons',
      buttons = list(
        list(method = "restyle",
             args = list("y", list(data$n)),  # put it in a list
             label = "Show Number"),
        list(method = "restyle",
             args = list("y", list(data$percent*100)),  # put it in a list
             label = "Show Percent")))
  )
  
  # plot_ly(data, type = 'bar', x = ~get(x), y = ~n, marker = list(color = ~mag, showscale = TRUE)) %>% 
  #   layout(yaxis = list(title = 'value'), barmode='stack', updatemenus = updatemenus)
  plot_ly(yrcount, type = 'bar', x = ~yr, y = ~n, marker = list(color = ~mag, showscale = TRUE)) %>% 
    layout(yaxis = list(title = 'value'), barmode='stack', updatemenus = updatemenus)
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
            width = "100%", animate = animationOptions(interval = 300, loop = FALSE), sep = ""
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
        h3("Width"), uiOutput("widthUnitLabel"),
        div(
          id = "widthDiv",
          class = "filterContainer",
          sliderInput("widthSlider", "", min = 0, max = 5000, value = c(0, 5000))
        ),
        
        createColorButtonGroup("lengthFilter"),
        h3("Length"), uiOutput("lengthUnitLabel"),
        div(
          class = "filterContainer",
          sliderInput("lengthSlider", "", min = 0, max = 250, value = c(0, 250))
        )
      ),
      
      div(
        class = "filter-group",        
        createButtonGroup('distanceFilter'),
        h3("Distance from Chicago"), uiOutput("distanceUnitLabel"),
        div(
          class = "filterContainer",
          sliderInput("distanceSlider", "", min = 0, max = 4500, value = c(0, 4500))
        ),

        createButtonGroup('lossFilter'),
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


updateColorBy <- function(isSelected, session, idOfSelectedColorBy) {
  if(isSelected){
    for (i in colorByArray){
      updatePrettyToggle(session = session,
                         inputId = i,
                         value = FALSE )
    }
    
    updatePrettyToggle(session = session,
                       inputId = idOfSelectedColorBy,
                       value = TRUE)
  }
  else{
    # print(isSelected)
  }
}#updateColorBy()

#ensure that one colorBy selected all times
checkOtherColorBySelection <- function(session, input, idOfSelectedColorBy){
  anythingSelected <- FALSE
  if(input$magnitudeFilterColor) anythingSelected <- TRUE
  else if(input$widthFilterColor) anythingSelected <- TRUE
  else if(input$lengthFilterColor) anythingSelected <- TRUE
  else if(input$distanceFilterColor) anythingSelected <- TRUE
  else if(input$lossFilterColor) anythingSelected <- TRUE
  else if(input$injuriesFilterColor) anythingSelected <- TRUE
  else if(input$fatalitiesFilterColor) anythingSelected <- TRUE
  
  if(anythingSelected == FALSE){
    updatePrettyToggle(session = session,
                       inputId = idOfSelectedColorBy,
                       value = TRUE)
  }
}#checkOtherColorBySelection()

updateWidthBy <- function(isSelected, session, idOfSelectedWidthBy) {
  if(isSelected){
    for (i in widthByArray){
      updatePrettyToggle(session = session,
                         inputId = i,
                         value = FALSE )
    }
    
    updatePrettyToggle(session = session,
                       inputId = idOfSelectedWidthBy,
                       value = TRUE)
  }
  else{
    # print(isSelected)
  }
}#updateWidthBy()


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
  
  output$widthUnitLabel <- renderUI({
    if(input$measurementRadio == 'Metric'){ h4("(m)") }
    else{ h4(class= "units","(yards)") }
  })
  
  output$lengthUnitLabel <- renderUI({
    if(input$measurementRadio == 'Metric'){ h4("(km)") }
    else{ h4(class= "units","(miles)") }
  })
  
  output$distanceUnitLabel <- renderUI({
    if(input$measurementRadio == 'Metric'){ h4("(km)") }
    else{ h4(class= "units","(miles)") }
  })
  
  observeEvent(input$measurementRadio, {
    if(input$measurementRadio == 'Metric'){
      updateSliderInput(session, "widthSlider",min = 0, max = 5000, value = c(0, 5000))
      updateSliderInput(session, "lengthSlider",min = 0, max = 450, value = c(0, 450))
      updateSliderInput(session, "distanceSlider",min = 0, max = 7500, value = c(0, 7500))
      
    }
    else{
      updateSliderInput(session, "widthSlider",min = 0, max = 5000, value = c(0, 5000))
      updateSliderInput(session, "lengthSlider",min = 0, max = 250, value = c(0, 250))
      updateSliderInput(session, "distanceSlider",min = 0, max = 4500, value = c(0, 4500))
    }
  })

  observeEvent(input$magnitudeFilterColor, {
    updateColorBy(input$magnitudeFilterColor, session , "magnitudeFilterColor")  
    if(!input$magnitudeFilterColor){
      checkOtherColorBySelection(session, input, "magnitudeFilterColor")
    }
  })
  
  observeEvent(input$widthFilterColor, {
      updateColorBy(input$widthFilterColor, session , "widthFilterColor") 
    if(!input$widthFilterColor){
      checkOtherColorBySelection(session, input, "widthFilterColor")
    }
  })
  
  observeEvent(input$lengthFilterColor, {
      updateColorBy(input$lengthFilterColor, session , "lengthFilterColor") 
    if(!input$lengthFilterColor){
      checkOtherColorBySelection(session, input, "lengthFilterColor")
    }
  })
  
  observeEvent(input$distanceFilterColor, {
      updateColorBy(input$distanceFilterColor, session , "distanceFilterColor")  
    if(!input$distanceFilterColor){
      checkOtherColorBySelection(session, input, "distanceFilterColor")
    }
  })
  
  observeEvent(input$lossFilterColor, {
    updateColorBy(input$lossFilterColor, session , "lossFilterColor")  
    if(!input$lossFilterColor){
      checkOtherColorBySelection(session, input, "lossFilterColor")
    }
  })
  
  observeEvent(input$injuriesFilterColor, {
    updateColorBy(input$injuriesFilterColor, session , "injuriesFilterColor")  
    if(!input$injuriesFilterColor){
      checkOtherColorBySelection(session, input, "injuriesFilterColor")
    }
  })
  
  observeEvent(input$fatalitiesFilterColor, {
    updateColorBy(input$fatalitiesFilterColor, session , "fatalitiesFilterColor")  
    if(!input$fatalitiesFilterColor){
      checkOtherColorBySelection(session, input, "fatalitiesFilterColor")
    }
  })
  
  observeEvent(input$magnitudeFilterWidth, {
    updateWidthBy(input$magnitudeFilterWidth, session , "magnitudeFilterWidth")  
  })
  observeEvent(input$distanceFilterWidth, {
    updateWidthBy(input$distanceFilterWidth, session , "distanceFilterWidth")  
  })
  observeEvent(input$lossFilterWidth, {
    updateWidthBy(input$lossFilterWidth, session , "lossFilterWidth")  
  })
  observeEvent(input$injuriesFilterWidth, {
    updateWidthBy(input$injuriesFilterWidth, session , "injuriesFilterWidth")  
  })
  observeEvent(input$fatalitiesFilterWidth, {
    updateWidthBy(input$fatalitiesFilterWidth, session , "fatalitiesFilterWidth")  
  })
  
  observe({
    if (ignoreNextMag) {
      magnitudesSelected <<- input$magGroup
      ignoreNextMag <<- !ignoreNextMag
    }
    else{
      allInteraction <-
        (allMag %in% magnitudesSelected && !(allMag %in% input$magGroup)) ||
        (!(allMag %in% magnitudesSelected) && allMag %in% input$magGroup)
      
      newMagnitudes <- c()
      if (allInteraction) {
        newMagnitudes <- if (allMag %in% input$magGroup) magnitudes else c()
        ignoreNextMag <<- TRUE
      }
      else {
        if (length(input$magGroup[input$magGroup != allMag]) == length(magnitudes) - 1) {
          if (!(allMag %in% input$magGroup))
          {
            newMagnitudes = c(input$magGroup, allMag)
            ignoreNextMag <<- TRUE
          }
          else
            newMagnitudes = input$magGroup
        }
        else{
          if (allMag %in% input$magGroup) {
            newMagnitudes = input$magGroup[input$magGroup != allMag]
            ignoreNextMag <<- TRUE
          }
          else
            newMagnitudes = input$magGroup
        }
      }
      
      updateCheckboxGroupInput(session, 'magGroup', choices = magnitudes, selected = newMagnitudes)
      magnitudesSelected <<- newMagnitudes
    }
  })
  
  output$sampleMap1 <-  renderLeaflet({
    leaflet() %>%
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
  output$samplePlot1<-renderPlot({getMagChart()})
}

shinyApp(ui = ui, server = server)