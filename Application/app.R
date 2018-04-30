library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(shiny)
library(shinyBS)
library(DT)
library(rgdal)
library(scales)
library(tidyr)
library(htmltools)

print(paste(Sys.time(), "Init"))
colorByArray <- c('magnitudeFilterColor', 'widthFilterColor', 'lengthFilterColor', 'lossFilterColor', 'distanceFilterColor', 'injuriesFilterColor', 'fatalitiesFilterColor')
widthByArray <- c('magnitudeFilterWidth', 'distanceFilterWidth', 'lossFilterWidth', 'injuriesFilterWidth', 'fatalitiesFilterWidth')

darkPalette <- c('#616161','#FFE0B2','#FFCC80','#FFA726','#FF9800','#FB8C00','#F57C00','#E65100')
lightPalette <- c('#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
highContrastPalette <- c('#ffd59b','#ffa474','#f47461','#db4551','#b81b34','#8b0000','#3f0000')
satellitePalette <- c('#FFE0B2','#FFCC80','#FFA726','#FB8C00','#F57C00','#E65100','#702800')

getPalette <- function(layer){
  # print(paste(Sys.time(), "getPalette", layer))
  if(is.null(layer)) return(darkPalette)
  
  palette <- switch(
    layer,
    "Dark" = darkPalette,
    "Light" = lightPalette,
    "Satellite" = satellitePalette,
    "Minimal" = lightPalette,
    "High Contrast" = highContrastPalette
  )
  return(palette)
}

months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
monthsDf <- data.frame(months, c(1:12))
colnames(monthsDf) <- c('MonthName', 'MonthNumber')

times <- c(paste(c(0:4), 'to', c(1:5), 'hr'), '5 hr or more')
timesDf <- data.frame(times, c(0:5))
colnames(timesDf) <- c('TimeName', 'TimeNumber')

print(paste(Sys.time(), "Loading countydata.csv"))
county_heatmap <- read.csv("data/countydata.csv", fileEncoding = "UTF-8-BOM")

us_counties <- NULL
counties_popup <- NULL
getUsCounties <- function() {
  if(is.null(us_counties)) {
    print(paste(Sys.time(), "Loading uscounties.geojson"))
    us_counties <- rgdal::readOGR("data/uscounties.geojson")
    us_counties <- merge(us_counties, county_heatmap, by.x="GEO_ID", by.y = "geoId")
    us_counties[is.na(us_counties$inj), "inj"] <- 0

    counties_popup <<- paste0("<strong>County: </strong>", us_counties$NAME, 
                         "<br><strong>Injuries: </strong>", us_counties$inj)
  }
  return(us_counties)
}

stateGeoJsons <- list()
getStateGeoJson <- function(stf) {
  key <- as.character(stf)
  if(is.null(stateGeoJsons[[key]])) {
    stateGeoJsons[[key]] <<- rgdal::readOGR(paste0("data/GeoJson/", key, ".geojson"))
  }
  return(stateGeoJsons[[key]])
}

us_states <- NULL
states_popup <- NULL
getUsStates <- function() {
  if(is.null(us_states)) {
    print(paste(Sys.time(), "Loading usstates.geojson"))
    us_states <- rgdal::readOGR("data/usstates.geojson")

    states_popup <<- paste0("<strong>State: </strong>", us_states$NAME)
  }
  return(us_states)
}

print(paste(Sys.time(), "Initializing Functions"))

pickOptions <- c("Pick a State", "Pick a County", "Show HeatMap")
pick <- c()

#helper function to get ctf and stf from map input
getStfCtfFromMap <- function(id, state = FALSE)
{
  print(paste(Sys.time(), "getStfCtfFromMap", id))
  string <- sub(".*US", "", id)
  code <- list(stf = -1, ctf = -1)
  
  code$stf <- as.numeric(substr(string, 0, 2))
  
  if(!state)
  {
    code$ctf<-as.numeric(substr(string, 3, 5))
  }
  return (code)
}

# function to create foundational map
foundational.map <- function() {
  print(paste(Sys.time(), "Loading Foundational Map"))
  
  y <- leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
    addProviderTiles(providers$CartoDB.Positron, group = 'Light') %>%
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Minimal') %>%
    addProviderTiles(providers$Stamen.TonerLite, group = 'High Contrast') %>%
    setView(lat = 41.881832, lng = -87.623177, zoom = 4)
  
  usstates <- getUsStates()
  y <- y %>%
    addPolygons(
      data = usstates, 
      fillOpacity = 0, 
      opacity = 0.2, 
      color = "#757575", 
      weight = 0.5, 
      layerId = usstates$GEO_ID, 
      group = "Pick a State", 
      popup = states_popup
    )

  uscounties <- getUsCounties()
  y <- y %>%
    addPolygons(
      data = uscounties,
      fillOpacity = 0,
      opacity = 0.2,
      color = "#757575",
      weight = 0.5,
      layerId = uscounties$GEO_ID,
      group = "Pick a County",
      popup = counties_popup
    )

  y <- y %>%
    addPolygons(
      data = uscounties,
      fillOpacity = rescale(uscounties[["heat"]], c(0, 1)),
      opacity = rescale(uscounties[["heat"]], c(0, 1)),
      color = "#FF0000",
      weight = 2,
      layerId = paste0(uscounties$GEO_ID, "_HeatMap"),
      group = "Show HeatMap",
      popup = counties_popup
    )

  y <- y %>% 
    addLayersControl(
      baseGroups = c("Dark", "Light", "Satellite", "Minimal", "High Contrast"), 
      overlayGroups = pickOptions, 
      options = layersControlOptions(collapsed = TRUE)
    ) %>% hideGroup(pickOptions)
  
  return(y)
}

hours <- function(is24Hour) {
  return(if (is24Hour) paste(c(0:23), 'hr') else c('Midnight', paste(c(1:11), 'am'), 'Noon', paste(c(1:11), 'pm')))
}
hoursDf <- function(is24Hour) {
  hdf <- data.frame(hours(is24Hour), c(0:23))
  colnames(hdf) <- c('HourName', 'HourNumber')
  return(hdf)
}

distanceGroups <- function(isMetric) {
  values <- (if (isMetric)
    c(paste('~', c(1:70) * 100, 'km'))
    else
      c(paste('~', c(1:70) * 60, 'mi')))
  
  values[1] <- paste('0', values[1])
  return(values)
}
distanceGroupDf <- function(isMetric) {
  dgdf <- data.frame(distanceGroups(isMetric), c(0:69))
  colnames(dgdf) <- c('DistanceName', 'DistanceNumber')
  return(dgdf)
}

getStates <- function() {
  print(paste(Sys.time(), "Loading statenames"))
  data <- read.csv("data/statenames.csv", fileEncoding = "UTF-8-BOM")
  states <- setNames(as.list(data$stf), data$name)
  return(states)
}

states <- getStates()

print(paste(Sys.time(), "Loading alldata"))
data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")
data[is.na(data$dollarloss), "dollarloss"] <- 0

print(paste(Sys.time(), "Loading fipscodes"))
counties <- read.csv("data/fipscodes.csv", fileEncoding = "UTF-8-BOM")

print(paste(Sys.time(), "Initializing functions"))

allMag <- c("All")
magnitudes <- c(levels(data$mag), allMag)
magnitudesSelected <- magnitudes
ignoreNextMag <- FALSE

getCounties <- function(stf) {
  cts <- counties[counties$stf == stf, ]
  choices <- setNames(as.list(c(0, cts$ctf)), c("All", as.character(cts$name)))
  return(choices)
}

JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 10;
      var powStop = 2;
      for (i = powStop; i <= powStart; i++) {
        var val = Math.pow(10, i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#lossSlider').data('ionRangeSlider').update({'values':vals})
    }, 5)})"

getLossValueFromSlider <- function(sliderValue) {
  if(sliderValue == 0) return(0)
  
  return(10 ^ (sliderValue + 1))
}

#creates button group for filtering based on color/width
createButtonGroup <- function(filter_id) {
  if(filter_id == 'magnitudeFilter') { value = TRUE }
  else { value = FALSE }
  div( 
    class = "buttonGroup", 
    prettyToggle(
      inputId = paste(filter_id, "Width", sep=""), 
      label_on = "", label_off="", 
      # animation = 'pulse', 
      icon_on = icon("bars", lib = "font-awesome"), 
      icon_off = icon("bars", lib = "font-awesome"), 
      status_on = "primary", status_off = "default", 
      shape = "square", outline = TRUE
    ), 
    bsTooltip(paste(filter_id, "Width", sep=""), title = "Apply_Width", placement = "bottom", trigger = "hover", options = NULL), 
    prettyToggle(
      inputId = paste(filter_id, "Color", sep=""), 
      label_on = "", label_off="", 
      icon_on = icon("tint"), 
      icon_off = icon("tint"), 
      status_on = "primary", status_off = "default", 
      shape = "round", outline = TRUE, value = value
    ), 
    bsTooltip(paste(filter_id, "Color", sep=""), title = "Apply_Color", placement = "bottom", trigger = "hover", options = NULL)
  )
}

createColorButtonGroup <- function(filter_id) {
  div(class = "buttonGroup", 
      prettyToggle(
        inputId = paste(filter_id, "Color", sep=""), 
        label_on = "", label_off="", 
        icon_on = icon("tint"), 
        icon_off = icon("tint"), 
        status_on = "primary", status_off = "default", 
        shape = "round", outline = TRUE
      ), 
      bsTooltip(paste(filter_id, "Color", sep=""), title = "Apply_Color", placement = "bottom", trigger = "hover", options = NULL)
      )
}

getMagnitudeChart<- function(df, chartBy) {
  print(df$DistanceName)
  x<-switch (chartBy, 
             'Year' = 'yr', 
             'Month' = 'mo', 
             'Hour' = 'HourName', 
             'Distance from Chicago' = 'DistanceName', 
             'County' = 'name'
  )
  
  # yrcount<-subset(data, st=='IL') %>% count(yr, mag) %>% group_by(yr) %>% mutate(percent = n/sum(n))
  
  updatemenus <-list(
    list(
      buttons = list(
        list(method = "restyle", 
             args = list("y", list(df$count)), # put it in a list
             label = "Show Count"), 
        list(method = "restyle", 
             args = list("y", list(df$percent*100)), # put it in a list
             label = "Show Percent"), 
        list(method = "restyle", 
             args = list("y", list(df$fat)), # put it in a list
             label = "Show Fatalities"), 
        list(method = "restyle", 
             args = list("y", list(df$inj)), # put it in a list
             label = "Show Injuries"), 
        list(method = "restyle", 
             args = list("y", list(df$dl)), # put it in a list
             label = "Show Loss")
        ))
  )
  
  return(plot_ly(df, type = 'bar', x = df[[x]], y = ~count, marker = list(color = ~as.numeric(df$mag), showscale = TRUE, colorbar=list(tickmode='array', tickvals=as.numeric(sort(unique(df$mag))), ticktext=sort(unique(df$mag))))) %>%
           layout(yaxis = list(title = 'value'), barmode='stack', updatemenus = updatemenus))
}

getParCoordChart<-function(df, chartBy) {
  switch (chartBy, 
             'Year' = {
               x<-'yr'
               vals<-as.numeric(unique(df$yr))
               text<-unique(df$yr)
              }, 
             'Month' = {
               x<-'mo'
               vals<-monthsDf$MonthNumber
               text<-monthsDf$MonthName
              }, 
             'Hour' = {
               x<-"hr"
               vals<-as.numeric(unique(df$hr))
               text<-unique(df$HourName)
              }, 
             'Distance from Chicago' = {
               x<-'chidistgrp'
               vals<-as.numeric(unique(df$chidistgrp))
               text<-unique(df$DistanceName)
              }, 
             'County' = {
               x<-'srno'
               vals<-unique(df$srno)
               text<-as.character(unique(df$name))
             }
  )
  
  return(
    df %>% plot_ly(
      type = 'parcoords', 
      line = list(
        color = ~as.numeric(mag),
        # colorscale='[[0, "rgb(50,50,255)", [1, "rgb(50,50,50)"]]',
        colorscale = 'YlGnBu',
        showscale = T, 
        colorbar = list(
          tickmode = 'array', 
          tickvals = as.numeric(sort(unique(df$mag))), 
          ticktext = sort(unique(df$mag))
        )
      ), 
      dimensions = list(
          list(label=chartBy, values=df[[x]], tickvals=vals, ticktext=text),
          list(label='Magnitude', values=~as.numeric(mag), tickvals=as.numeric(sort(unique(df$mag))), ticktext=sort(unique(df$mag))), 
          list(label='Injuries', values=~inj), 
          list(label='Fatalities', values=~fat), 
          list(label='Loss ($)', values=~dl)
  )))
}

getChartData <- function(data, x, isMetric, is24Hour, state){
  if(x =='fips'){
    datasepcounties<-separate_rows(data,fips, sep = '::')
    datasepcounties$fips<-gsub(':','', datasepcounties$fips)
    topcounties<-head(count(datasepcounties, fips, sort = T), 10)
    topcounties$srno <- 1:nrow(topcounties)
    datasepcounties<-semi_join(datasepcounties, topcounties, by='fips')
    data<-datasepcounties
  }
  
  result <- data %>% group_by_at(c(x, 'mag')) %>% summarise(
    fat = sum(fat), 
    inj = sum(inj), 
    dl = sum(dollarloss, na.rm = TRUE), 
    count = n()
  ) %>% group_by_at(x) %>% mutate(percent = count / sum(count))
  
  switch (x,
          'hr' = {
            result<-merge(
              result,
              hoursDf(is24Hour),
              by.x = 'hr',
              by.y = 'HourNumber',
              all = TRUE
            )
          },
          'chidistgrp' = {
            result<-merge(
              result,
              distanceGroupDf(isMetric),
              by.x = 'chidistgrp',
              by.y = 'DistanceNumber',
              all.x = TRUE
            )
          },
          'fips' = {
            result<-merge(
              result,
              subset(counties, stf==state),
              by.x = 'fips',
              by.y = 'ctf',
              all.x = TRUE
            )
            result<-merge(
              result,
              dplyr::select(topcounties, fips, srno),
              by = 'fips',
              all.x = TRUE
            )
            
          }
  )
  
  return(result)
}

getTable<-function(df, chartBy) {
  switch (chartBy,
    'Month' =  {
      df<-merge(df, monthsDf, by.x='mo', by.y='MonthNumber', all.x=T)
      df<-df[c(8,2,3,4,5,6,7)]
    },
    'Hour' = {
      df<-df[c(8,2,3,4,5,6,7)]
    },
    'Distance from Chicago' = {
      df<-df[c(8,2,3,4,5,6,7)]
    },
    'County' = {
      df<-df[c(11,2,3,4,5,6,7)]
    }
  )
  colnames(df)<-c(chartBy, 'Magnitude', 'Fatalities', 'Injuries', 'Loss', 'Count', 'Percent(per year)')
  return(
    datatable(df, rownames= FALSE, options = list(scrollY = '100%'))%>%
      formatPercentage('Percent(per year)', 2) %>% 
      formatCurrency('Loss', '$')
  )
}

print(paste(Sys.time(), "Initializing UI"))
ui <- fluidPage(tags$head(tags$script(HTML(JScode))), 
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
        class = "filter-group states", 
        textOutput("state1Count", inline = TRUE), 
        selectInput("state1Select", "State 1", states, "17"), 
        textOutput("county1Count", inline = TRUE), 
        selectInput("county1Select", "County 1", c("All"), "All")
      ), 
      
      div(class = "spacer"), 
      div(
        class = "filter-group states", 
        textOutput("state2Count", inline = TRUE), 
        selectInput("state2Select", "State 2", states, "6"), 
        textOutput("county2Count", inline = TRUE), 
        selectInput("county2Select", "County 2", c("All"), "All")
      ), 
      
      div(
        class = "filter-group", 
        selectInput("chartBySelect", "Chart By:", 
          choices = list("Year", "Month", "Hour", "Distance from Chicago", "County")
        ), 
        div(
          id = "yearDiv", 
          class = "filterContainer", 
          sliderInput("yearSlider", "Year", min = 1950, max = 2017, value = c(2000, 2017), step = 5, 
            sep = "", width = "100%", animate = animationOptions(interval = 4000, loop = FALSE))
        )
      ), 
      
      div(class = "spacer"), 
      div(
        class = "filter-group", 
        createButtonGroup('magnitudeFilter'), 
        div(
          id = "magnitudeDiv", 
          class = "filterContainer", 
          checkboxGroupInput("magGroup", label = "Magnitude (F-scale)", choices = magnitudes, selected = magnitudes)
        )
      ), 
      
      div(
        class = "filter-group", 
        createColorButtonGroup("widthFilter"), 
        div(
          id = "widthDiv", 
          class = "filterContainer", 
          sliderInput("widthSlider", "Width", min = 0, max = 5000, value = c(0, 5000))
        ), 
        
        createColorButtonGroup("lengthFilter"), 
        div(
          class = "filterContainer", 
          sliderInput("lengthSlider", "Length", min = 0, max = 250, value = c(0, 250))
        )
      ), 
      
      div(
        class = "filter-group", 
        createButtonGroup('distanceFilter'), 
        div(
          class = "filterContainer", 
          sliderInput("distanceSlider", "Distance from Chicago", min = 0, max = 4500, value = c(0, 4500))
        ), 

        createButtonGroup('lossFilter'), 
        div(
          class = "filterContainer", 
          sliderInput("lossSlider", "Loss (USD)", min = 0, max = 3000000000, value = c(0, 3000000000)))
      ), 
      
      div(
        class = "filter-group", 
        createButtonGroup('injuriesFilter'), 
        div(
          class = "filterContainer", 
          sliderInput("injuriesSlider", "Injuries", min = 0, max = 1750, value = c(0, 1750))
        ), 
        
        createButtonGroup('fatalitiesFilter'), 
        div(
          class = "filterContainer", 
          sliderInput("fatalitiesSlider", "Fatalities", min = 0, max = 160, value = c(0, 160)))
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
        height = "100%", 
        tabsetPanel(
          type = 'tabs', 
          id='chartbox1', 
          tabPanel(
            'Injuries, Fatalities and Loss', 
            plotlyOutput('parcoordchart1')
          ), 
          tabPanel(
            'Number of Tornadoes', 
            plotlyOutput('countpercent1')
          ), 
          tabPanel(
            'Table', 
            dataTableOutput('table1')
          )
        )
    ), 
    div(id = "plotTwo", 
        tabsetPanel(
          type = 'tabs', 
          id='chartbox2', 
          tabPanel(
            'Injuries, Fatalities and Loss', 
            plotlyOutput('parcoordchart2')
          ), 
          tabPanel(
            'Number of Tornadoes', 
            plotlyOutput('countpercent2')
          ), 
          tabPanel(
            'Table', 
            dataTableOutput('table2')
          )
        )
    )
  )
)

server <- function(input, output, session) {
  colorby <- reactiveVal("inj")
  widthby <- reactiveVal("fat")
  
  updateColorBy <-
    function(isSelected, session, input, idOfSelectedColorBy, field) {
      if (isSelected) {
        colorby(field)
        for (i in colorByArray) {
          updatePrettyToggle(session = session, inputId = i, value = i == idOfSelectedColorBy)
        }
      }
      else{
        anythingSelected <- FALSE
        for (i in colorByArray) {
          if (input[[i]]) 
            anythingSelected <- TRUE
        }
        
        if (anythingSelected == FALSE)
          updatePrettyToggle(session = session, inputId = idOfSelectedColorBy, value = TRUE)
      }
    }
  
  updateWidthBy <-
    function(isSelected, session, input, idOfSelectedWidthBy, field) {
      if (isSelected) {
        widthby(field)
        for (i in widthByArray) {
          updatePrettyToggle(session = session, inputId = i, value = i == idOfSelectedWidthBy)
        }
      }
      else{
        anythingSelected <- FALSE
        for (i in widthByArray) {
          if (input[[i]]) 
            anythingSelected <- TRUE
        }
        
        if (anythingSelected == FALSE)
          updatePrettyToggle(session = session, inputId = idOfSelectedWidthBy, value = TRUE)
      }
    }
  
  county1Data <- reactiveVal(data.frame())
  county2Data <- reactiveVal(data.frame())

  chart1Data <- reactiveVal(data.frame())
  chart2Data <- reactiveVal(data.frame())

  myMap_reval <- reactiveVal(foundational.map())
  group <- list(id=vector())

  prev1 <- reactiveValues(id = vector())
  prev2 <- reactiveValues(id = vector())

  output$sampleMap1 <- renderLeaflet({ map <- myMap_reval() })
  output$sampleMap2 <- renderLeaflet({ map <- myMap_reval() })

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
  
  observeEvent(input$measurementRadio, {
    label <- if(input$measurementRadio == "Imperial") "yards" else "meters"
    maxValue <- max(if(input$measurementRadio == "Imperial") data$wid else data$widm)
    updateSliderInput(session, "widthSlider", label = paste0("Width (", label, ")"), max = maxValue, value = c(0, maxValue))
    
    label <- if(input$measurementRadio == "Imperial") "miles" else "kilometers"
    maxValue <- max(if(input$measurementRadio == "Imperial") data$len else data$lenkm)
    updateSliderInput(session, "lengthSlider", label = paste0("Length (", label, ")"), max = maxValue, value = c(0, maxValue))
    
    maxValue <- max(if(input$measurementRadio == "Imperial") data$chidist else data$chidistkm)
    updateSliderInput(session, "distanceSlider", label = paste0("Dist. to Chicao (", label, ")"), max = maxValue, value = c(0, maxValue))
  })
  
  observeEvent(input$state1Select, {
    choices <- getCounties(input$state1Select)
    updateSelectInput(session, "county1Select", "County 1", choices, 0)
  })
  
  observeEvent(input$state2Select, {
    choices <- getCounties(input$state2Select)
    updateSelectInput(session, "county2Select", "County 2", choices, 0)
  })
  
  observeEvent(input$magnitudeFilterColor, {
    updateColorBy(input$magnitudeFilterColor, session, input, "magnitudeFilterColor", "mag") 
  })
  observeEvent(input$widthFilterColor, {
    updateColorBy(input$widthFilterColor, session, input, "widthFilterColor", if(input$measurementRadio == "Imperial") "wid" else "widm") 
  })
  observeEvent(input$lengthFilterColor, {
    updateColorBy(input$lengthFilterColor, session, input, "lengthFilterColor", if(input$measurementRadio == "Imperial") "len" else "lenkm") 
  })
  observeEvent(input$distanceFilterColor, {
    updateColorBy(input$distanceFilterColor, session, input, "distanceFilterColor", if(input$measurementRadio == "Imperial") "chidist" else "chidistkm") 
  })
  observeEvent(input$lossFilterColor, {
    updateColorBy(input$lossFilterColor, session, input, "lossFilterColor", "dollarloss") 
  })
  observeEvent(input$injuriesFilterColor, {
    updateColorBy(input$injuriesFilterColor, session, input, "injuriesFilterColor", "inj") 
  })
  observeEvent(input$fatalitiesFilterColor, {
    updateColorBy(input$fatalitiesFilterColor, session, input, "fatalitiesFilterColor", "fat") 
  })
  
  observeEvent(input$magnitudeFilterWidth, {
    updateWidthBy(input$magnitudeFilterWidth, session, input, "magnitudeFilterWidth", "mag") 
  })
  observeEvent(input$distanceFilterWidth, {
    updateWidthBy(input$distanceFilterWidth, session, input, "distanceFilterWidth", if(input$measurementRadio == "Imperial") "chidist" else "chidistkm")
  })
  observeEvent(input$lossFilterWidth, {
    updateWidthBy(input$lossFilterWidth, session, input, "lossFilterWidth", "dollarloss") 
  })
  observeEvent(input$injuriesFilterWidth, {
    updateWidthBy(input$injuriesFilterWidth, session, input, "injuriesFilterWidth", "inj") 
  })
  observeEvent(input$fatalitiesFilterWidth, {
    updateWidthBy(input$fatalitiesFilterWidth, session, input, "fatalitiesFilterWidth", "fat") 
  })
  
  observe({ # This should not be observeEvent
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
        print("All Interaction")
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
  
  observe({
    plotData <- subset(data, mag %in% input$magGroup)

    plotData <- subset(plotData, yr >= input$yearSlider[1])
    plotData <- subset(plotData, yr <= input$yearSlider[2])
    
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") wid else widm) >= input$widthSlider[1])
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") wid else widm) <= input$widthSlider[2])
    
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") len else lenkm) >= input$lengthSlider[1])
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") len else lenkm) <= input$lengthSlider[2])
    
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") chidist else chidistkm) >= input$distanceSlider[1])
    plotData <- subset(plotData, (if(input$measurementRadio == "Imperial") chidist else chidistkm) <= input$distanceSlider[2])
    
    plotData <- subset(plotData, fat >= input$fatalitiesSlider[1])
    plotData <- subset(plotData, fat <= input$fatalitiesSlider[2])
    
    plotData <- subset(plotData, inj >= input$injuriesSlider[1])
    plotData <- subset(plotData, inj <= input$injuriesSlider[2])
    
    plotData <- subset(plotData, dollarloss >= getLossValueFromSlider(input$lossSlider[1]))
    plotData <- subset(plotData, dollarloss <= getLossValueFromSlider(input$lossSlider[2]))
    
    state1Data <- subset(plotData, stf == input$state1Select)
    output$state1Count <- renderText(paste(nrow(state1Data), "records"))
    newCountyData <- if (input$county1Select == 0) state1Data else
      subset(state1Data, grepl(paste0(":", input$county1Select, ":"), fips, fixed = TRUE))
    
    if(!identical(county1Data(), newCountyData)) {
      print(paste(Sys.time(), "Updating County 1 Data"))
      county1Data(newCountyData)
      output$county1Count <- renderText(paste(nrow(county1Data()), "records"))
    }
    chart1Data(getChartData(county1Data(), switch(
      input$chartBySelect, 
      "Year" = "yr", 
      "Month" = "mo", 
      "Hour" = "hr", 
      "Distance from Chicago" = 'chidistgrp', 
      "County" = 'fips'
    ), input$measurementRadio == "Metric", input$hourRadio == "24 hr", input$state1Select))
    
    state2Data <- subset(plotData, stf == input$state2Select)
    output$state2Count <- renderText(paste(nrow(state2Data), "records"))
    newCountyData <- if (input$county2Select == 0) state2Data else
      subset(state2Data, grepl(paste0(":", input$county2Select, ":"), fips, fixed = TRUE))
    
    if(!identical(county2Data(), newCountyData)) {
      print(paste(Sys.time(), "Updating County 2 Data"))
      county2Data(newCountyData)
      output$county2Count <- renderText(paste(nrow(county2Data()), "records"))
    }
    
    chart2Data(getChartData(county2Data(), switch(
      input$chartBySelect, 
      "Year" = "yr", 
      "Month" = "mo", 
      "Hour" = "hr", 
      "Distance from Chicago" = 'chidistgrp',
      "County" = 'fips'
    ), input$measurementRadio == "Metric", input$hourRadio == "24 hr", input$state2Select))
    
    output$parcoordchart1<-renderPlotly({
      getParCoordChart(chart1Data(), input$chartBySelect)
    })
    output$countpercent1<-renderPlotly({
      getMagnitudeChart(chart1Data(), input$chartBySelect)
    })
    
    output$parcoordchart2<-renderPlotly({
      getParCoordChart(chart2Data(), input$chartBySelect)
    })
    output$countpercent2<-renderPlotly({
      getMagnitudeChart(chart2Data(), input$chartBySelect)
    })
    output$table1 = DT::renderDataTable({
      getTable(chart1Data(), input$chartBySelect)
    })
    output$table2 = DT::renderDataTable({
      getTable(chart2Data(), input$chartBySelect)
    })
  })
  
  observeEvent(input$sampleMap1_groups, ignoreNULL = T, ignoreInit = T, {    
    click <- input$sampleMap1_groups
    picked <- pickOptions %in% click
    print(picked)
    if(sum(picked) == 1) {
      pick <<- click[click %in% pickOptions]
    }
    else if(sum(picked) > 1) {
      toRemove <- pick
      print (paste("To Remove", toRemove))
      pick <<- click[click %in% pickOptions && !click %in% pick]
      print (paste("New Pick", pick))
      leafletProxy("sampleMap1", session) %>% hideGroup(toRemove)
    }
  })
  
  observe({
    print(paste(Sys.time(), "Observing 1", colorby(), widthby()))
    
    if(nrow(county1Data()) > 0) {
      lines <- getStateGeoJson(input$state1Select)
      mergedData <- merge(lines, county1Data(), by.x = "tornadoId",  by.y = "id", all.x = FALSE)
      numerics <- as.numeric(county1Data()[[colorby()]])
      uniques <- unique(numerics)
      domain <- unique(c(uniques, unique(as.numeric(county2Data()[[colorby()]]))))
      palette <- getPalette(input$sampleMap1_groups[1])
      quantiles <- colorQuantile(palette, domain, n = min(7, length(domain)))
      
      minlat <- min(c(county1Data()$slat, county1Data()$elat))
      minlon <- min(c(county1Data()$slon, county1Data()$elon))
      maxlat <- max(c(county1Data()$slat, county1Data()$elat))
      maxlon <- max(c(county1Data()$slon, county1Data()$elon))
      
      combinedData <- c(county1Data()[[widthby()]], county2Data()[[widthby()]])
      widthRange <- c(min(combinedData), max(combinedData))
      
      labels <- paste(mergedData$yr, "-", mergedData$mo, "-", mergedData$dy, 
                      "<br><b>Magnitude: </b>", mergedData$mag,
                      "<br><b>Injuries: </b>", mergedData$inj,
                      "<br><b>Fatalities: </b>", mergedData$fat,
                      "<br><b>Loss: </b> USD", format(mergedData$dollarloss, big.mark = ",")) %>% 
        lapply(htmltools::HTML)
      
      if (length(prev1$id) != 0)
        leafletProxy("sampleMap1", session) %>% removeShape(layerId = prev1$id)
      
      leafletProxy("sampleMap1", session) %>% addPolylines(
        data = mergedData,
        layerId = mergedData$tornadoId,
        color = if(length(domain) == 1) palette[length(palette)] else quantiles(numerics),
        weight = rescale(as.numeric(county1Data()[[widthby()]]), to = c(1, 5), from = widthRange),
        opacity = 0.7,
        label = labels
      ) %>% flyToBounds(minlon, minlat, maxlon, maxlat)
      
      prev1$id <- mergedData$tornadoId
    }
    else if (length(prev1$id) != 0) {
      leafletProxy("sampleMap1", session) %>% removeShape(layerId = prev1$id)
      prev1$id <- vector()
    }
  })
  
  observe({
    print(paste(Sys.time(), "Observing 2", colorby(), widthby()))
    
    if(nrow(county2Data()) > 0) {
      lines <- getStateGeoJson(input$state2Select)
      mergedData <- merge(lines, county2Data(), by.x = "tornadoId",  by.y = "id", all.x = FALSE)
      numerics <- as.numeric(county2Data()[[colorby()]])
      uniques <- unique(numerics)
      domain <- unique(c(uniques, unique(as.numeric(county1Data()[[colorby()]]))))
      palette <- getPalette(input$sampleMap2_groups[1])
      quantiles <- colorQuantile(palette, domain, n = min(7, length(domain)))
      
      minlat <- min(c(county2Data()$slat, county2Data()$elat))
      minlon <- min(c(county2Data()$slon, county2Data()$elon))
      maxlat <- max(c(county2Data()$slat, county2Data()$elat))
      maxlon <- max(c(county2Data()$slon, county2Data()$elon))
      
      combinedData <- c(county1Data()[[widthby()]], county2Data()[[widthby()]])
      widthRange <- c(min(combinedData), max(combinedData))
      
      labels <- paste(mergedData$yr, "-", mergedData$mo, "-", mergedData$dy, 
                      "<br><b>Magnitude: </b>", mergedData$mag,
                      "<br><b>Injuries: </b>", mergedData$inj,
                      "<br><b>Fatalities: </b>", mergedData$fat,
                      "<br><b>Loss: </b> USD", format(mergedData$dollarloss, big.mark = ",")) %>% 
        lapply(htmltools::HTML)
      
      if (length(prev2$id) != 0)
        leafletProxy("sampleMap2", session) %>% removeShape(layerId = prev2$id)
      
      leafletProxy("sampleMap2", session) %>% addPolylines(
        data = mergedData,
        layerId = mergedData$tornadoId,
        color = if(length(domain) == 1) palette[length(palette)] else quantiles(numerics),
        weight = rescale(as.numeric(county2Data()[[widthby()]]), to = c(1, 5), from = widthRange),
        opacity = 0.7,
        label = labels
      ) %>% flyToBounds(minlon, minlat, maxlon, maxlat)
      
      prev2$id <- mergedData$tornadoId
    }
    else if (length(prev2$id) != 0) {
      leafletProxy("sampleMap1", session) %>% removeShape(layerId = prev2$id)
      prev2$id <- vector()
    }
  })
  
  observeEvent(input$sampleMap1_shape_click, ignoreNULL = T, ignoreInit = T, {
    click1 <- input$sampleMap1_shape_click
    stf <- getStfCtfFromMap(click1$id, TRUE)$stf
    ctf <- getStfCtfFromMap(click1$id, ! "Pick a County" %in% input$sampleMap1_groups)$ctf
    
    updateSelectInput(session, "state1Select", selected = stf)
    updateSelectInput(session, "county1Select", selected = if (ctf == -1) 0 else ctf)
  })

  observeEvent(input$sampleMap2_shape_click, ignoreNULL = T, ignoreInit = T, {
    click2 <- input$sampleMap2_shape_click
    stf <- getStfCtfFromMap(click2$id, TRUE)$stf
    ctf <- getStfCtfFromMap(click2$id, ! "Pick a County" %in% input$sampleMap2_groups)$ctf
    
    updateSelectInput(session, "state2Select", selected = stf)
    updateSelectInput(session, "county2Select", selected = if (ctf == -1) 0 else ctf)
  })
}

shinyApp(ui = ui, server = server)