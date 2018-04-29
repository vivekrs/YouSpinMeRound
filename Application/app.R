library(shiny)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(shiny)
library(shinyBS)
library(DT)

colorByArray <- c('magnitudeFilterColor', 'widthFilterColor', 'lengthFilterColor', 'lossFilterColor', 'distanceFilterColor', 'injuriesFilterColor', 'fatalitiesFilterColor')
widthByArray <- c('magnitudeFilterWidth', 'distanceFilterWidth', 'lossFilterWidth', 'injuriesFilterWidth', 'fatalitiesFilterWidth')

months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
monthsDf <- data.frame(months, c(1:12))
colnames(monthsDf) <- c('MonthName', 'MonthNumber')

times <- c(paste(c(0:4), 'to', c(1:5), 'hr'), '5 hr or more')
timesDf <- data.frame(times, c(0:5))
colnames(timesDf) <- c('TimeName', 'TimeNumber')

county_heatmap <- read.csv("data/countydata.csv", fileEncoding = "UTF-8-BOM")
uscounties <- rgdal::readOGR("data/uscounties.geojson")
uscounties <- merge(uscounties, county_heatmap, by.x="GEO_ID", by.y = "geoId")
uscounties[is.na(uscounties$inj), "inj"] <- 0
counties_popup <- paste0("<strong>County: </strong>", uscounties$NAME, 
                         "<strong>Injuries: </strong>", uscounties$inj)

usstates <- rgdal::readOGR("data/usstates.geojson")
state_popup <- paste0("<strong>State: </strong>", usstates$NAME)

state <- TRUE
heatmap <- FALSE
heatmapby <- "inj"

#helper function to get ctf and stf from map input
code_get <- function(id, state = FALSE)
{
  string <- sub(".*US", "", id)
  code <- list(stf= -1, ctf = -1)
  
  code$stf <- as.numeric(substr(string, 0, 2))
  
  if(!state)
  {
    code$ctf<-as.numeric(substr(string, 3, 5))
  }
  return (code)
}

draw_tracks <- function(map, df, heatmap, group )
{
  if(!heatmap & df$stf!=-1) { 
    #lines <- rgdal::readOGR(paste0("data/GeoJson/", df$stf, ".geojson"))
    map <- map %>% addPolygons( data = lines )
  }
  return (map)
}

# function to create foundational map

foundational.map <- function(state, heatmap, heatmapby) {
  y <- leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
    addProviderTiles(providers$CartoDB.Positron, group = 'Light') %>%
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = 'Minimal') %>%
    addProviderTiles(providers$Stamen.TonerLite, group = 'Colorblind Safe') %>%
    addLayersControl(
      baseGroups = c("Dark", "Light", "Satellite", "Minimal", "Colorblind Safe"), 
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    setView(lat = 41.881832, lng = -87.623177, zoom = 4)

  if (state & !heatmap) {
    y <- y %>%
      addPolygons(
        data = usstates, 
        fillOpacity = 0, 
        opacity = 0.2, 
        color = "#000000", 
        weight = 2, 
        layerId = usstates$GEO_ID, 
        group = "click.list", 
        popup = state_popup
      )
  }
  else if (!state & !heatmap) {
    y <- y %>%
      addPolygons(
        data = uscounties, 
        fillOpacity = 0, 
        opacity = 0.2, 
        color = "#000000", 
        weight = 2, 
        layerId = uscounties$GEO_ID, 
        group = "click.list", 
        popup = counties_popup
      )
  }
  else if (heatmap) {
    y <- y %>%
      addPolygons(
        data = uscounties, 
        fillOpacity = rescale(uscounties[[heatmapby]], c(0, 1)), 
        opacity = rescale(uscounties[[heatmapby]], c(0, 1)), 
        color = "#FF0000", 
        weight = 2, 
        layerId = uscounties$GEO_ID, 
        group = "heatmap", 
        popup = counties_popup
      )
  }
  
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
  dgdf <- data.frame(distanceGroups(isMetric), c(0:8))
  colnames(dgdf) <- c('DistanceName', 'DistanceNumber')
  return(dgdf)
}

getStates <- function() {
  data <- read.csv("data/statenames.csv", fileEncoding = "UTF-8-BOM")
  states <- setNames(as.list(data$Code), data$Name)
  return(states)
}
states <- getStates()

data <- read.csv("data/alldata.csv", fileEncoding = "UTF-8-BOM")
data[is.na(data$dollarloss), "dollarloss"] <- 0

chart1Data <- data
chart2Data <- data

allMag <- c("All")
magnitudes <- c(levels(data$mag), allMag)
magnitudesSelected <- c()
ignoreNextMag <- FALSE

counties <- read.csv("data/fipscodes.csv", fileEncoding = "UTF-8-BOM")

getCounties <- function(st) {
  cts <- counties[counties$st == st, ]
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
  x<-switch (chartBy, 
             'Year' = 'yr', 
             'month' = 'mo', 
             'hour' = 'hr', 
             'dist' = 'chidist', 
             'county' = 'county'
  )
  
  # yrcount<-subset(data, st=='IL') %>% count(yr, mag) %>% group_by(yr) %>% mutate(percent = n/sum(n))
  
  updatemenus <-list(
    list(
      buttons = list(
        list(method = "restyle", 
             args = list("y", list(df$count)), # put it in a list
             label = "Show Count"), 
        list(method = "restyle", 
             args = list("y", list(df$percent)), # put it in a list
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
  
  return(plot_ly(df, type = 'bar', x = ~get(x), y = ~count, marker = list(color = ~as.numeric(df$mag), showscale = TRUE, colorbar=list(tickmode='array', tickvals=as.numeric(sort(unique(df$mag))), ticktext=sort(unique(df$mag))))) %>%
           layout(yaxis = list(title = 'value'), barmode='stack', updatemenus = updatemenus))
}

getParCoordChart<-function(df, chartBy) {
  x<-switch (chartBy, 
             'Year' = 'yr', 
             'month' = 'mo', 
             'hour' = 'hr', 
             'dist' = 'chidist', 
             'county' = 'county'
  )
  
  return(
    df %>% plot_ly(
      type = 'parcoords', 
      line = list(
        color = ~ as.numeric(mag), 
        showscale = T, 
        colorbar = list(
          tickmode = 'array', 
          tickvals = as.numeric(sort(unique(df$mag))), 
          ticktext = sort(unique(df$mag))
        )
      ), 
      dimensions = list(
          list(label=chartBy, values=~get(x), range = c(~min(get(x)), ~max(get(x)))), 
          list(label='Magnitude', values=~as.numeric(mag), tickvals=as.numeric(sort(unique(df$mag))), ticktext=sort(unique(df$mag))), 
          list(label='Injuries', values=~inj, range = c(~min(inj), ~max(inj))), 
          list(label='Fatalities', values=~fat, range = c(~min(fat), ~max(fat))), 
          list(label='Loss ($)', values=~dl, range = c(~min(dl), ~max(dl)))
  )))
}

getChartData <- function(data, x){
  result <- data %>% group_by_at(c(x, 'mag')) %>% summarise(
    fat = sum(fat), 
    inj = sum(inj), 
    dl = sum(dollarloss, na.rm = TRUE), 
    count = n()
  ) %>% group_by_at(x) %>% mutate(percent = count * 100 / sum(count))
  
  return(result)
}

getTable<-function(df, chartBy) {
  colnames(df)<-c(chartBy, 'Magnitude', 'Fatalities', 'Injuries', 'Loss ($)', 'Count', 'Percent(per year)')
  return(datatable(df, rownames= FALSE, options = list(scrollY = '100%')))
}

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
        selectInput("state1Select", "State 1", states, "IL"), 
        textOutput("county1Count", inline = TRUE), 
        selectInput("county1Select", "County 1", c("All"), "All")
      ), 
      
      div(class = "spacer"), 
      div(
        class = "filter-group states", 
        textOutput("state2Count", inline = TRUE), 
        selectInput("state2Select", "State 2", states, "TX"), 
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
          sliderInput("yearSlider", "Year", min = 1950, max = 2017, value = c(2000, 2017), step = 1, 
            sep = "", width = "100%", animate = animationOptions(interval = 300, loop = FALSE))
        )
      ), 
      
      div(class = "spacer"), 
      div(
        class = "filter-group", 
        createButtonGroup('magnitudeFilter'), 
        div(
          id = "magnitudeDiv", 
          class = "filterContainer", 
          checkboxGroupInput("magGroup", label = "Magnitude (F-scale)", choices = magnitudes, selected = "All")
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

updateColorBy <-
  function(isSelected, session, input, idOfSelectedColorBy) {
    if (isSelected) {
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
  function(isSelected, session, input, idOfSelectedWidthBy) {
    if (isSelected) {
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
    updateColorBy(input$magnitudeFilterColor, session, input, "magnitudeFilterColor") 
  })
  observeEvent(input$widthFilterColor, {
    updateColorBy(input$widthFilterColor, session, input, "widthFilterColor") 
  })
  observeEvent(input$lengthFilterColor, {
    updateColorBy(input$lengthFilterColor, session, input, "lengthFilterColor") 
  })
  observeEvent(input$distanceFilterColor, {
    updateColorBy(input$distanceFilterColor, session, input, "distanceFilterColor") 
  })
  observeEvent(input$lossFilterColor, {
    updateColorBy(input$lossFilterColor, session, input, "lossFilterColor") 
  })
  observeEvent(input$injuriesFilterColor, {
    updateColorBy(input$injuriesFilterColor, session, input, "injuriesFilterColor") 
  })
  observeEvent(input$fatalitiesFilterColor, {
    updateColorBy(input$fatalitiesFilterColor, session, input, "fatalitiesFilterColor") 
  })
  
  observeEvent(input$magnitudeFilterWidth, {
    updateWidthBy(input$magnitudeFilterWidth, session, input, "magnitudeFilterWidth") 
  })
  observeEvent(input$distanceFilterWidth, {
    updateWidthBy(input$distanceFilterWidth, session, input, "distanceFilterWidth") 
  })
  observeEvent(input$lossFilterWidth, {
    updateWidthBy(input$lossFilterWidth, session, input, "lossFilterWidth") 
  })
  observeEvent(input$injuriesFilterWidth, {
    updateWidthBy(input$injuriesFilterWidth, session, input, "injuriesFilterWidth") 
  })
  observeEvent(input$fatalitiesFilterWidth, {
    updateWidthBy(input$fatalitiesFilterWidth, session, input, "fatalitiesFilterWidth") 
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
    
    state1Data <- subset(plotData, st == input$state1Select)
    output$state1Count <- renderText(paste(nrow(state1Data), "records"))
    county1Data <<- if (input$county1Select == 0) state1Data else
      subset(state1Data, grepl(paste0(":", input$county1Select, ":"), fips, fixed = TRUE))
    output$county1Count <- renderText(paste(nrow(county1Data), "records"))
    
    chart1Data <- getChartData(county1Data, switch(
      input$chartBySelect, 
      "Year" = "yr", 
      "Month" = "mo", 
      "Hour" = "hr"#, 
      #"Distance from Chicago" =, 
      #"County"
    ))
    
    state2Data <- subset(plotData, st == input$state2Select)
    output$state2Count <- renderText(paste(nrow(state2Data), "records"))
    county2Data <<- if (input$county2Select == 0) state2Data else
      subset(state2Data, grepl(paste0(":", input$county2Select, ":"), fips, fixed = TRUE))
    output$county2Count <- renderText(paste(nrow(county2Data), "records"))
    
    chart2Data <- getChartData(county2Data, switch(
      input$chartBySelect, 
      "Year" = "yr", 
      "Month" = "mo", 
      "Hour" = "hr"#, 
      #"Distance from Chicago" =, 
      #"County"
    ))

    output$parcoordchart1<-renderPlotly({
      getParCoordChart(chart1Data, input$chartBySelect)
    })
    output$countpercent1<-renderPlotly({
      getMagnitudeChart(chart1Data, input$chartBySelect)
    })
    
    output$parcoordchart2<-renderPlotly({
      getParCoordChart(chart2Data, input$chartBySelect)
    })
    output$countpercent2<-renderPlotly({
      getMagnitudeChart(chart2Data, input$chartBySelect)
    })
    output$table1 = DT::renderDataTable({
      getTable(chart1Data, input$chartBySelect)
    })
    output$table2 = DT::renderDataTable({
      getTable(chart2Data, input$chartBySelect)
    })

  })
  
  myMap_reval <- reactiveVal(foundational.map(state, heatmap, heatmapby))
  geoid_map1 <- reactiveValues(stf=-1, ctf= -1)
geoid_map2 <- reactiveValues(stf=-1, ctf= -1)
group<-list(id=vector())

prev1<-reactiveValues(id = vector())
prev2<-reactiveValues( id = vector())


  output$sampleMap1 <- renderLeaflet({
   
   map<- myMap_reval()
  
  })

  output$sampleMap2 <- renderLeaflet({
  
   map<- myMap_reval()
  })


  observeEvent( input$sampleMap1_shape_click, ignoreNULL = T, ignoreInit = T, {
    
    click1 <- input$sampleMap1_shape_click
    
   

  if(geoid_map1$stf==-1)
    { 
    
      geoid_map1$stf <- code_get(click1$id, state)$stf
      geoid_map1$ctf <- code_get(click1$id, state)$ctf 

    lines <- rgdal::readOGR(paste0("data/GeoJson/", geoid_map1$stf, ".geojson"))

      

      leafletProxy("sampleMap1", session) %>% addPolylines(data = lines, layerId = lines$tornadoId)
     }
    else
    { 
       print("here")
       geoid_map1$stf <- code_get(click1$id, state)$stf
       geoid_map1$ctf <- code_get(click1$id, state)$ctf 

    
      lines <- rgdal::readOGR(paste0("data/GeoJson/", geoid_map1$stf, ".geojson"))

 
        leafletProxy("sampleMap1", session) %>% removeShape(layerId = prev1$id) %>% addPolylines(data = lines, layerId = lines$tornadoId)
    }
    prev1$id <- lines$tornadoId
    
  })


  observeEvent( input$sampleMap2_shape_click, ignoreNULL = T, ignoreInit = T, {
    
    click2 <- input$sampleMap2_shape_click
    
  
    

    if(geoid_map2$stf==-1)
    { 
      geoid_map2$stf <- code_get(click2$id, state)$stf
    geoid_map2$ctf <- code_get(click2$id, state)$ctf
    
    lines <- rgdal::readOGR(paste0("data/GeoJson/", geoid_map2$stf, ".geojson"))

      leafletProxy("sampleMap2", session) %>% addPolylines(data = lines, layerId = lines$tornadoId)
     }
    else
    { 
      print("here")
      geoid_map2$stf <- code_get(click2$id, state)$stf
    geoid_map2$ctf <- code_get(click2$id, state)$ctf
    
    lines <- rgdal::readOGR(paste0("data/GeoJson/", geoid_map2$stf, ".geojson"))


        leafletProxy("sampleMap2", session) %>% removeShape(layerId = prev2$id) %>% addPolylines(data = lines, layerId = lines$tornadoId)
    }
    prev2$id <- lines$tornadoId
    #lines.of.interest <- us[ which( us$GEO_ID %in% click.list$ids ), ] # for later
    print(geoid_map2)
 
  })


  observe({
    
  })
}

shinyApp(ui = ui, server = server)