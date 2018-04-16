library(shiny)
library(leaflet)
library(shinyjs)

magList <- list("All" = 10,"Unknown" = -9, "Light" = 0, "Moderate" = 1,"Significant" = 2,"Severe" = 3,"Devastating" = 4,"Catastrophic" = 5)

#creates button group for filtering based on color/width
createButtonGroup <- function(){
  div(class="buttonGroup",
      tags$button(
        id = "colorBtn",
        class = "btn action_button",
        img(id= "colorBtnImg", src = "images/color.png", height = "20px")
      ),
      tags$button(
        id = "widthBtn",
        type = "button",
        class = "btn action_button",
        img(id= "widthBtnImg", src = "images/width_white.png", height = "20px")
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
          
          hr(class='doubleRule'),
          
          h4('Filter By:'),
          checkboxInput("magnitudeFilter","Magnitude", value = FALSE),
          conditionalPanel(condition = "input.magnitudeFilter == true",
            div(id = 'magnitudeDiv', class = 'filterContainer',
                checkboxGroupInput("magGroup", label = '', 
                                   choices = magList,
                                   selected = 0)
                )               
          ),
    
          checkboxInput("widthFilter","Width", value = FALSE),
          conditionalPanel(condition = "input.widthFilter == true",
                           div(id = 'widthDiv', class='filterContainer',
                               sliderInput("widthSlider",'',  min = 0, 
                                           max = 5000, value = c(100, 900)),
                               tags$button(
                                 id = "colorBtn",
                                 class = "btn action_button colorFilter",
                                 img(id= "colorBtnImg", src = "images/color.png", height = "20px")
                               )
                           )
          ),
          
          checkboxInput("lengthFilter","Length", value = FALSE),
          conditionalPanel(condition = "input.lengthFilter == true",
          div(class='filterContainer',
              sliderInput("lengthSlider",'',  min = 0, 
                          max = 250, value = c(100, 200)),
              tags$button(
                id = "colorBtn",
                class = "btn action_button colorFilter",
                img(id= "colorBtnImg", src = "images/color.png", height = "20px")
              )
          )),
         
          hr(),
          
          checkboxInput("lossFilter","Loss", value = FALSE),
          conditionalPanel(condition = "input.lossFilter == true",
          div(class='filterContainer', 
               sliderInput("lossSlider",'',  min = 50, 
                           max = 1000000, value = c(30000, 400000)),
              createButtonGroup()
              )),
         
          checkboxInput("fatalitiesFilter","Fatalities", value = FALSE),
          conditionalPanel(condition = "input.fatalitiesFilter == true",
          div(class='filterContainer', 
              sliderInput("fatalitiesSlider",'',  min = 0, 
                          max = 160, value = c(50, 100)),
              createButtonGroup()
              )),
    
          checkboxInput("injuriesFilter","Injuries", value = FALSE),
          conditionalPanel(condition = "input.injuriesFilter == true",
          div(class='filterContainer',
              sliderInput("injuriesSlider",'',  min = 0, 
                          max = 1800, value = c(20, 400)),
              createButtonGroup()
              )),
          
          hr(),
          
          checkboxInput("yearFilter","Year", value = FALSE),
          conditionalPanel(condition = "input.yearFilter == true",
          sliderInput("year", "",
                      min = 1965, max = 2016,
                      value = 1, step = 1, width = '100%',
                      animate =
                        animationOptions(interval = 300, loop = FALSE))),
          
          hr(class='doubleRule'),
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

  #show about page 
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "You Spin Me Round",
      "You Spin Me Round is a geospatial visualization of tornadoes data from 1965-2016.",
      br(), br(),
      "The data for this project is from NOAA's National Weather Service:",
      a(href = 'http://www.spc.noaa.gov/wcm/index.html#data', target='_blank', 'Storm Prediction Center'),
      br(), br(),
      "This visualization allows the users to view the relative strength of the tornadoes and damage caused by them across 
      different states in USA. Additionally, users can filter data by the tornado width, length, injuries, fatalities and loss. Moreover, users
      can also choose the time frame(year, month, hour) for which they want to see the tornado data.",
      # hr(),
      # a(href = '', target='_blank', 'More Details'),
      hr(),
      h5('Team: R You Shiny'),
      "Amey Barapatre | Sai Phaltankar | Jaspreet Kaur Sohal | Vivek R. Shivaprabhu",
      easyClose = TRUE
    ))
  })
  
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