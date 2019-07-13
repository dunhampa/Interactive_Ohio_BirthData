# Load packages ----
library(shiny)
library(leaflet)

library(rgdal)
library(magrittr)
library(jsonlite)


data_prep<-function(){
  
  birthdata<-read.csv("ReportOutput-BirthWeight.csv", stringsAsFactors = FALSE)
  
  
  
  states <- readOGR("county/cb_2015_us_county_20m.shp",
                    layer = "cb_2015_us_county_20m", GDAL1_integer64_policy = TRUE)
  
  Statekey<-read.csv('STATEFPtoSTATENAME_Key.csv', colClasses=c('character'), stringsAsFactors = FALSE) #AsFactor changed from original prep
  states<-merge(x=states, y=Statekey, by="STATEFP", all=TRUE)
  
  
  SingleState <- subset(states, states$STATENAME %in% c("Ohio"))
  
   SumState<-birthdata2 %>%
    group_by(CountyCountyName,YearBirthYearDesc, LowBirthWeightIndLowBirthWeightIndDesc) %>%
    summarise(BirthTotal=sum(as.numeric(replace_na(as.numeric(BirthCount),0))))
  
  
  
  
}


#data_prep()

# User interface ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: #fff; }"))
  ),
  #tags$style(type = "text/css", "#myMap {height: calc(75vh - 80px) !important;}"),
  tags$style(type = "text/css", "#myMap{ height: 500px !important;; }"),
  #mapid { height: 180px; }
  
  titlePanel("Birth Weights by County"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("Obser_Choice", h3("Observation to Explore:"), 
                  choices = list("Birth Weight" = "Birth Weight", "Maternal Age" = "Maternal Age",
                                 "Birth Count" = "Birth Count"), selected = "Birth Weight")
    ),
    
    mainPanel(
      leafletOutput("myMap"),
      tags$div("Source: Data was obtained from public facing website Ohio Department of Health Ohio Public Warehouse: http://publicapps.odh.ohio.gov/EDW/DataBrowser/Browse/OhioLiveBirths")
    ),
  
  
    
  
  )
  
  
  
 
)

# Server logic
server <- function(input, output) {
  
  

  
  
 
  
  output$myMap <- renderLeaflet({
   map()
  })
 
  
  what <- reactive({
    input$Obser_Choice
    print(input$Obser_Choice)
    
    
  })
  
  observeEvent(input$Obser_Choice, { # update the location selectInput on map clicks
    print(input$Obser_Choice)
    
    
    
    
    
  }) 
  
  
   
  map <- reactive({
    #leaflet() %>%
     # options = leafletOptions(zoomControl = FALSE) %>%
      readRDS("map.rds")
      #addTiles()
      
     
  })
  

  
  observeEvent(input$myMap_shape_click, { # update the location selectInput on map clicks
    p <- input$myMap_shape_click
    p2<-input$myMap_center
    p3<-input$myMap_zoom
    print(p)
    print(str(p2))
    print(str(p3))
  }) 
  
  
}

# Run the app
shinyApp(ui, server)
