# Load packages ----
library(shiny)
library(leaflet)
library(ggplot2)
library(plyr)


birthdata<-read.csv("Report_Data_2019_07July_08.csv", stringsAsFactors = FALSE)

#gets county specifc ggplot for county clicked
get_county_plot<-function(birthdata, county){
  
  data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2014","2015","2016","2017","2018"),]
  
  colnames(data)<-c("BirthWeight" ,"MaternalAge", "Year", "County", "BirthCount", "BirthCount_Percent")
  
  data$BirthWeight<-revalue(data$BirthWeight, c("Low birth weight (<2500g)"="< 5.5 lbs", "Normal birth weight (2500g+)"="5.5 lbs+"))
  data$BirthCount[data$BirthCount=="*"]<-0
  data$BirthCount<-as.numeric(replace_na(as.numeric(data$BirthCount),0))
  data$MaternalAge<-as.factor(data$MaternalAge)
  data$MaternalAge<-revalue(data$MaternalAge, c("Less than 15"="< 15", "45 and older"="> 44"))
  data$MaternalAge<-factor(data$MaternalAge, levels=c("< 15","15 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","> 44"))
  
  
  plot<-ggplot(data, aes(x=MaternalAge, y=BirthCount, fill=BirthWeight))+
    geom_bar(stat="identity")+
    facet_grid(Year~.) +
    theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust=.5,margin = unit(c(5, 5, 5, 5), "mm"))) +
    theme(axis.title.x  = element_text(color = "black", size = 16, face = "bold",margin = unit(c(5, 5, 5, 5), "mm"))) +
    theme(axis.title.y  = element_text(color = "black", size = 16, face = "bold", vjust=.5,margin = unit(c(3, 3, 3, 3), "mm"))) + 
    theme(axis.text.y  = element_text(color = "black", size = 12, vjust=.5,margin = unit(c(2, 2, 2, 2), "mm"))) + 
    theme(axis.text.x = element_text(angle=90, hjust =1)) +
    theme(panel.background = element_rect(fill = "#D7EDF9",size = 1, linetype = "solid")) +
    theme(strip.text.y  = element_text(color = "black", size = 12, face = "bold")) +
    theme(legend.title =  element_text(color = "black", size = 12, face = "bold")) +
    labs(title="County Birth Data" , x= "Maternal Age", y="Birth Count", fill = "Birth Weight") 
  
  plot
}


# User interface ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: #fff; }"))
  ),
  tags$style(type = "text/css", "#myMap{ height: 500px !important;; }"),
  
  titlePanel("Birth Weights by County"),
  
  sidebarLayout(
    mainPanel(
      leafletOutput("myMap"),
      plotOutput("county_plot"),
      tags$div("Source: Data was obtained from public facing website Ohio Department of Health Ohio Public Warehouse: http://publicapps.odh.ohio.gov/EDW/DataBrowser/Browse/OhioLiveBirths")
    ),
    sidebarPanel(
      selectInput("Obser_Choice", h3("Observation to Explore:"), 
                  choices = list("Birth Weight" = "Birth Weight", "Maternal Age" = "Maternal Age",
                                 "Birth Count" = "Birth Count"), selected = "Birth Weight")
      
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
    
    #gets county specifc ggplot for county clicked
    get_county_plot<-function(birthdata, county){
      
      data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2014","2015","2016","2017","2018"),]
      
      colnames(data)<-c("BirthWeight" ,"MaternalAge", "Year", "County", "BirthCount", "BirthCount_Percent")
      
      data$BirthWeight<-revalue(data$BirthWeight, c("Low birth weight (<2500g)"="< 5.5 lbs", "Normal birth weight (2500g+)"="5.5 lbs+"))
      data$BirthCount[data$BirthCount=="*"]<-0
      data$BirthCount<-as.numeric(replace_na(as.numeric(data$BirthCount),0))
      data$MaternalAge<-as.factor(data$MaternalAge)
      data$MaternalAge<-revalue(data$MaternalAge, c("Less than 15"="< 15", "45 and older"="> 44"))
      data$MaternalAge<-factor(data$MaternalAge, levels=c("< 15","15 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","> 44"))
      
      
      plot<-ggplot(data, aes(x=MaternalAge, y=BirthCount, fill=BirthWeight))+
        geom_bar(stat="identity")+
        facet_grid(Year~.) +
        theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust=.5,margin = unit(c(5, 5, 5, 5), "mm"))) +
        theme(axis.title.x  = element_text(color = "black", size = 16, face = "bold",margin = unit(c(5, 5, 5, 5), "mm"))) +
        theme(axis.title.y  = element_text(color = "black", size = 16, face = "bold", vjust=.5,margin = unit(c(3, 3, 3, 3), "mm"))) + 
        theme(axis.text.y  = element_text(color = "black", size = 12, vjust=.5,margin = unit(c(2, 2, 2, 2), "mm"))) + 
        theme(axis.text.x = element_text(angle=90, hjust =1)) +
        theme(panel.background = element_rect(fill = "#D7EDF9",size = 1, linetype = "solid")) +
        theme(strip.text.y  = element_text(color = "black", size = 12, face = "bold")) +
        theme(legend.title =  element_text(color = "black", size = 12, face = "bold")) +
        labs(title=paste0(county," County Birth Data") , x= "Maternal Age", y="Birth Count", fill = "Birth Weight") 
      
      plot
    }
    birthdata<-read.csv("Report_Data_2019_07July_08.csv", stringsAsFactors = FALSE)
    output$county_plot<-renderPlot({ get_county_plot(birthdata,input$myMap_shape_click$id[1])})
    
    
    
    
    #for debugging, print position to console for map alignment
    if(FALSE){
    p <- input$myMap_shape_click
    p2<-input$myMap_center
    p3<-input$myMap_zoom
    
    print(p)
    print(p$id[1])
    #print(str(p2))
    #print(str(p3))
    }
  }) 
  
  
}

# Run the app
shinyApp(ui, server)
