# Load packages ----
library(shiny)
library(leaflet)
library(ggplot2)
library(tidyr)
library(plyr)
library(shinydashboard)
#library(shinyWidgets)

#setwd()
birthdata<-read.csv(paste0(getwd(),"/data_prep/Report_Data_2019_07July_08.csv"), stringsAsFactors = FALSE)

get_county_plot<-function(birthdata, county, trend=FALSE){
  
  #data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2014","2015","2016","2017","2018"),]
  if(!trend)
  {
    data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2018"),]
  }
  else
  {
    data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2014","2015","2016","2017","2018"),]
  }
  
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
    theme(axis.text.x = element_text(size=12, angle=90, hjust =1)) +
    theme(panel.background = element_rect(fill = "#D7EDF9",size = 1, linetype = "solid")) +
    theme(strip.text.y  = element_text(color = "black", size = 12, face = "bold")) +
    theme(legend.title =  element_text(color = "black", size = 12, face = "bold")) +
    labs(title=paste0(county," County Birth Data") , x= "Maternal Age", y="Birth Count", fill = "Birth Weight") 
  
  plot
}


  ui <- dashboardPage(
    
   
    
    dashboardHeader(title = "Ohio Birth Data Exploration", 
                    tags$li(class = "dropdown", tags$a(HTML("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no\" />")))
                    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Explore Data", tabName = "dashboard", icon = icon("chart-bar")),
        menuItem("5 Year Trend", tabName = "trend", icon = icon("chart-area")),
        menuItem("Background", tabName = "background", icon = icon("info"))
      )
      
    ),
    dashboardBody(
    
      
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                
              
                
                fluidRow(
                  
                  column(6
                        
                         ),
                  column(6
                        
                  )
                  
                ),
                
                fluidRow(
             
                 column(6,
                        h1("Click On A County"),
                        h3("Popup will show birth totals for 2018"),
                   #tags$style(type = "text/css", "#myMap {height: 100vh ;width: 150vh; }"),
                   #leafletOutput("myMap", height=600)
                   leafletOutput("myMap", height=600)
                   #box(leafletOutput("myMap"), height="125%", width="100%")
          
                     ),  
                 column(6 ,
                        h1("Country Birth Data Details"),
                        h3("Details on Maternal Age and Birth Weight"),
                        #tags$style(type = "text/css", "#myMap{ height: 500px !important;; }"),
                        box(plotOutput("county_plot", height=600), width="100%")),
                        #box(plotOutput("county_plot"), height="100%")),  
                 
                 tags$head(
                        tags$style(HTML(".leaflet-container { background: #fff; height: 100%; width: 100%; }"))
                      )
                      ),
                fluidRow()
          
        ),
        tabItem(tabName = "trend",
                fluidRow(
                  h2("County Birth Data Trend"),
                  column(2),
                  column(8,box(plotOutput("county_trend_plot", height=600), width=600)),
                  #column(8,box(plotOutput("county_trend_plot", height="auto"), height="100%")),
                  column(2)
                )
               
        ),
        # Second tab content
        tabItem(tabName = "background",
                h2("Background"),
                includeHTML("background.html")
        )
      )
      
      
      
      
    )
  )
  
  server <- function(input, output) {

    output$vbox <- renderValueBox({
      valueBox(
        "Ohio Birth Data",
        "Interactive Data Exploration By County",
        icon = icon("chart-bar", class="Solid"),
        color="light-blue"
      )
    })
  
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
    
    output$county_plot<-renderPlot({ get_county_plot(birthdata,"Franklin")})
    output$county_trend_plot<-renderPlot({ get_county_plot(birthdata,"Franklin", TRUE)})
    
    observeEvent(input$myMap_shape_click, { # update the location selectInput on map clicks
      
      #gets county specifc ggplot for county clicked
      print(input$myMap_shape_click$id[1])
      #birthdata<-read.csv("Report_Data_2019_07July_08.csv", stringsAsFactors = FALSE)
      output$county_plot<-renderPlot({ get_county_plot(birthdata,input$myMap_shape_click$id[1])})
      output$county_trend_plot<-renderPlot({ get_county_plot(birthdata,input$myMap_shape_click$id[1], TRUE)})
      
      
      
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
  
  shinyApp(ui, server)
