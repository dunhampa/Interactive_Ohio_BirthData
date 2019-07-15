# Load packages ----
library(shiny)
library(leaflet)
library(ggplot2)
library(plyr)
library(shinydashboard)
library(shinyWidgets)


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


## Only run this example in interactive R sessions
  ui <- dashboardPage(
    dashboardHeader(title = "Ohio Birth Data Exploration"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Explore Data", tabName = "dashboard", icon = icon("chart-area")),
        menuItem("Background", tabName = "background", icon = icon("info"))
      )
      
    ),
    dashboardBody(
     
     # tags$head(
      #  tags$style(HTML("
       #   .content-wrapper {
        #    background-color: white !important;
         # }
          
      #  "))
      #),
      
      
      
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
    
                
                fluidRow(
                  
                  column(6,
                         h1("Click On A County"),
                         h3("Pop will show birth totals for 2018")
                         ),
                  column(6,
                         h1("Country Birth Data Details"),
                         h3("Details on Maternal Age and Birth Weight")
                  )
                  
                ),
               
                #fluidRow(
  
                  #valueBoxOutput("vbox")),
                
                fluidRow(
                 #column(1),
                 column(6,
                 #box(
                   leafletOutput("myMap", height=600)
                     #, width="100%")
                     ),  
                 column(6,
                        box(plotOutput("county_plot", height=600), width="100%")),  
                 #column(1),
                 tags$head(
                        tags$style(HTML(".leaflet-container { background: #fff; }"))
                      )#,
                      #tags$style(type = "text/css", "#myMap{ height: 500px !important;; }")
                      
                      ),
                fluidRow(),
                
                fluidRow(
                  
                  title = "",
                  column(2),
                  #column(8,
                  #box(plotOutput("county_plot", height=1000),width="100%")),
                  column(2)
                )
        ),
        
        # Second tab content
        tabItem(tabName = "background",
                h2("Widgets tab content")
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
    
    
    
    observeEvent(input$myMap_shape_click, { # update the location selectInput on map clicks
      
      #gets county specifc ggplot for county clicked
      get_county_plot<-function(birthdata, county){
        
        #data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2014","2015","2016","2017","2018"),]
        data<-birthdata[birthdata$CountyCountyName==county & birthdata$YearBirthYearDesc %in% c("2018"),]
        
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
  
  shinyApp(ui, server)
