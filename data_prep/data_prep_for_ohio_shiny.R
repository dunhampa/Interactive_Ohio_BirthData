#run data_prep() when you ready

data_prep<-function(){
  
  #data prep for shiny app

  library(plyr)
  library(shiny)
  library(rgdal)
  library(magrittr)
  library(leaflet)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  
  
  
  birthdata<-read.csv(paste0(getwd(),"/data_prep/","Report_Data_2019_07July_08.csv"), stringsAsFactors = FALSE)
  data<-birthdata[birthdata$YearBirthYearDesc %in% c("2018"),]
  
  colnames(data)<-c("BirthWeight" ,"MaternalAge", "Year", "County", "BirthCount", "BirthCount_Percent")
  
  data$BirthWeight<-revalue(data$BirthWeight, c("Low birth weight (<2500g)"="< 5.5 lbs", "Normal birth weight (2500g+)"="5.5 lbs+"))
  data$BirthCount[data$BirthCount=="*"]<-0
  data$BirthCount<-as.numeric(replace_na(as.numeric(data$BirthCount),0))
  
  data$BirthCount_Percent[data$BirthCount_Percent=="*"]<-0
  data$BirthCount_Percent<-as.numeric(replace_na(as.numeric(data$BirthCount_Percent),0))
  
  
  
  data<-data[,1:6] %>% 
    group_by(County, BirthWeight, BirthCount_Percent ) %>%
    #group_by(County, BirthWeight, BirthCount_Percent ) 
    summarize(totals=sum(BirthCount), low=sum(BirthCount_Percent))

  birthdata <- data %>%
      spread( BirthWeight, value="totals") 
  
  birthdata$`< 5.5 lbs`[birthdata$`< 5.5 lbs`=="*"]<-0
  birthdata$`< 5.5 lbs`<-as.numeric(replace_na(as.numeric(birthdata$`< 5.5 lbs`),0))
  
  birthdata$`5.5 lbs+`[birthdata$`5.5 lbs+`=="*"]<-0
  birthdata$`5.5 lbs+`<-as.numeric(replace_na(as.numeric(birthdata$`5.5 lbs+`),0))
  
 birthdata<-birthdata %>%
   group_by(County) %>%
   summarize(low_birth_lbs=sum(`< 5.5 lbs`), norm_birth_lbs=sum(`5.5 lbs+`)) %>%
   mutate(percent=low_birth_lbs/(low_birth_lbs+norm_birth_lbs))
  
  states <- readOGR(paste0(getwd(),"/data_prep/","county/cb_2015_us_county_20m.shp"),
                    layer = "cb_2015_us_county_20m", GDAL1_integer64_policy = TRUE)
  
  Statekey<-read.csv( paste0(getwd(),"/data_prep/",'STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))
  states<-merge(x=states, y=Statekey, by="STATEFP", all=TRUE)
    
    
    SingleState <- subset(states, states$STATENAME %in% c(
      "Ohio"
    ))
    
    SingleState<-sp::merge(x=SingleState, y=birthdata, by.x="NAME", by.y="County", by=x)
    
    map<-leaflet(SingleState,options = leafletOptions(zoomControl = FALSE, zoomLevelFixed = TRUE, dragging=FALSE, minZoom = 7, maxZoom = 7) ) %>%
    #Worked: map<-leaflet(SingleState,options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE, minZoom = 7, maxZoom = 7) ) %>%  
      
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  layerId = ~NAME,
                  fillColor = ~colorQuantile("Reds",percent )(percent),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",SingleState$NAME,"</font></center>","<b>% of Low Birth Weight Births: </b>", sprintf("%1.2f%%", 100*SingleState$percent),"<br/>")))
    
    map<-map %>% setView(-82.1, 39.9,  zoom = 7)
  
    
    saveRDS(map, file="map.rds")
  
}