#Breaking into 3 different sets:
# 1 by BirthWeight
# 2 by MaternalAge
# 3 by TotalBirths

#the wonky names are left as is from the raw data extract from the warehouse

#data from dataware
#http://publicapps.odh.ohio.gov/EDW/DataBrowser/Browse/OhioLiveBirths

birthdata<-read.csv("Report_Data_2019_07July_08.csv", stringsAsFactors = FALSE)

states <- readOGR("county/cb_2015_us_county_20m.shp",
                  layer = "cb_2015_us_county_20m", GDAL1_integer64_policy = TRUE)

Statekey<-read.csv('STATEFPtoSTATENAME_Key.csv', colClasses=c('character'))

states<-merge(x=states, y=Statekey, by="STATEFP", all=TRUE)

SingleState <- subset(states, states$STATENAME %in% c("Ohio"))


get_ByBirth_map<-function(){
  
  #by birth weights
  ByBirthWeight<-birthdata %>%
    group_by(CountyCountyName,YearBirthYearDesc, LowBirthWeightIndLowBirthWeightIndDesc) %>%
    summarise(BirthTotal=sum(as.numeric(replace_na(as.numeric(BirthCount),0))))
  
  ByBirthWeight<-spread(ByBirthWeight,LowBirthWeightIndLowBirthWeightIndDesc,-CountyCountyName)
  
  CurrentByBirthWeight<-ByBirthWeight[ByBirthWeight$YearBirthYearDesc==2018,]
  
  ByWeightMapData<-sp::merge(x=SingleState, y=CurrentByBirthWeight, by.x="NAME", by.y="CountyCountyName", by=x)
  
  ByWeightMap<-leaflet(ByWeightMapData,options = leafletOptions(zoomControl = FALSE, zoomLevelFixed = TRUE, dragging=FALSE, minZoom = 7, maxZoom = 7) ) %>%
    
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                layerId = ~NAME,
                fillColor = ~colorQuantile("Reds",`Low birth weight (<2500g)` )(`Low birth weight (<2500g)`),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",ByWeightMapData$NAME,"</font></center>","<b># Low Birth Weight Births: </b>", prettyNum(ByWeightMapData$`Low birth weight (<2500g)`,big.mark=","),"<br/>","<b># Normal Birth Weight Births: </b>",prettyNum(ByWeightMapData$`Normal birth weight (2500g+)`,big.mark=","))))
  
  ByWeightMap<-ByWeightMap %>% setView(-82.1, 39.9,  zoom = 7)
  
  #setwd("../shiny_ohio")
  saveRDS(ByWeightMap, file="ByWeightMap.rds")
  #saveRDS(ByWeightMap, file="map.rds")
  
}


get_ByMaternalAge_map<-function(){
  
  #by Maternal Age
  ByMaternalAge<-birthdata %>%
    group_by(CountyCountyName,YearBirthYearDesc, BirthAgeGroupAgeGroupDesc) %>%
    summarise(BirthTotal=sum(as.numeric(replace_na(as.numeric(BirthCount),0))))
  
  ByMaternalAge<-spread(ByMaternalAge,BirthAgeGroupAgeGroupDesc,-CountyCountyName)
  
  CurrentByMaternalAge<-ByMaternalAge[ByMaternalAge$YearBirthYearDesc==2018,]
  
  ByMaternalAgeMapData<-sp::merge(x=SingleState, y=CurrentByMaternalAge, by.x="NAME", by.y="CountyCountyName", by=x)
  
  ByMaternalAgeMap<-leaflet(ByMaternalAgeMapData,options = leafletOptions(zoomControl = FALSE, zoomLevelFixed = TRUE, dragging=FALSE, minZoom = 7, maxZoom = 7) ) %>%
    
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                layerId = ~NAME,
                fillColor = ~colorQuantile("Reds",`Low birth weight (<2500g)` )(`Low birth weight (<2500g)`),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",ByMaternalAgeMapData$NAME,"</font></center>","<b># Low Birth Weight Births: </b>", prettyNum(ByMaternalAgeMapData$`Low birth weight (<2500g)`,big.mark=","),"<br/>","<b># Normal Birth Weight Births: </b>",prettyNum(ByMaternalAgeMapData$`Normal birth weight (2500g+)`,big.mark=","))))
  
  ByMaternalAgeMap<-ByMaternalAgeMap %>% setView(-82.1, 39.9,  zoom = 7)
  
  #setwd("../shiny_ohio")
  saveRDS(ByMaternalAgeMap, file="ByMaternalAgeMap.rds")
  #saveRDS(ByWeightMap, file="map.rds")
  
}





