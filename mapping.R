#plot location of VAS and cases

#install.packages('plyr')
library(leaflet)
library(magrittr)
library(plyr)
library(dplyr)

vs=na.omit(vas16)
df2 <- sp::SpatialPointsDataFrame(
  cbind(vs$Longitude, vs$Latitude), data = data.frame(vs$Population, vs$No..ASV))

leaflet()  %>% 
  addTiles() %>%
  addCircles(data=df2, radius = ~ (sqrt(vs$Population)*50),weight=1,
             popup = ~ as.character(vs$Population))
#addProviderTiles("OpenStreetMap.BlackAnsedWhite") %>% 
#addProviderTiles("Stamen.TonerLabels") %>% 
#addMarkers(data=df2, popup = ~ as.character(vs$Population))

vs15=na.omit(vas15)
h=na.omit(human$Longitude)
h=human
h<-h[!is.na(h$Long),]
df2 <- sp::SpatialPointsDataFrame(
  cbind(vs15$Longitude, vs15$Latitude), data = data.frame(vs15$Population, vs15$No..ASV))
df1 <- sp::SpatialPointsDataFrame(
  cbind(h$Longitude, h$Latitude), data=h)

qpal<-colorFactor(palette="Accent",domain=h$Year)
ppal<-colorQuantile(palette="Blues",domain=vs15$Population,n=5)
h$Yrfact=factor(h$Year)

map<-leaflet()  %>%   
  addTiles() %>%
  addCircles(data=df2, radius = ~ (sqrt(vs15$Population)*50),weight=1) %>%
  addCircles(data=h,~h$Long,~h$Lat,color=~qpal(h$Year),radius=10000,
           fill=TRUE,stroke=FALSE,opacity=0.5) %>%
  addCircleMarkers(data=h,~h$Long,~h$Lat,weight=5,color=qpal(h$Year),
             fill=TRUE,stroke=TRUE,opacity=1) %>%
  addLegend("topright",pal=qpal,values=h$Year,title="Year of GW case",opacity=1)
  addLegend("topright",pal=ppal,values=vs15$Population,title="Year of GW case",opacity=1)

#map of human cases, by year, with 2015 VAS overlapped  
h=human
h<-h[!is.na(h$Long),]
d=dog
d<-d[!is.na(dog$Longitude),]

qpal<-colorFactor(palette="Accent",domain=h$Year)
ppal<-colorQuantile(palette="Blues",domain=vs15$Population,n=5)

map<-leaflet()  %>%   
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircles(data=vs12, radius = 1000,weight=1,group="VAS 2012",color="blue") %>%
    addCircles(data=vs13, radius = 1000,weight=1,group="VAS 2013",color="blue") %>%
    addCircles(data=vs14, radius = ~ (sqrt(vs14$Population)*50),weight=1,group="VAS 2014",color="blue") %>%
    addCircles(data=vs15, radius = ~ (sqrt(vs15$Population)*50),weight=1,group="VAS 2015",color="blue") %>%
    addLegend("topright",pal=qpal,values=h$Year,title="Year of GW case",opacity=1) 
map %>% addCircleMarkers(data=h,~h$Long,~h$Lat,weight=2,fillColor=qpal(h$Year),group="human",radius=9,
              opacity=1,color="black",stroke=TRUE,dashArray=ifelse(h$Case.contained=="YES",'5,0','3,6'),
              #fill=ifelse(h$Case.contained=="YES",TRUE, FALSE),fillOpacity=1,
              fill=TRUE,fillOpacity=1,
              #clusterOptions=markerClusterOptions(),
        popup=~paste(paste("human",h0$Year,h$Case..,sep="-"),paste("Lat:",h$Lat),paste("Long:",h$Long),
               paste("Contained?",h$Case.contained),
               paste("in VAS?",h$in.VAS..per.case.line.list.), sep="<br/>")) %>%
  addCircleMarkers(data=d,~d$Longitude,~d$Latitude,weight=2,color=qpal(d$Year),group="dog",
                           fill=TRUE,stroke=FALSE,fillOpacity=1,radius=4,
                           popup=~paste(paste("dog",d$Year,d$Case..,sep="-"),paste("lat:",d$Latitude),
                                        paste("long:",d$Longitude),sep="<br/>")) %>%
  addScaleBar() %>% addLayersControl(
    overlayGroups=c("human","dog","VAS 2012","VAS 2013","VAS 2014","VAS 2015"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
#map of VAS from 2012-2015
vs14=vas14
vs14<-vs14[!is.na(vs14$Long),]
vs15=vas15
vs15<-vs15[!is.na(vs15$Longitude),]
vs12=vas12
vs12<-vs12[!is.na(vs12$Longitude),]
vs13=vas13
vs13<-vs13[!is.na(vs13$Longitude),]

mymap <- leaflet() %>% addTiles() %>%
  addCircles(data=vs12,~vs12$Longitude,~vs12$Latitude, radius = ~ 1000,weight=1,color="red",group="2012") %>%
  addCircles(data=vs13,~vs13$Longitude,~vs13$Latitude, radius = ~ 1000,weight=1,color="blue",group="2013") %>%
  addCircles(data=vs14,~vs14$Long,~vs14$Lat, radius = ~ (sqrt(vs14$Population)*100),weight=1,color="green",group="2014",
             popup=~paste(vs14$Lat,vs14$Long, sep="<br/>")) %>%
  addCircles(data=vs15,~vs15$Longitude,~vs15$Latitude, radius = ~ (sqrt(vs15$Population)*100),group="2015",
             weight=1,color="black",
              popup=~paste(vs15$Latitude,vs15$Longitude, sep="<br/>"))
mymap %>%   addScaleBar() %>% 
  addLayersControl(
    overlayGroups=c("2012","2013","2014","2015"),
    options = layersControlOptions(collapsed = FALSE)
  )

#map of dogs
d=dog
d<-d[!is.na(dog$Longitude),]

map<-leaflet()  %>%   
  addTiles() %>%
  addCircles(data=vs14, radius = ~ (sqrt(vs14$Population)*50),weight=1) %>%
  addLegend("topright",pal=qpal,values=d$Year,title="Year of GW case",opacity=1)
map %>% addCircleMarkers(data=d,~d$Longitude,~d$Latitude,weight=3,color=qpal(d$Year),
                         fill=TRUE,stroke=TRUE,opacity=1,
                         popup=~paste(paste(d$Year,d$Case..,sep=" #"),paste("lat:",d$Latitude),
                                      paste("long:",d$Longitude),sep="<br/>"))

#test a diff??
