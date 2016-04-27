require(RCurl)
require(RJSONIO)
require(plyr)
require(leaflet)

############ set up, run one at a time
setwd(getwd())
BingMapsKey <- readline("Enter Bing key: ")
MapquestKey <- readline("Enter Mapquest key: ")
geoghelper<- readline("Can you define area to improve geocoding accuracy? i.e. country:")


### google maps- limtied to 2,500 a day and resutls need to be displayed
### Code from: http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
### can also geocode using ggmap

Google <- function(str,verbose=FALSE) {
  construct.geocode.url <- function(str, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", str, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }
    if(verbose) cat(str,"\n")
    u <- construct.geocode.url(str)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      data<-(c(lat, lng))
    } else {
      data<-(c(NA,NA,NA, NA))
    }
    Sys.sleep(5)
    data[3]<-"Google"
    return(data)
  }
############ BING
############ get key from : https://www.bingmapsportal.com/Application
############ BingMapskey=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG

BING <- function(str){
  require(RCurl)
  require(RJSONIO)
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=", BingMapsKey))
  d <- getURL(u)
  j <- fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  data<-c(lat,lng)
  data[3]<-"BING"
  Sys.sleep(5)
  return(data)
}  

### Mapquest/OSM- 15,000 calls a month, and results have to be displayed (same with google)
### key from: https://developer.mapquest.com/user/me/plan
### mykey: aGO27r4vYdEZP9yZzSxz2Vw4Akhbyb6i


Mapquest <- function(str){
  
  require(RCurl)
  require(RJSONIO)
  url <- paste0("http://www.mapquestapi.com/geocoding/v1/address?key=", MapquestKey, "&location=", str, "&outFormat=json")
  raw_json <- scan(url, "", sep="\n")
  j <- fromJSON(raw_json) 
  if (j$info$statuscode == 0) {
    coord <- j$results[[1]]$locations[[1]]$latLng
    latlong<-t(data.frame(coord))
  }
  else {    
    latlong <- NA
  }
  data<-c(latlong)
  data[3]<-"Mapquest"
  Sys.sleep(5)
  return(data)
}  

# Nomatim-OSM - ok for one time bulk usage, but should limit use to < 1,000 a day 

Nomatim <- function(str){
  require(RCurl)
  require(RJSONIO)
  str<-gsub(" ", "+", str)
  url <- paste0("http://nominatim.openstreetmap.org/search?q=", str,"&addressdetails=1&format=json&limit=1")
  raw_json <- scan(url, "", sep="\n")
  if (nchar(raw_json)<3){
    data<-NA}
  else{
  j <- fromJSON(raw_json) 
  if (j[[1]]$lat>0) {
    lat <- j[[1]]$lat
    lng <- j[[1]]$lon
 
  }
  else {    
    lat <- lng <- NA
  }
  }
  data<-c(lat,lng)
  data[3]<-"Nomatim"
  Sys.sleep(5)
  return(data)
}  


# get a dataframe from test.csv, has to contain column with "str", which is the addresses

for(i in c("Google", "BING", "Mapquest", "Nomatim")){
setwd(getwd())
test<-read.csv("test.csv")
test$str<-paste0(test$str, ",", geoghelper)
result = mdply(test, i)
str<-result[1]
address<-data.frame(str)
result<-result[2:4]
setwd("results")
write.table(result,file="master.csv", append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE, sep=",")
}

################ VIZZZZZZZZZZZZZZZZ

map<-read.csv("master.csv", header=F)
colnames(map)<-c("lat", "long", "provider")
mapleaflet <- sp::SpatialPointsDataFrame(
  cbind(
    map$long,  # lng
    map$lat # lat
  ),
  data.frame(type = factor(
    map$provider), address=(address$str)
  )
)
pal <- colorFactor(c("chartreuse1", "blueviolet", "darkgoldenrod1", "firebrick1"), domain = c("Google", "BING", "Mapquest", "Nomatim"))
leaflet(mapleaflet) %>% addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.75,radius=10, popup = ~as.character(address)) %>%
  addLegend("bottomright", pal = pal, values=~type,
            title = "Geocoding Provider",
            opacity = 1
  )