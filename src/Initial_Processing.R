library(googlesheets4)
sheet.gid <- "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo"

kits.2019 <- read_sheet(sheet.gid, sheet = "2019")
kits.2020 <- read_sheet(sheet.gid, sheet = "2020")
kits.2021 <- read_sheet(sheet.gid, sheet = "2021")

kits.combined <- rbind(kits.2019, kits.2020, kits.2021)


kits.combined.unique <- kits.combined[!duplicated(kits.combined$`Kit ID`), ]

kits.ready <- kits.combined.unique[!is.na(kits.combined.unique$Address), ]
kits.ready$geo <- NA
kits.ready$testlon <- NA

kits.ready<-kits.ready[!(kits.ready$Address=="21506" | kits.ready$Address=="19391" | kits.ready$Address=="No Form Provided"),]

library (sjmisc)
for (i in 1:nrow(kits.ready)){
  if (str_contains(kits.ready$Address[i], "IN")){
    print (kits.ready$Address[i])
  } else {
    kits.ready$Address[i] <- paste (kits.ready$Address[i],", IN", sep="")
    print (kits.ready$Address[i])
  } 
}

library("ggmap")

source("src/API.R")
print(API)
register_google(key = API)

for(i in 1:nrow(kits.ready)){
  if (kits.ready$geo[i] == 'Y' & is.na(kits.ready$geo[i]) == FALSE ){
    print ('already geocoded')
  } else{
    result <- geocode(as.character(kits.ready$Address[i]), output = "latlona", source = "google")
    kits.ready$testlon[i] <- as.numeric(result[1])
    if (is.na(kits.ready$testlon[i])==TRUE){
      kits.ready$geo[i]='E'
      kits.ready$lon[i] <- NA
      kits.ready$lat[i] <- NA
      kits.ready$geoAddress[i] <- NA
     } else {
      kits.ready$geo[i]='Y'
      kits.ready$lon[i] <- as.numeric(result[1])
      kits.ready$lat[i] <- as.numeric(result[2])
      kits.ready$geoAddress[i] <- as.character(result[3])
    }
  }
}

head(kits.ready)

library(dplyr)
library(tidyr)

kits.ready %>% select(-testlon)

kits.ready <- apply(kits.ready,2,as.character)

write.csv(kits.ready, 'kits_ready.csv', na="-")

final.kits <- read.csv('kits_ready.csv')
final.kits.no.na <- drop_na(final.kits)

sheet_write(final.kits, ss = "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo", sheet = "Combined + Geocoded")

library (sf)
parcels = st_read("src/sbparcels")
parcels <- st_transform(parcels, crs=4326)
final.kits.sf = st_as_sf(final.kits.no.na, coords = c(x = "lon", y = "lat"), crs = 4326)

parcel_join <- st_join(final.kits.sf, parcels, join = st_nearest_feature)
as.data.frame(parcel_join)
parcel_join$conyear <- NA

kit_parcel <- subset(parcel_join, select = c("Kit.ID", "PARCELID"))

library(XML)
library(RCurl)
library(rlist)

sid = "1944116155AE43FBA68587F1F8294D9C"

for(i in 1:nrow(parcel_join)){
  url.overview <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=",
                       parcel_join$PARCELSTAT[i],
                       "&sid=",
                       sid)
  print(url.overview)
  theurl <- getURL(url.overview,.opts = list(ssl.verifypeer = FALSE) )
  library(xml2)
  each.html <- read_html(url.overview)
  write_html(each.html,"file.html",encoding = "UTF-8") 
  tables <- readHTMLTable("file.html")
  #try without caching 
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  q <- as.data.frame(tables[2])
  
  library(tidyverse)
  q <- q[ ,4:ncol(q)] %>% na.omit()
  if (nrow(q) < 2){
      print (paste0 (parcel_join$PARCELSTAT[i], " is vacant"))
  }else{
      var.names <- c("Improvement Type Code", 	"Building No.", 	"ID No.", 	"Constructed Year", "Grade" 	,"Total Base Area","Replacement Cost")
      names(q)<-var.names
      parcel_join$conyear[i] <- as.numeric(min(q$`Constructed Year`))
  }
}





library (leaflet)
library (sp)

final.kits$lon <- as.numeric(final.kits$lon)
final.kits$lat <- as.numeric(final.kits$lat)

final.kits.SP <- SpatialPointsDataFrame(final.kits[,c(16,17)], final.kits[,-c(16,17)])


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = final.kits, lng= ~lon, lat= ~lat, popup= ~Address)

m

head(kits.ready)

