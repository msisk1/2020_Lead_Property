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

kits.ready <- apply(kits.ready,2,as.character)

write.csv(kits.ready, 'kits_ready.csv', na="-")

final.kits <- read.csv('kits_ready.csv')
final.kits.no.na <- drop_na(final.kits)
final.kits.no.na$IN <- NA

for (i in 1:nrow(final.kits.no.na)){
  if ((str_contains(final.kits.no.na$geoAddress[i], "south bend")) | (str_contains(final.kits.no.na$geoAddress[i], "elkhart")) | (str_contains(final.kits.no.na$geoAddress[i], "mishawaka"))|(str_contains(final.kits.no.na$geoAddress[i], "granger")) | (str_contains(final.kits.no.na$geoAddress[i], "roseland")) | (str_contains(final.kits.no.na$geoAddress[i], "notre dame"))) {
    final.kits.no.na$IN[i]='Y'
  } else {
    final.kits.no.na$IN[i]='N'
  } 
}

final.kits <- subset(final.kits.no.na, final.kits.no.na$IN=='Y')

sheet_write(final.kits, ss = "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo", sheet = "Combined + Geocoded")

library (sf)
parcels = st_read("src/SJC_2018_Parcels")


parcels <- st_transform(parcels, crs=4326)
final.kits.sf = st_as_sf(final.kits, coords = c(x = "lon", y = "lat"), crs = 4326)

parcel_join <- st_join(final.kits.sf, parcels, join = st_nearest_feature)
as.data.frame(parcel_join)
parcel_join$conyear <- NA

write.csv(parcel_join, "src/parcel_join.csv")

library(XML)
library(RCurl)
library(rlist)

sid = "154087D2E7D34377B7F9B0DDBF06B8CA"

for(i in 1:nrow(parcel_join)){
  url.overview <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=",
                       parcel_join$PARCELSTAT[i],
                       "&sid=",
                       sid)
  print(url.overview)
  theurl <- getURL(url.overview,.opts = list(ssl.verifypeer = FALSE) )
  library(xml2)
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  q <- as.data.frame(tables[2])
  
  library(tidyverse)
  q <- q[ ,4:ncol(q)] %>% na.omit()
  if (ncol(q) < 6){
      print (paste0 (parcel_join$PARCELSTAT[i], " is vacant"))
  }else{
      var.names <- c("Improvement Type Code", 	"Building No.", 	"ID No.", 	"Constructed Year", "Grade" 	,"Total Base Area","Replacement Cost")
      names(q)<-var.names
      parcel_join$conyear[i] <- as.numeric(min(q$`Constructed Year`[q$`Constructed Year`>0]))
  }
}

# I had tried something like this
# parcel_join$conyear[i] <- as.numeric(min(q[q$`Constructed Year`>0, `Constructed Year`]))
# and also had issues


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

