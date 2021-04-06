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

kits.ready %>% select(-testlon)

kits.ready <- apply(kits.ready,2,as.character)

write.csv(kits.ready, 'kits_ready.csv', na="-")

final.kits <- read.csv('kits_ready.csv')

sheet_write(final.kits, ss = "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo", sheet = "Combined + Geocoded")

library (leaflet)
library (sp)

final.kits$lon <- as.numeric(final.kits$lon)
final.kits$lat <- as.numeric(final.kits$lat)

final.kits.SP <- SpatialPointsDataFrame(final.kits[,c(16,17)], final.kits[,-c(16,17)])


m <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = final.kits, lng= ~lon, lat= ~lat, popup= ~Address)

m

library (sf)
parcels = st_read("src/sbparcels")
parcels <- st_transform(parcels, crs=4326)
final.kits.sf = st_as_sf(final.kits, coords = c(x = "lon", y = "lat"), crs = 4326)

parcel_join <- st_join(final.kits.sf, parcels, join = st_nearest_feature)

kit_parcel <- subset(parcel_join, select = c("Kit.ID", "PARCELID"))  

rm(list=ls(all=TRUE)) # clear memory

library(rvest)
library (textreadr)
library(tidyverse)
library(xml2)

sid = "4EDA38D4D6CA4BABBDA499D4CFB99F14"

# testing for a url I know works, but only produces "null"- is it not reading the data properly?

url<- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=", parcel_join$PARCELSTAT[5],"&sid=", sid)
each.html <- read_html(url)
write_html(each.html,"file.html",encoding = "UTF-8")
full.parsed.tables <- each.html %>% 
  html_table(fill = T)

for(i in parcel_join) {
  url[i]<- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=", parcel_join$PARCELSTAT[i],"&sid=", sid)
  each.html <- read_html(url[i])
  write_html(each.html,"file.html",encoding = "UTF-8")
  
  full.parsed.tables <- each.html %>% 
  html_table(fill = T)
  
  each.imp.table <- as.data.frame(full.parsed.tables[13]) %>% #the 13 seems to pull the proper section for all I have done
  select(-c(X1,X2,X3))%>% #tossing some unneded values
  filter_all(any_vars(!is.na(.))) #removing empty rows
  names(each.imp.table) <- each.imp.table[2,] 
  each.imp.table <- data.frame(each.imp.table[3:nrow(each.imp.table),])
  each.assessed.table <- as.data.frame(full.parsed.tables[11]) %>% #the 11 seems to pull the assessment
  select(-c(X2,X6,X7))%>% #tossing some unneded values
  filter_all(any_vars(!is.na(.))) #removing empty rows
  assessed.value <- each.assessed.table[2,3]
}
