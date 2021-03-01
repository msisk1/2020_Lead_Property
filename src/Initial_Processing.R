library(googlesheets4)
sheet.gid <- "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo"

kits.2019 <- read_sheet(sheet.gid, sheet = "2020")
kits.2020 <- read_sheet(sheet.gid, sheet = "2020")
kits.2021 <- read_sheet(sheet.gid, sheet = "2021")

kits.combined <- rbind(kits.2019, kits.2020, kits.2021)


kits.combined.unique <- kits.combined[!duplicated(kits.combined$`Kit ID`), ]

kits.ready <- kits.combined.unique[!is.na(kits.combined.unique$Address), ]

kits.ready<-kits.ready[!(kits.ready$Address=="21506" | kits.ready$Address=="19391" | kits.ready$Address=="No Form Provided"),]


if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library("ggmap")

source("API.R")
print(API)
register_google(key = API)


for(i in 1:nrow(kits.ready)){
  result <- geocode(as.character(kits.ready$Address[i]), output = "latlona", source = "google")
  
  while(is.na(result[1])){ #checks if the latitude is NA and reruns if it is
    Sys.sleep(2) #Pauses for a minute to let the API Catch up
    result <- geocode(as.character(kits.ready$Address[i]), output = "latlona", source = "google")
  } 
  kits.ready$lon[i] <- as.numeric(result[1])
  kits.ready$lat[i] <- as.numeric(result[2])
  kits.ready$geoAddress[i] <- as.character(result[3])
}
head(kits.ready)

kits.ready <- apply(kits.ready,2,as.character)

write.csv(kits.ready, 'kits_ready.csv', na="-")

final.kits <- read.csv('kits_ready.csv')

sheet_write(final.kits, ss = "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo", sheet = "Combined + Geocoded")

install.packages("maptools")