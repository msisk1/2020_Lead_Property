library(googlesheets4)
sheet.gid <- "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo"


kits.combined <- read_sheet(sheet.gid, sheet = "Combined")

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library("ggmap")

register_google(key = "___")


for(i in 1:nrow(kits.combined)){
  result <- geocode(as.character(kits.combined$Address[i]), output = "latlona", source = "google")
  while(is.na(result[1])){ #checks if the latitude is NA and reruns if it is
    Sys.sleep(2) #Pauses for a minute to let the API Catch up
    result <- geocode(as.character(kits.combined$Address[i]), output = "latlona", source = "google")
  } 
  kits.combined$lon[i] <- as.numeric(result[1])
  kits.combined$lat[i] <- as.numeric(result[2])
  kits.combined$geoAddress[i] <- as.character(result[3])
}
head(kits.combined)

kits.combined[is.na(kits.combined)] <- "N/A"

kits.combined <- apply(kits.combined,2,as.character)

write.csv(kits.combined, '/Users/patrick.mcguire/Downloads/kits_combined.csv')

final.kits <- read.csv('/Users/patrick.mcguire/Downloads/kits_combined.csv')

sheet_write(final.kits, ss = "1gzDR0CGf-kYddJZS7koD1iYwQQNKSiS1xdQ7AB0rxSo", sheet = "Combined + Geocoded")

install.packages("maptools")

