
library(XML)
library(RCurl)
library(rlist)

#create a list of parcelID
#write code to iterate through the list and pull the records
#use example below to get the improvement data
#if no record, throw it out (vacant lot): probably worth flagging to check if the parcel ID is correct
#check for 0000

# each.html <- read_html(url.overview) #downloads the html file that contains the table of information
# write_html(each.html,"file.html",encoding = "UTF-8") # this is what you would use to cache the html docs
#new try
parcelID.example01 <- "71-08-12-428-018.000-026" #this can be pulled from the parcel layers
parcelID.example01 <- "71-03-33-456-011.000-026"
parcelID.example01 <- "71-08-01-108-016.000-026"

url.overview <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=",
                       parcelID.example01,
                       "&sid=",
                       sid)
theurl <- getURL(url.overview,.opts = list(ssl.verifypeer = FALSE) )
#save them as a cache
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
q <- as.data.frame(tables[2])
library(tidyverse)
q <- q[,4:ncol(q)] %>% na.omit()
var.names <- c("Improvement Type Code", 	"Building No.", 	"ID No.", 	"Constructed Year", "Grade" 	,"Total Base Area","Replacement Cost")
names(q)<-var.names
q