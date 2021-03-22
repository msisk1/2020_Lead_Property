rm(list=ls(all=TRUE)) # clear memory

library(rvest)
library(sf)
library(tidyverse)



# url.example <- "http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_Indiana/tab_parcel_v0701.asp?t_nm=base&l_cr=10&t_wc=|parcelid=71-08-12-428-018.000-026&sid=9BB936AB3F0D4B4781D219C4A8BA5159"

parcelID.example01 <- "71-08-12-428-018.000-026" #this can be pulled from the parcel layers
parcelID.example02 <- "71-03-33-456-011.000-026"
parcelID.example03 <- "71-09-15-362-031.000-023"

sid = "0F49640AD03D4CCDBC37BE8A0423193D"  #this may be an identifer for my computer. If it dosn't work, a first debug step can be to goto a url in the browser, agree to the Terms, search for a parcel and copyu the sid at the end of the URL


#otherwise, there are two types of records that are easy to access:
# first is the overview page. It may have enough info for what we need:
url.overview <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=",
                       parcelID.example01,
              "&sid=",
              sid)

            
#There are also the record-level ones, I would use this if necessary
url.record <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_dwell_v0704.asp?bldnum=R01&tname=DWELL&printview=true&t_wc=|parcelid=",
                     parcelID.example01,
                     "&sid=",
                     sid)



each.html <- read_html(url.overview) #downloads the html file that contains the table of information
write_html(each.html,"file.html",encoding = "UTF-8") # this is what you would use to cache the html docs

full.parsed.tables <- each.html %>% 
  html_table(fill = T) #converts it into an R table, the fill makes it work for blank cells
each.imp.table <- as.data.frame(full.parsed.tables[13]) %>% #the 13 seems to pull the proper section for all I have done
  select(-c(X1,X2,X3))%>% #tossing some unneded values
  filter_all(any_vars(!is.na(.))) #removing empty rows
names(each.imp.table) <- each.imp.table[2,] renaming
  
each.imp.table <- data.frame(each.imp.table[3:nrow(each.imp.table),])  #throwing out unnecessary values


#this pulls the assessed ba
each.assessed.table <- as.data.frame(full.parsed.tables[11]) %>% #the 11 seems to pull the assessment
  select(-c(X2,X6,X7))%>% #tossing some unneded values
  filter_all(any_vars(!is.na(.))) #removing empty rows
assessed.value <- each.assessed.table[2,3]





###Here is my old code with a loop and things like that
rm(list=ls(all=TRUE)) # clear memory

library(rvest)
library(sf)
library(tidyverse)

#open parcels file
#parse for Housing Authority
#download the imporovement records

ha.parcels <- st_read("data\\HousingAuthority.shp", stringsAsFactors = F)


url <- "http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=71-03-33-456-011.000-026&sid=907653F1801347ABB466235B4BBB2EFC"


valid.codes <- c("Residential Dwelling","Single-Family")
ha.parcels$ConYear <- NA_integer_
all.codes <- valid.codes
out.folder <- "data\\Housing\\SJ_Cards"
redo.dates <- FALSE
use.saved.downloads <- TRUE

full <- data.frame(`Improvement Type CodeÂ`=character(),
                   `Improvement Type CodeÂ`=character(), 
                   PARCELID=character(), 
                   stringsAsFactors=FALSE) 



for(x in 1:nrow(ha.parcels)){
  if (redo.dates){
    ha.parcels[x,]$ConYear <- 0
  }
  #first test if website it null. Then test if date exists. then test if it is done
  print(x)
  
  if (is.na(ha.parcels[x,]$ConYear) ){
    file.name <- paste(out.folder, "\\",ha.parcels[x,]$PARCELID,".html",sep = "")
    if (file.exists(file.name) & use.saved.downloads){
      print("    read cached")
      each.html <- read_html(file.name, encoding = "UTF-8")
      
    }else{
      url <- paste0("http://in-stjoseph-assessor.governmax.com/propertymax/ACAMA_INDIANA/tab_improve_v0704.asp?t_nm=improvements&l_cr=5&t_wc=|parcelid=",ha.parcels[x,]$PARCELSTAT,"&sid=907653F1801347ABB466235B4BBB2EFC")
      
      each.html <- read_html(url)
      write_html(each.html,file.name,encoding = "UTF-8")
      print("    downloaded")
    }
    
    each.con.table <- each.html %>% html_table(fill = T)
    oop <- as.data.frame(each.con.table[13])
    oop2 <- data.frame(oop[3:nrow(oop),])
    names(oop2) <- oop[2,]
    
    oop2 <- oop2 %>%
      select("Improvement Type CodeÂ", "Constructed YearÂ")%>%
      filter(`Improvement Type CodeÂ` != "<NA>")%>%
      mutate(PARCELID = ha.parcels[x,]$PARCELID,
             `Constructed YearÂ` = as.integer(`Constructed YearÂ`))
    
    full <- bind_rows(full, oop2)    
    # each.con.table <- each.html %>% html_table(fill = T)
    # each.con.table <- data.frame(each.con.table[10])
    # names(each.con.table) <- each.con.table[1,]
    # all.codes <- unique(c(all.codes, each.con.table$Buildings))
    # year <- as.numeric(each.con.table[each.con.table$Buildings %in% valid.codes ,"Construction Year"])
    # if (length(year)>0){
    #   elkhart.parcels[x,]$ConYear <- min(year)
    # }else{
    #   elkhart.parcels[x,]$ConYear <- -999
    # }
    # remove(year)  
  } #end major if
}#end for loop
save.image("inprog.RData")


full2 <- full %>%
  filter(`Constructed YearÂ` >0)%>%
  group_by(PARCELID)%>%
  summarise(EarliestYear = min(`Constructed YearÂ`))

to.out <- left_join(ha.parcels, full2, by = c("PARCELID" = "PARCELID"))
st_write(to.out, "PublicHousing.shp")


each.html <- read_html(url)
each.con.table <- each.html %>% html_table(fill = T)
oop <- as.data.frame(each.con.table[13])
oop2 <- data.frame(oop[3:nrow(oop),])
names(oop2) <- oop[2,]
oop2<- select

