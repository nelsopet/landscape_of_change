#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(dplyr)
require(lubridate)
require(doBy)


#------------------------------------------------#
####     Manipulation of Modern Bird Data     ####
#------------------------------------------------#

#Select the columns of interest for our purposes, and rename column headers to fit our conventions
bird.mod <- bird.mod.ALL[c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'STATE.CODE', 'COUNTY', 
                           'LOCALITY', 'LATITUDE', 'LONGITUDE')] %>% 
  rename('ch.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 'scientific.name'='SCIENTIFIC.NAME', 
         'state.code'='STATE.CODE', 'county'='COUNTY', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

#Change the date from chr to ymd format to easily subset data from only year 2021
bird.mod$date <- ymd(bird.mod$ch.date)
#Subset for year 2021, delete the old date column, and reorder columns
bird.mod.v2 <- subset(bird.mod, format(as.Date(date),"%Y")==2021) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())

#Take the Hancock county wide data and narrow to our defined geographic study area


#Create a species list from this MDI dataset
bm.species.list <- bird.mod.v2[!duplicated(bird.mod.v2$scientific.name), ] %>%
  dplyr::select(c('common.name', 'scientific.name', 'date', 'locality', 'latitude', 'longitude'))

#Remove non species level taxa and any domestics
bm.species.list2 <- bm.species.list[!grepl("sp.", bm.species.list$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("/", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("(hybrid)", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("(Domestic type)", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Helmeted Guineafowl", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Chukar", bm.species.list2$common.name),]

#Alphebetically order
bm.species.list3 <-  orderBy("common.name", bm.species.list2)


#Create a table of frequencies by species and merge onto the 
bm.freq <- as.data.frame(table(bird.mod.v2$common.name)) %>% 
  rename('common.name'='Var1')
bm.species.list4 <- merge(bm.freq, bm.species.list3, by = "common.name")

#Now take the species list and add a column denoting species commonness
bm.species.list4$frequency <- ifelse(bm.species.list4$"Freq">50, "common", "uncommon")
  


#------------------------------------------------#
####    Manipulation of Historic Bird Data    ####
#------------------------------------------------#

#Rename column headers to appropriate titles and remove columns of no use
bird.his <- bird.his.ALL[, -c(4,6,8,13:21)] %>%
  rename_all(make.names) %>% 
  rename('year'='YEAR', 'date'='DATE', 'scientific.name'='current.scientific.name', 
         'cs.location'='CS.Location', 'cs.list.notes.1883'='CS.list.notes..1883.', 
         'tb.narrative.1941'='Tyson...Bond.1941.Narrative', 'tb.list.1941'='Tyson...Bond.1941.List')

#Alter the common names to all be lowercase for ease of use
correct.bird <- as.data.frame(tolower(bird.his$common.name)) %>% 
  rename('COMMON.NAME'='tolower(bird.his$common.name)')

#Rebind the new common name column and reorder
bird.his.v2 <- cbind(correct.bird, bird.his) %>% 
  dplyr::select(-c('common.name')) %>% 
  dplyr::select('COMMON.NAME', 'scientific.name', 'year', 'date', everything()) %>% 
  rename('common.name'='COMMON.NAME') 

#Fix the lack of scientific name for alder flycatcher
bird.his.v2['scientific.name'][bird.his.v2['common.name'] == "alder flycatcher"] <- 'Empidonax alnorum'
#Fix scientific name of Osprey
bird.his.v2['scientific.name'][bird.his.v2['scientific.name'] == "Pandion cristatus"] <- 'Pandion haliaetus'
bird.his.v2['scientific.name'][bird.his.v2['common.name'] == "least sandpiper"] <- 'Calidris minutilla'

#Make species list just for consistency
bh.species.list <- bird.his.v2[-c(92,93,96),]



###Making a qualitative frequency column
#Take the species list and add a column for frequency and remove Wild Turkey and Wood Thrush
his.com.not <- bh.species.list[, c(1:2)]

#Add in the commonness column starting with common listed for all nows
his.com.not <- cbind(his.com.not, data.frame(rep(c('common'),times=97))) 
colnames(his.com.not) <- c('common.name', 'scientific.name', 'frequency')

##Change frequency status for uncommon species
#Create function to pull row numbers
uncommon.bird <- function(x) {
  whatrow <- which(his.com.not$current.scientific.name=="Strix varia" |
                     his.com.not$current.scientific.name=="Rissa tridactyla" |
                     his.com.not$current.scientific.name=="Chroicocephalus philadelphia" |
                     his.com.not$current.scientific.name=="Dolichonyx oryzivorus" |
                     his.com.not$current.scientific.name=="Gavia immer" |  
                     his.com.not$current.scientific.name=="Tyrannus tyrannus" |   
                     his.com.not$current.scientific.name=="Agelaius phoeniceus" |   
                     his.com.not$current.scientific.name=="Accipiter striatus")
  return(whatrow)
}

#Append those species' status to uncommon
his.com.not$frequency <- replace(his.com.not$frequency, uncommon.bird(), 'uncommon')


##Change frequency status for rare species
#Create function to pull row numbers
rare.bird <- function(x) {
  whatrow <- which(his.com.not$current.scientific.name=="Botaurus lentiginosus" |
                     his.com.not$current.scientific.name=="Melanitta americana" |
                     his.com.not$current.scientific.name=="Setophaga caerulescens" |
                     his.com.not$current.scientific.name=="Cyanocitta cristata" |
                     his.com.not$current.scientific.name=="Sialia sialis" |  
                     his.com.not$current.scientific.name=="Poecile hudsonicus" |   
                     his.com.not$current.scientific.name=="Certhia americana" | 
                     his.com.not$current.scientific.name=="Passerina cyanea" | 
                     his.com.not$current.scientific.name=="Parkesia noveboracensis" | 
                     his.com.not$current.scientific.name=="Sayornis phoebe" | 
                     his.com.not$current.scientific.name=="Podilymbus podiceps" | 
                     his.com.not$current.scientific.name=="Progne subis" | 
                     his.com.not$current.scientific.name=="Loxia curvirostra" | 
                     his.com.not$current.scientific.name=="Buteo jamaicensis" | 
                     his.com.not$current.scientific.name=="Aythya collaris" | 
                     his.com.not$current.scientific.name=="Pooecetes gramineus" | 
                     his.com.not$current.scientific.name=="Antrostomus vociferus" | 
                     his.com.not$current.scientific.name=="Loxia leucoptera" | 
                     his.com.not$current.scientific.name=="Cardellina pusilla")
  return(whatrow)
}

#Append those species' status to rare
his.com.not$frequency <- replace(his.com.not$frequency, rare.bird(), 'rare')


##Change frequency status for migrant species
#Create function to pull row numbers
mig.bird <- function(x) {
  whatrow <- which(his.com.not$current.scientific.name=="Tringa melanoleuca" |
                     his.com.not$current.scientific.name=="Calidris minutilla" |
                     his.com.not$current.scientific.name=="Tringa flavipes" |
                     his.com.not$current.scientific.name=="Clangula hyemalis" |
                     his.com.not$current.scientific.name=="Calidris melanotos" |  
                     his.com.not$current.scientific.name=="Arenaria interpres" |   
                     his.com.not$current.scientific.name=="Calidris pusilla" |  
                     his.com.not$current.scientific.name=="Limnodromus griseus" | 
                     his.com.not$current.scientific.name=="Tringa solitaria")
  return(whatrow)
}

#Append those species' status to migrant only
his.com.not$frequency <- replace(his.com.not$frequency, mig.bird(), 'migrant only')


#Merge this back with the bh.species.list
bh.species.list <- merge(his.com.not[,c(2:3)], bh.species.list, by='scientific.name') %>% 
  dplyr::select('common.name','scientific.name','year','date','cs.location','frequency', everything()) %>% 
  dplyr::select(-c(7:10))



#------------------------------------------------#
####     Writing Out Processed .csv Files     ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Create pathway for exported files
drive.output <- "https://drive.google.com/drive/u/5/folders/1t21kymVP3y3ghdh_7MOl1ax4uZUTZwW0"

##Automate the newest file name output for processed Champlain Society bird data
bird.his.proc <- function(x) {
  bird <- basename(list.files(pattern = 'csbirds_processed'))
  return(tail(bird, 1))
}


##File exporting
#Change WD to put download data into the 'data' folder
setwd(paste0(getwd(), "/data"))

#Write out modern bird data as .csv and upload to google drive


#Write out historic bird data as .csv and upload to google drive
write_csv(bh.species.list, paste('csbirds_processed_', filedate, '.csv', sep=''))
drive_upload(bird.his.proc(), path = as_id(drive.output))

#Return working directory to main folder
wd <- getwd()
wdr <- gsub("/data", "", wd)
setwd(wdr)

