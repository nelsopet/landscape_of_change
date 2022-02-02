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

##Cleaning and subsetting
#Select the columns of interest for our purposes, and rename column headers to fit our conventions
bird.mod <- bird.mod.ALL[c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'STATE.CODE', 'COUNTY', 
                           'LOCALITY', 'LATITUDE', 'LONGITUDE')] %>% 
  rename('ch.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 'scientific.name'='SCIENTIFIC.NAME', 
         'state.code'='STATE.CODE', 'county'='COUNTY', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

#Change the date from chr to ymd format to easily subset data from only year 2021
bird.mod$date <- ymd(bird.mod$ch.date)

#Subset for year 2021, delete the old date column, and reorder columns
bird.mod.2021 <- subset(bird.mod, format(as.Date(date),"%Y")==2021) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())
bird.mod.2020 <- subset(bird.mod, format(as.Date(date),"%Y")==2020) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())
bird.mod.2019 <- subset(bird.mod, format(as.Date(date),"%Y")==2019) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())
bird.mod.2018 <- subset(bird.mod, format(as.Date(date),"%Y")==2018) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())

#Combine data from all 4 years
bird.mod.v2 <- rbind(bird.mod.2021, bird.mod.2020)
bird.mod.v2 <- rbind(bird.mod.v2, bird.mod.2019)
bird.mod.v2 <- rbind(bird.mod.v2, bird.mod.2018)

#Subset for only months july, august, and first week of sept
bird.mod.v2[c('year', 'month', 'day')] <- str_split_fixed(bird.mod.v2$dat, '-', 3)
bird.mod.07 <- subset(bird.mod.v2, month == "07")
bird.mod.08 <- subset(bird.mod.v2, month == "08")

#Combine new data frame
bird.mod.v3 <- rbind(bird.mod.07, bird.mod.08)

#Take the Hancock county wide data and narrow to our defined geographic study area
bird.mod.v4 <- bird.mod.v3[bird.mod.v3$latitude < 44.49284 & bird.mod.v3$latitude > 44.16919 & 
                            bird.mod.v3$longitude < -68.14983 & bird.mod.v3$longitude > -68.47352,]


##Species list
#Create a species list from this MDI dataset
bm.species.list <- bird.mod.v4[!duplicated(bird.mod.v4$scientific.name), ] %>%
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
bm.freq <- as.data.frame(table(bird.mod.v4$common.name)) %>% 
  rename('common.name'='Var1')
bm.species.list4 <- merge(bm.freq, bm.species.list3, by = "common.name")

#Now take the species list and add a column denoting species commonness
bm.species.list4$frequency <- ifelse(bm.species.list4$"Freq">50, "common", "uncommon")
bm.species.list4['frequency'][bm.species.list4["Freq"] <20] <- "rare"
bm.species.list4['frequency'][bm.species.list4["Freq"] <5] <- "migrant only"

#Reorder for ease of view
bm.species.list5 <- bm.species.list4[c(1,3,8,2,4,5,6,7)]

#Fix species that are marked incorrectly
#Create function to pull row numbers and make them rare
rare.ebird <- function(x) {
  whatrow <- which(bm.species.list5$scientific.name=="Bubo virginianus" |
                     bm.species.list5$scientific.name=="Canachites canadensis")
  return(whatrow)
}

#Append those species' status to rare
bm.species.list5$frequency <- replace(bm.species.list5$frequency, rare.ebird(), 'rare')


#Create function to pull row numbers and make then migrant only
mig.ebird <- function(x) {
  whatrow <- which(bm.species.list5$scientific.name=="Tringa melanoleuca" |
                     bm.species.list5$scientific.name=="Calidris minutilla" |
                     bm.species.list5$scientific.name=="Calidris alba" |
                     bm.species.list5$scientific.name=="Charadrius semipalmatus" |
                     bm.species.list5$scientific.name=="Numenius phaeopus" |  
                     bm.species.list5$scientific.name=="Arenaria interpres" |   
                     bm.species.list5$scientific.name=="Calidris pusilla" |  
                     bm.species.list5$scientific.name=="Limnodromus griseus" | 
                     bm.species.list5$scientific.name=="Pluvialis squatarola" |
                     bm.species.list5$scientific.name=="Phalaropus lobatus" |
                     bm.species.list5$scientific.name=="Spizella pusilla" |
                     bm.species.list5$scientific.name=="Tringa solitaria")
  return(whatrow)
}

#Append those species' status to migrant only
bm.species.list5$frequency <- replace(bm.species.list5$frequency, mig.ebird(), 'migrant only')


##Final output
#Split date column to just retain year
bm.species.list5[c('year', 'trash')] <- str_split_fixed(bm.species.list5$date, '-', 2)

#Remove columns and reorder
bm.species.list.final <- bm.species.list5[, -c(4,5,7,8,10)] %>% 
  dplyr::select('common.name', 'scientific.name', 'year', 'locality', 'frequency')



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
  whatrow <- which(his.com.not$scientific.name=="Strix varia" |
                     his.com.not$scientific.name=="Rissa tridactyla" |
                     his.com.not$scientific.name=="Chroicocephalus philadelphia" |
                     his.com.not$scientific.name=="Dolichonyx oryzivorus" |
                     his.com.not$scientific.name=="Gavia immer" |  
                     his.com.not$scientific.name=="Tyrannus tyrannus" |   
                     his.com.not$scientific.name=="Agelaius phoeniceus" |   
                     his.com.not$scientific.name=="Accipiter striatus")
  return(whatrow)
}

#Append those species' status to uncommon
his.com.not$frequency <- replace(his.com.not$frequency, uncommon.bird(), 'uncommon')


##Change frequency status for rare species
#Create function to pull row numbers
rare.bird <- function(x) {
  whatrow <- which(his.com.not$scientific.name=="Botaurus lentiginosus" |
                     his.com.not$scientific.name=="Melanitta americana" |
                     his.com.not$scientific.name=="Setophaga caerulescens" |
                     his.com.not$scientific.name=="Cyanocitta cristata" |
                     his.com.not$scientific.name=="Sialia sialis" |  
                     his.com.not$scientific.name=="Poecile hudsonicus" |   
                     his.com.not$scientific.name=="Certhia americana" | 
                     his.com.not$scientific.name=="Passerina cyanea" | 
                     his.com.not$scientific.name=="Parkesia noveboracensis" | 
                     his.com.not$scientific.name=="Sayornis phoebe" | 
                     his.com.not$scientific.name=="Podilymbus podiceps" | 
                     his.com.not$scientific.name=="Progne subis" | 
                     his.com.not$scientific.name=="Loxia curvirostra" | 
                     his.com.not$scientific.name=="Buteo jamaicensis" | 
                     his.com.not$scientific.name=="Aythya collaris" | 
                     his.com.not$scientific.name=="Pooecetes gramineus" | 
                     his.com.not$scientific.name=="Antrostomus vociferus" | 
                     his.com.not$scientific.name=="Loxia leucoptera" | 
                     his.com.not$scientific.name=="Cardellina pusilla")
  return(whatrow)
}

#Append those species' status to rare
his.com.not$frequency <- replace(his.com.not$frequency, rare.bird(), 'rare')


##Change frequency status for migrant species
#Create function to pull row numbers
mig.bird <- function(x) {
  whatrow <- which(his.com.not$scientific.name=="Tringa melanoleuca" |
                     his.com.not$scientific.name=="Calidris minutilla" |
                     his.com.not$scientific.name=="Tringa flavipes" |
                     his.com.not$scientific.name=="Clangula hyemalis" |
                     his.com.not$scientific.name=="Calidris melanotos" |  
                     his.com.not$scientific.name=="Arenaria interpres" |   
                     his.com.not$scientific.name=="Calidris pusilla" |  
                     his.com.not$scientific.name=="Limnodromus griseus" | 
                     his.com.not$scientific.name=="Tringa solitaria")
  return(whatrow)
}

#Append those species' status to migrant only
his.com.not$frequency <- replace(his.com.not$frequency, mig.bird(), 'migrant only')


#Merge this back with the bh.species.list
bh.species.list.final <- merge(his.com.not[,c(2:3)], bh.species.list, by='scientific.name') %>% 
  dplyr::select('common.name','scientific.name','year','cs.location','frequency', everything()) %>% 
  dplyr::select(-c("date",7:10)) %>% 
  rename('locality'='cs.location')


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

##Automate the newest file name output for processed ebird data
bird.mod.proc <- function(x) {
  bird <- basename(list.files(pattern = 'ebird_processed'))
  return(tail(bird, 1))
}


##File exporting
#Change WD to put download data into the 'data' folder
setwd(paste0(getwd(), "/data"))

##Write out modern bird data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(bm.species.list.final, paste('ebird_processed_', filedate, '.csv', sep=''))
#drive_upload(bird.mod.proc(), path = as_id(drive.output))

##Write out historic bird data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(bh.species.list.final, paste('csbirds_processed_', filedate, '.csv', sep=''))
#drive_upload(bird.his.proc(), path = as_id(drive.output))

#Return working directory to main folder
wd <- getwd()
setwd(gsub("/data", "", wd))

