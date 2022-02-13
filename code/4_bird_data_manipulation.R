#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(readxl)
require(tidyverse)
require(dplyr)
require(lubridate)
require(doBy)
library(sf)


#------------------------------------------------#
####       Read in Required Bird Data         ####
#------------------------------------------------#

#Read in modern bird data
bird.mod.ALL <- read.delim('data/ebd_US-ME-009_relDec-2021.txt', header = TRUE)
bird.modinat.ALL <- get_inat_obs(taxon_name = "Aves",  quality = 'research', place_id = 174940, maxresults = 10000)

#Read in historic bird data
bird.his.ALL <- read_excel('data/csbirds_rawdata_readin.xlsx')

#Read in shapefile for filtering data
loc.circle = read_sf("data/MDI_Circle.shp")



#------------------------------------------------#
####    Manipulation of Modern eBird Data     ####
#------------------------------------------------#

##Cleaning and subsetting
#Select the columns of interest for our purposes, and rename column headers to fit our conventions
bird.mod <- bird.mod.ALL[c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'STATE.CODE', 'COUNTY', 
                           'LOCALITY', 'LATITUDE', 'LONGITUDE')] %>% 
  rename('ch.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 'scientific.name'='SCIENTIFIC.NAME', 
         'state.code'='STATE.CODE', 'county'='COUNTY', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

#Change the date from chr to ymd format to easily subset data by years
bird.mod$date <- ymd(bird.mod$ch.date)

#Subset for year 2021, delete the old date column, and reorder columns
bird.mod.v2 <- subset(bird.mod, format(as.Date(date),"%Y")==2021 | format(as.Date(date),"%Y")==2020 |
                          format(as.Date(date),"%Y")==2019 | format(as.Date(date),"%Y")==2018) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())

#Subset for only months july, august, and first week of sept
bird.mod.v2[c('year', 'month', 'day')] <- str_split_fixed(bird.mod.v2$dat, '-', 3)
bird.mod.v3 <- subset(bird.mod.v2, month == "07" | month == "08" | month == "09")
bird.mod.v4 <- subset(bird.mod.v3, !(month == "09" & day > "07"))
 
#Take the Hancock county wide data and narrow to our defined geographic study area
pnts_sf <- st_as_sf(bird.mod.v4, coords = c('longitude', 'latitude'), crs = st_crs(loc.circle))

bird.mod.v5 <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, loc.circle))
  , area = if_else(is.na(intersection), '', loc.circle$Name[intersection])
) 

#An alternative way to grab point within bbox; not as specific but not the worst
#bird.mod.v4 <- bird.mod.v3[bird.mod.v3$latitude < 44.49284 & bird.mod.v3$latitude > 44.16919 & 
#                            bird.mod.v3$longitude < -68.14983 & bird.mod.v3$longitude > -68.47352,]

#filter by those records inside polygon
bird.mod.v5 <- filter(bird.mod.v5, intersection==1)

bird.mod.v6 <- bird.mod.v5 %>% 
  dplyr::select(-c('intersection', 'area', 'state.code', 'county', 'year', 'month', 'day'))



#------------------------------------------------#
####      Manipulation of iNat Bird Data      ####
#------------------------------------------------#

#Take the raw inat bird data and filter out what we don't need
bird.inat <- bird.modinat.ALL %>% 
  dplyr::select(c('common_name', 'scientific_name', 'datetime', 'place_guess', 'latitude', 'longitude')) %>% 
  rename('common.name'='common_name', 'scientific.name'='scientific_name', 'locality'='place_guess') %>% 
  subset(format(as.Date(datetime),"%Y")==2021 | format(as.Date(datetime),"%Y")==2020 |
           format(as.Date(datetime),"%Y")==2019 | format(as.Date(datetime),"%Y")==2018)

#Filter by the correct months and reorganize
bird.inat[c('date', 'time')] <- str_split_fixed(bird.inat$datetime, ' ', 2)
bird.inat[c('year', 'month', 'day')] <- str_split_fixed(bird.inat$date, '-', 3)
bird.inat.v2 <- subset(bird.inat, month == "07" | month == "08" | month == "09")
bird.inat.v2 <- subset(bird.inat.v2, !(month == "09" & day > "07")) %>% 
  dplyr::select(c('common.name', 'scientific.name', 'date', 'locality', 'latitude', 'longitude'))

#Filter by LOC polygon
pnts_sf2 <- st_as_sf(bird.inat.v2, coords = c('longitude', 'latitude'), crs = st_crs(loc.circle))

bird.inat.v3 <- pnts_sf2 %>% mutate(
  intersection = as.integer(st_intersects(geometry, loc.circle))
  , area = if_else(is.na(intersection), '', loc.circle$Name[intersection])
) 

#Clean up the datasheet
bird.inat.v4 <- bird.inat.v3 %>% 
  dplyr::select(-c('intersection', 'area'))

#Merge the two location data together
bird.mod.locs <- rbind(bird.inat.v4, bird.mod.v6)
bird


#------------------------------------------------#
####   Creation of Modern Bird Species List   ####
#------------------------------------------------#

##Species list
#Create a species list from this MDI dataset
bm.species.list <- bird.mod.locs[!duplicated(bird.mod.locs$scientific.name), ] %>%
  dplyr::select(c('common.name', 'scientific.name'))

#Remove non species level taxa and any domestics
bm.species.list2 <- bm.species.list[!grepl(" sp.", bm.species.list$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("/", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("(hybrid)", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("(Domestic type)", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Helmeted Guineafowl", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Chukar", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Large White-headed Gulls", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("American Black Duck × Mallard", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Northern Slate-colored Junco", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Myrtle Warbler", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Feral Pigeon", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Domestic Guineafowl", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("Brown Thrushes and Nightingale-Thrushes", bm.species.list2$common.name),]
bm.species.list2 <- bm.species.list2[!grepl("American Herring Gull", bm.species.list2$common.name),]


#Create a table of frequencies by species and merge onto the 
bm.freq <- as.data.frame(table(bird.mod.locs$common.name)) %>% 
  rename('common.name'='Var1')
bm.species.list4 <- merge(bm.freq, bm.species.list2, by = "common.name")

#Now take the species list and add a column denoting species commonness
bm.species.list4$frequency <- ifelse(bm.species.list4$"Freq">50, "common", "uncommon")
bm.species.list4['frequency'][bm.species.list4["Freq"] <20] <- "rare"
bm.species.list4['frequency'][bm.species.list4["Freq"] <5] <- "migrant only"

#Reorder for ease of view
bm.species.list5 <- bm.species.list4[c(1,3,2,5)]

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
                     bm.species.list5$scientific.name=="Tringa flavipes" |
                     bm.species.list5$scientific.name=="Calidris fuscicollis" |
                     bm.species.list5$scientific.name=="Tringa solitaria")
  return(whatrow)
}


#Append those species' status to migrant only
bm.species.list5$frequency <- replace(bm.species.list5$frequency, mig.ebird(), 'migrant only')

##Final output
bm.species.list.final <- bm.species.list5 %>% 
  dplyr::select(-('Freq'))



#------------------------------------------------#
####      Manipulation of iNat Bird Data      ####
#------------------------------------------------#

#Take the raw inat bird data and filter out what we don't need
bird.inat <- bird.modinat.ALL %>% 
  dplyr::select(c('common_name', 'scientific_name', 'datetime', 'place_guess', 'latitude', 'longitude')) %>% 
  rename('common.name'='common_name', 'scientific.name'='scientific_name', 'locality'='place_guess') %>% 
  subset(format(as.Date(datetime),"%Y")==2021 | format(as.Date(datetime),"%Y")==2020 |
           format(as.Date(datetime),"%Y")==2019 | format(as.Date(datetime),"%Y")==2018)

#Filter by the correct months and reorganize
bird.inat[c('date', 'time')] <- str_split_fixed(bird.inat$datetime, ' ', 2)
bird.inat[c('year', 'month', 'day')] <- str_split_fixed(bird.inat$date, '-', 3)
bird.inat.v2 <- subset(bird.inat, month == "07" | month == "08" | month == "09")
bird.inat.v2 <- subset(bird.inat.v2, !(month == "09" & day > "07")) %>% 
  dplyr::select(c('common.name', 'scientific.name', 'date', 'locality', 'latitude', 'longitude'))

#Filter by LOC polygon
pnts_sf2 <- st_as_sf(bird.inat.v2, coords = c('longitude', 'latitude'), crs = st_crs(loc.circle))

bird.inat.v3 <- pnts_sf2 %>% mutate(
  intersection = as.integer(st_intersects(geometry, loc.circle))
  , area = if_else(is.na(intersection), '', loc.circle$Name[intersection])
) 

#Clean up the datasheet
bird.inat.v4 <- bird.inat.v3 %>% 
  dplyr::select(-c('intersection', 'area'))

#Merge the two location data together
bird.mod.locs <- rbind(bird.inat.v4, bird.mod.v6)
bird.mod.locs[c('long','lat')] <- str_split_fixed(bird.mod.locs$geometry, ', ', 2)

bird.mod.locs <- data.frame(lapply(bird.mod.locs,c))

bird.mod.locs$long <- bird.mod.locs %>%
  dplyr::select(long) %>%
  mutate(long = str_replace(long, '^\\w\\(', ''))

bird.mod.locs <- data.frame(lapply(bird.mod.locs,c))

bird.mod.locs$lat <- bird.mod.locs %>%
  dplyr::select(lat) %>%
  mutate(lat = str_replace(lat, '\\)$', ''))

bird.mod.locs <- data.frame(lapply(bird.mod.locs,c))

bird.mod.locs <- bird.mod.locs %>% 
  dplyr::select('common.name','scientific.name','date','locality','long','lat') %>% 
  rename('longitude'='long', 'latitude'='lat')



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
bird.his.v2['scientific.name'][bird.his.v2['scientific.name'] == "Circus hudsonicus"] <- 'Circus hudsonius'
bird.his.v2['scientific.name'][bird.his.v2['scientific.name'] == "Oceanodroma leucorhoa"] <- 'Hydrobates leucorhous'
bird.his.v2['scientific.name'][bird.his.v2['scientific.name'] == "Picoides pubescens"] <- 'Dryobates pubescens'
bird.his.v2['scientific.name'][bird.his.v2['common.name'] == "hairy woodpecker"] <- 'Dryobates villosus'




###Making a qualitative frequency column
#Take the species list and add a column for frequency and remove Wild Turkey and Wood Thrush
bh.species.list <- bird.his.v2[-c(93, 94, 97),]
his.com.not <- bh.species.list[, c(1:2)]

#Add in the commonness column starting with common listed for all nows
his.com.not <- cbind(his.com.not, data.frame(rep(c('common'),times=98))) 
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
                     his.com.not$scientific.name=="Ectopistes migratorius" | 
                     his.com.not$scientific.name=="Sayornis phoebe" | 
                     his.com.not$scientific.name=="Podilymbus podiceps" | 
                     his.com.not$scientific.name=="Progne subis" | 
                     his.com.not$scientific.name=="Loxia curvirostra" | 
                     his.com.not$scientific.name=="Buteo jamaicensis" | 
                     his.com.not$scientific.name=="Aythya collaris" | 
                     his.com.not$scientific.name=="Pooecetes gramineus" | 
                     his.com.not$scientific.name=="Antrostomus vociferus" | 
                     his.com.not$scientific.name=="Loxia leucoptera" | 
                     his.com.not$scientific.name=="Hydrobates leucorhous" |
                     his.com.not$scientific.name=="Circus hudsonius" |
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
  bird <- basename(list.files(path = 'data/', pattern = 'csbirds_processed_2'))
  return(tail(bird, 1))
}

##Automate the newest file name output for processed ebird data
bird.mod.proc <- function(x) {
  bird <- basename(list.files(path = 'data/', pattern = 'ebird_processed_2'))
  return(tail(bird, 1))
}

##Automate the newest file name output for bird locations data
bird.mod.locs <- function(x) {
  bird <- basename(list.files(path = 'data/', pattern = 'ebird_mappingloc_2'))
  return(tail(bird, 1))
}

###File exporting
##Write out modern bird data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(bm.species.list.final, paste('data/ebird_processed_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', bird.mod.proc()), path = as_id(drive.output))

##Write out modern bird data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(bird.mod.locs, paste('data/ebird_mappingloc_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', bird.mod.locs()), path = as_id(drive.output))

##Write out historic bird data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(bh.species.list.final, paste('data/csbirds_processed_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', bird.his.proc()), path = as_id(drive.output))

