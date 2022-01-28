#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####     Manipulation of Modern Bird Data     ####
#------------------------------------------------#

#Select the columns of interest for our purposes, and rename column headers to fit our conventions
bird.mod <- bird.mod.ALL[c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'STATE.CODE', 'COUNTY', 
                           'LOCALITY', 'LATITUDE', 'LONGITUDE')] %>% 
  rename('ch.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 'current.scientific.name'='SCIENTIFIC.NAME', 
         'state.code'='STATE.CODE', 'county'='COUNTY', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

#Change the date from chr to ymd format to easily subset data from only year 2021
bird.mod$date <- ymd(bird.mod$ch.date)
#Subset for year 2021, delete the old date column, and reorder columns
bird.mod.v2 <- subset(bird.mod, format(as.Date(date),"%Y")==2021) %>% 
  select(-'ch.date') %>% 
  select('common.name', 'current.scientific.name', 'date', everything())

#Take the Hancock county wide data and narrow to our defined geographic study area


#Create a species list from this MDI dataset
#bm.species.list <- bird.mod.v?[!duplicated(bird.mod.v?$current.scientific.name), ] %>%
#                  select(c('common.name', 'current.scientific.name', 'date', 'locality', 'latitude',
#                 'longitude'))

#Take the species list and add a column denoting species commonness
#mod.com.not <- as.data.frame(bm.species.list$common.name) %>% 
#               rename('common.name'='bm.species.list$common.name')


#Merge this new column onto our species list



#------------------------------------------------#
####    Manipulation of Historic Bird Data    ####
#------------------------------------------------#

#Rename column headers to appropriate titles and remove columns of no use
bird.his <- bird.his.ALL[, -c(4,6,8,13:21)] %>%
  rename_all(make.names) %>% 
  rename('year'='YEAR', 'date'='DATE', 'CS.location'='CS.Location', 
         'CS.list.notes.1883'='CS.list.notes..1883.', 
         'TB.narrative.1941'='Tyson...Bond.1941.Narrative', 
         'TB.list.1941'='Tyson...Bond.1941.List')

#Alter the common names to all be lowercase for ease of use
correct.bird <- as.data.frame(tolower(bird.his$common.name)) %>% 
  rename('COMMON.NAME'='tolower(bird.his$common.name)')

#Rebind the new common name column and reorder
bird.his.v2 <- cbind(correct.bird, bird.his) %>% 
  select(-c('common.name')) %>% 
  select('COMMON.NAME', 'current.scientific.name', 'year', 'date', everything()) %>% 
  rename('common.name'='COMMON.NAME') 

#Fix the lack of scientific name for alder flycatcher
bird.his.v2['current.scientific.name'][bird.his.v2['common.name'] == "alder flycatcher"] <- 'Empidonax alnorum'
#Fix scientific name of Osprey
bird.his.v2['current.scientific.name'][bird.his.v2['current.scientific.name'] == "Pandion cristatus"] <- 'Pandion haliaetus'
bird.his.v2['current.scientific.name'][bird.his.v2['common.name'] == "least sandpiper"] <- 'Calidris minutilla'

#Make species list just for consistency
bh.species.list <- bird.his.v2[-c(92,93,96),]



###Making a qualitative frequency column
#Take the species list and add a column for frequency and remove Wild Turkey and Wood Thrush
his.com.not <- bh.species.list[, c(1:2)]

#Add in the commonness column starting with common listed for all nows
his.com.not <- cbind(his.com.not, data.frame(rep(c('common'),times=97))) 
colnames(his.com.not) <- c('common.name', 'current.scientific.name', 'frequency')

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
bh.species.list <- merge(his.com.not[,c(2:3)], bh.species.list, by='current.scientific.name') %>% 
  select('common.name','current.scientific.name','year','date','CS.location','frequency', everything()) %>% 
  select(-c(7:10))



#------------------------------------------------#
####     Writing Out Processed .csv Files     ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Automate the newest file name output for processed Champlain Society bird data
bird.his.proc <- function(x) {
  bird <- basename(list.files(pattern = 'csbirds_processed'))
  return(tail(bird, 1))
}


##Create pathway for exported files
drive.output <- "https://drive.google.com/drive/u/5/folders/1t21kymVP3y3ghdh_7MOl1ax4uZUTZwW0"


##File exporting
#Write out modern bird data as .csv and upload to google drive


#Write out historic bird data as .csv and upload to google drive
write_csv(bh.species.list, paste('csbirds_processed_', filedate, '.csv', sep=''))
drive_upload(bird.his.proc(), path = as_id(drive.output))


