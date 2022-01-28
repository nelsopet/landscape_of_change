#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####    Manipulation of Modern Insect Data    ####
#------------------------------------------------#

#Gather columns of interest and only pull insect species data from 2021
insect.mod <- insect.mod.ALL[,c('class', 'order', 'family', 'genus', 'specificEpithet',
                                'verbatimScientificName', 'taxonRank', 'year', 'month', 'day',
                                'verbatimLocality', 'decimalLatitude', 'decimalLongitude',
                                'coordinateUncertaintyInMeters')] %>%
  subset(year==2021 & class=='Insecta' & taxonRank=='SPECIES') %>% 
  rename('specific.epithet'='specificEpithet', 'current.scientific.name'='verbatimScientificName', 
         'taxon.rank'='taxonRank', 'location'='verbatimLocality', 'latitude'='decimalLatitude',
         'longitude'='decimalLongitude', 'coord.uncert.m'='coordinateUncertaintyInMeters')

#Combine the separated date columns into one
insect.mod$date = paste(insect.mod$year, insect.mod$month, insect.mod$day, sep="-")

#Subset dataframe to remove points of locality with error >5 miles, and grab only Lepidoptera and Apidae
insect.mod.v2 <- select(insect.mod, c('order', 'family', 'genus', 'specific.epithet', 'current.scientific.name', 
                                      'date', 'location', 'latitude', 'longitude', 'coord.uncert.m')) %>% 
  subset(coord.uncert.m<8047) %>% 
  subset(order=="Lepidoptera" | family=="Apidae")

unique(insect.mod.v2$current.scientific.name)
unique(ih.species.list$current.scientific.name)

#Take the Hancock county wide data and narrow to our defined geographic study area


#Create a species list from this dataframe
#im.species.list <- insect.mod[!duplicated(insect.mod$current.scientific.name), ]



#------------------------------------------------#
####   Manipulation of Historic Insect Data   ####
#------------------------------------------------#

#Gather columns of interest, rename, and reorder for both data sets
lep.his <- lep.his.ALL[,c('Class', 'ORDER', 'Super Family', 'FAMILY', 'Species Name & authority', 
                          'Synonymy/Other Names', 'Locations', 'Notes', 'ALL VOUCHERS')] %>% 
  rename('class'='Class', 'order'='ORDER', 'super.family'='Super Family', 'family'='FAMILY',
         'scientific.name'='Species Name & authority', 'name.synonyms'='Synonymy/Other Names', 
         'location'='Locations', 'notes'='Notes', 'vouchers'='ALL VOUCHERS')

api.his <- api.his.ALL[,c('Class', 'ORDER', 'Super Family', 'FAMILY', 'Species Name & authority', 
                          'Synonymy/Other Names', 'Locations', 'Notes', 'ALL VOUCHERS')] %>% 
  rename('class'='Class', 'order'='ORDER', 'super.family'='Super Family', 'family'='FAMILY',
         'scientific.name'='Species Name & authority', 'name.synonyms'='Synonymy/Other Names', 
         'location'='Locations', 'notes'='Notes', 'vouchers'='ALL VOUCHERS')

#make this one data set of lepidoptera and apidae
insect.his.ALL <- rbind(lep.his, api.his)

#Split the species name and authority column to get species names by themselves
insect.his.ALL[c('genus', 'specific.epithet', 'trash')] <- str_split_fixed(insect.his.ALL$scientific.name, ' ', 3)
insect.his.ALL$current.scientific.name = paste(insect.his.ALL$genus, insect.his.ALL$specific.epithet, sep=" ")

#Reoder and remove uneeded columns
insect.his <- select(insect.his.ALL, -c('class', 'scientific.name', 'trash')) %>% 
  select(c('order', 'super.family', 'family', 'genus', 'specific.epithet',
           'current.scientific.name', everything()))

ih.species.list <- subset(insect.his, insect.his$specific.epithet!="sp.")




#------------------------------------------------#
####     Writing Out Processed .csv Files     ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Automate the newest file name output for processed Proctor's insect bird data
insect.his.proc <- function(x) {
  insect <- basename(list.files(pattern = 'proctorinsect_processed')) 
  return(tail(insect, 1))
}

##Create pathway for exported files
drive.output <- "https://drive.google.com/drive/u/5/folders/1t21kymVP3y3ghdh_7MOl1ax4uZUTZwW0"

##File exporting
#Write out modern insect data as .csv and upload to google drive


#Write out historic insect data as .csv and upload to google drive
write_csv(ih.species.list, paste('proctorinsect_processed_', filedate, '.csv', sep=''))
drive_upload(insect.his.proc(), path = as_id(drive.output))


