#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(dplyr)
require(lubridate)


#------------------------------------------------#
####    Manipulation of Modern Insect Data    ####
#------------------------------------------------#

###Cleaning and organizing
##Apidae
#Gather columns of interest and rename columns
api.mod <- api.mod.ALL[,c('latitude', 'longitude', 'id', 'scientific_name', 'common_name', 'place_guess',
                        'observed_on')] %>%
  rename('scientific.name'='scientific_name', 'common.name'='common_name', 'locality'='place_guess',
         'date'='observed_on') %>% 
  dplyr::select('common.name','scientific.name','date','locality')

#Fix dates
api.mod[c('year', 'trash')] <- str_split_fixed(api.mod$date, '-', 2)

#Clean up after date manipulation
api.mod.v2 <- dplyr::select(api.mod, -c('date','trash')) %>% 
  dplyr::select('common.name','scientific.name','year','locality')

#Add column for taxa
api.rn <- nrow(api.mod.v2)
api.taxa <- as.data.frame(rep("Apidae", times = api.rn))

#Cbind together
api.mod.v3 <- cbind(api.mod.v2, api.taxa) %>% 
  rename('taxonomy'='rep(\"Apidae\", times = api.rn)') %>% 
  dplyr::select('common.name','scientific.name','taxonomy','year','locality')


##Lepidoptera
#Gather columns of interest and rename columns
lep.mod <- lep.mod.ALL[,c('latitude', 'longitude', 'id', 'scientific_name', 'common_name', 'place_guess',
                          'observed_on')] %>%
  rename('scientific.name'='scientific_name', 'common.name'='common_name', 'locality'='place_guess',
         'date'='observed_on') %>% 
  dplyr::select('common.name','scientific.name','date','locality')

#Fix dates
lep.mod[c('year', 'trash')] <- str_split_fixed(lep.mod$date, '-', 2)

#Clean up after date manipulation
lep.mod.v2 <- dplyr::select(lep.mod, -c('date','trash')) %>% 
  dplyr::select('common.name','scientific.name','year','locality')

#Add column for taxa
lep.rn <- nrow(lep.mod.v2)
lep.taxa <- as.data.frame(rep("Lepidoptera", times = lep.rn))

#Cbind together
lep.mod.v3 <- cbind(lep.mod.v2, lep.taxa) %>% 
  rename('taxonomy'='rep(\"Lepidoptera\", times = lep.rn)') %>% 
  dplyr::select('common.name','scientific.name','taxonomy','year','locality')


##Create one data sheet with all our modern insect data
#Rbind these two dataframes
insect.mod <- rbind(lep.mod.v3, api.mod.v3)

##Species list
#Create a species list from this dataframe
im.species.list <- insect.mod[!duplicated(insect.mod$scientific.name), ]



#------------------------------------------------#
####   Manipulation of Historic Insect Data   ####
#------------------------------------------------#

#Gather columns of interest, rename, and reorder for both data sets
lep.his <- lep.his.ALL[,c('Class', 'ORDER', 'Super Family', 'FAMILY', 'Species Name & authority', 
                          'Synonymy/Other Names', 'Locations', 'Notes', 'ALL VOUCHERS')] %>% 
  rename('class'='Class', 'order'='ORDER', 'super.family'='Super Family', 'family'='FAMILY',
         'scientific.name'='Species Name & authority', 'name.synonyms'='Synonymy/Other Names', 
         'locality'='Locations', 'notes'='Notes', 'vouchers'='ALL VOUCHERS')

api.his <- api.his.ALL[,c('Class', 'ORDER', 'Super Family', 'FAMILY', 'Species Name & authority', 
                          'Synonymy/Other Names', 'Locations', 'Notes', 'ALL VOUCHERS')] %>% 
  rename('class'='Class', 'order'='ORDER', 'super.family'='Super Family', 'family'='FAMILY',
         'scientific.name'='Species Name & authority', 'name.synonyms'='Synonymy/Other Names', 
         'locality'='Locations', 'notes'='Notes', 'vouchers'='ALL VOUCHERS')

#make this one data set of lepidoptera and apidae
insect.his.ALL <- rbind(lep.his, api.his)

#Split the species name and authority column to get species names by themselves
insect.his.ALL[c('genus', 'specific.epithet', 'trash')] <- str_split_fixed(insect.his.ALL$scientific.name, ' ', 3)
insect.his.ALL$scientific.name = paste(insect.his.ALL$genus, insect.his.ALL$specific.epithet, sep=" ")

#Reoder and remove uneeded columns
insect.his <- dplyr::select(insect.his.ALL, -c('class', 'trash', 'notes', 'vouchers')) %>% 
  dplyr::select(c('order', 'super.family', 'family', 'genus', 'scientific.name', everything()))

#Create species list
ih.species.list <- subset(insect.his, insect.his$specific.epithet!="sp.")
ih.species.list2 <- ih.species.list[!duplicated(ih.species.list$scientific.name), ] %>% 
  dplyr::select(-c('specific.epithet'))



#------------------------------------------------#
####  Adding Taxonomy to Modern Insect Data   ####
#------------------------------------------------#

##Add taxonomy
#Split column to make a genus column
im.species.list[c('genus', 'trash')] <- str_split_fixed(im.species.list$scientific.name, ' ', 2)

#Merge with taxonomy from the historic dataset and clean
im.species.list2 <- merge(im.species.list, ih.species.list2, by = "scientific.name", all.x = TRUE) %>% 
  dplyr::select(-c('taxonomy','trash','genus.y','name.synonyms','locality.y')) %>% 
  dplyr::select('order','super.family','family','genus.x','scientific.name', everything()) %>% 
  rename('genus'='genus.x', 'locality'='locality.x')

#Fill in the order, super family, and family names for NAs from proctor
im.species.list2$order[is.na(im.species.list2$order)] <- im.species.list2$order[match(im.species.list2$genus,im.species.list2$genus)][which(is.na(im.species.list2$order))]
im.species.list2$super.family[is.na(im.species.list2$super.family)] <- im.species.list2$super.family[match(im.species.list2$genus,im.species.list2$genus)][which(is.na(im.species.list2$super.family))]
im.species.list2$family[is.na(im.species.list2$family)] <- im.species.list2$family[match(im.species.list2$genus,im.species.list2$genus)][which(is.na(im.species.list2$family))]

#Fill in the order, super family, and family names for the remainging NAs from the iNaturalist taxon download in Google Drive
im.species.list2$order[is.na(im.species.list2$order)] <- inat.taxon$taxon_order_name[match(im.species.list2$genus,inat.taxon$taxon_genus_name)][which(is.na(im.species.list2$order))]
im.species.list2$super.family[is.na(im.species.list2$super.family)] <- inat.taxon$taxon_superfamily_name[match(im.species.list2$genus,inat.taxon$taxon_genus_name)][which(is.na(im.species.list2$super.family))]
im.species.list2$family[is.na(im.species.list2$family)] <- inat.taxon$taxon_family_name[match(im.species.list2$genus,inat.taxon$taxon_genus_name)][which(is.na(im.species.list2$family))]

#Remove a sneaky non-species level taxa
im.species.list3 <- im.species.list2[!(im.species.list2$scientific.name=="Crambinae"),]


#------------------------------------------------#
####     Writing Out Processed .csv Files     ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Create pathway for exported files
drive.output <- "https://drive.google.com/drive/u/5/folders/1t21kymVP3y3ghdh_7MOl1ax4uZUTZwW0"

##Automate the newest file name output for processed Proctor's insect bird data
insect.his.proc <- function(x) {
  insect <- basename(list.files(pattern = 'proctorinsect_processed')) 
  return(tail(insect, 1))
}

insect.mod.inat <- function(x) {
  insect <- basename(list.files(pattern = 'inatinsect_processed')) 
  return(tail(insect, 1))
}



##File exporting
#Change WD to put download data into the 'data' folder
setwd(paste0(getwd(), "/data"))

#Write out modern insect data as .csv and upload to google drive
write_csv(im.species.list, paste('inatinsect_processed_', filedate, '.csv', sep=''))
drive_upload(insect.mod.inat(), path = as_id(drive.output))

#Write out historic insect data as .csv and upload to google drive
write_csv(ih.species.list2, paste('proctorinsect_processed_', filedate, '.csv', sep=''))
drive_upload(insect.his.proc(), path = as_id(drive.output))

#Return working directory to main folder
wd <- getwd()
setwd(gsub("/data", "", wd))
