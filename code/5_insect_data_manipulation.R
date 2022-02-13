#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(rinat)
require(tidyverse)
require(mapview)
require(readxl)
require(tidyverse)
require(dplyr)
require(lubridate)


#------------------------------------------------#
####       Read in Required Insect Data       ####
#------------------------------------------------#

##Read in the modern iNat data
#Apidae data
api.mod.ALL <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', place_id = 174940, maxresults = 10000)
#Lepidoptera data
lep.mod.ALL <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', place_id = 174940, maxresults = 10000)

##Read in the historical data
#Apidae data
api.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 2)
#Lepidoptera data
lep.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 1)

##Read in taxonomy data
inat.taxon <- read.csv('data/inat_taxonomy_20220130.csv', header = TRUE)



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
  dplyr::select('common.name','scientific.name','date','locality','latitude','longitude')

#Fix dates
api.mod[c('year', 'trash')] <- str_split_fixed(api.mod$date, '-', 2)

#Clean up after date manipulation
api.mod.v2 <- dplyr::select(api.mod, -c('trash')) %>% 
  dplyr::select('common.name','scientific.name','date','year','locality','latitude','longitude')

#Add column for taxa
api.rn <- nrow(api.mod.v2)
api.taxa <- as.data.frame(rep("Apidae", times = api.rn))

#Cbind together
api.mod.v3 <- cbind(api.mod.v2, api.taxa) %>% 
  rename('taxonomy'='rep(\"Apidae\", times = api.rn)') %>% 
  dplyr::select('common.name','scientific.name','taxonomy','date','year','locality','latitude','longitude')


##Lepidoptera
#Gather columns of interest and rename columns
lep.mod <- lep.mod.ALL[,c('latitude', 'longitude', 'id', 'scientific_name', 'common_name', 'place_guess',
                          'observed_on')] %>%
  rename('scientific.name'='scientific_name', 'common.name'='common_name', 'locality'='place_guess',
         'date'='observed_on') %>% 
  dplyr::select('common.name','scientific.name','date','locality','latitude','longitude')

#Fix dates
lep.mod[c('year', 'trash')] <- str_split_fixed(lep.mod$date, '-', 2)

#Clean up after date manipulation
lep.mod.v2 <- dplyr::select(lep.mod, -c('trash')) %>% 
  dplyr::select('common.name','scientific.name','date','year','locality','latitude','longitude')

#Add column for taxa
lep.rn <- nrow(lep.mod.v2)
lep.taxa <- as.data.frame(rep("Lepidoptera", times = lep.rn))

#Cbind together
lep.mod.v3 <- cbind(lep.mod.v2, lep.taxa) %>% 
  rename('taxonomy'='rep(\"Lepidoptera\", times = lep.rn)') %>% 
  dplyr::select('common.name','scientific.name','taxonomy','date','year','locality','latitude','longitude')


##Create one data sheet with all our modern insect data
#Rbind these two dataframes
insect.mod <- rbind(lep.mod.v3, api.mod.v3)
insect.mod <- filter(insect.mod, year > 2011)


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
ih.species.list.final <- ih.species.list[!duplicated(ih.species.list$scientific.name), ] %>% 
  dplyr::select(-c('specific.epithet'))



#------------------------------------------------#
####  Adding Taxonomy to Modern Insect Data   ####
#------------------------------------------------#

##Add taxonomy for location dataset
#Split column to make a genus column
insect.mod[c('genus', 'trash')] <- str_split_fixed(insect.mod$scientific.name, ' ', 2)

insect.mod2 <- base::merge(insect.mod, ih.species.list.final, by = "scientific.name", all.x = TRUE) %>% 
  dplyr::select(-c('taxonomy','trash','genus.y','name.synonyms','locality.y')) %>% 
  dplyr::select('order','super.family','family','genus.x','scientific.name', everything()) %>% 
  rename('genus'='genus.x', 'locality'='locality.x')

#Fill in the order, super family, and family names for NAs from proctor
insect.mod2$order[is.na(insect.mod2$order)] <- insect.mod2$order[match(insect.mod2$genus,insect.mod2$genus)][which(is.na(insect.mod2$order))]
insect.mod2$super.family[is.na(insect.mod2$super.family)] <- insect.mod2$super.family[match(insect.mod2$genus,insect.mod2$genus)][which(is.na(insect.mod2$super.family))]
insect.mod2$family[is.na(insect.mod2$family)] <- insect.mod2$family[match(insect.mod2$genus,insect.mod2$genus)][which(is.na(insect.mod2$family))]

#Fill in the order, super family, and family names for the remainging NAs from the iNaturalist taxon download in Google Drive
insect.mod2$order[is.na(insect.mod2$order)] <- inat.taxon$taxon_order_name[match(insect.mod2$genus,inat.taxon$taxon_genus_name)][which(is.na(insect.mod2$order))]
insect.mod2$super.family[is.na(insect.mod2$super.family)] <- inat.taxon$taxon_superfamily_name[match(insect.mod2$genus,inat.taxon$taxon_genus_name)][which(is.na(insect.mod2$super.family))]
insect.mod2$family[is.na(insect.mod2$family)] <- inat.taxon$taxon_family_name[match(insect.mod2$genus,inat.taxon$taxon_genus_name)][which(is.na(insect.mod2$family))]

#Remove a sneaky non-species level taxa
insect.mod.locs <- insect.mod2[!(insect.mod2$scientific.name=="Crambinae"),]
insect.mod.locs <- insect.mod.locs %>% 
  dplyr::select(-c('year'))


##Add taxonomy for species list
#Split column to make a genus column
im.species.list[c('genus', 'trash')] <- str_split_fixed(im.species.list$scientific.name, ' ', 2)

#Merge with taxonomy from the historic dataset and clean
im.species.list2 <- base::merge(im.species.list, ih.species.list.final, by = "scientific.name", all.x = TRUE) %>% 
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
im.species.list.final <- im.species.list2[!(im.species.list2$scientific.name=="Crambinae"),]
im.species.list.final <- im.species.list.final %>% 
  dplyr::select('order','super.family','family','genus','scientific.name', 'common.name')



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
  insect <- basename(list.files(path = 'data/', pattern = 'proctorinsect_processed_2')) 
  return(tail(insect, 1))
}

insect.mod.inat <- function(x) {
  insect <- basename(list.files(path = 'data/', pattern = 'inatinsect_processed_2')) 
  return(tail(insect, 1))
}

in.mod.locs <- function(x) {
  insect <- basename(list.files(path = 'data/', pattern = 'inatinsect_mappingloc_2')) 
  return(tail(insect, 1))
}

##File exporting
##Write out modern insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(im.species.list.final, paste('data/inatinsect_processed_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', insect.mod.inat()), path = as_id(drive.output))

##Write out modern insect location data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(insect.mod.locs, paste('data/inatinsect_mappingloc_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', in.mod.locs()), path = as_id(drive.output))

##Write out historic insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(ih.species.list.final, paste('data/proctorinsect_processed_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', insect.his.proc()), path = as_id(drive.output))

