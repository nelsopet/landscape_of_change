#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(readxl)
require(tidyverse)
require(dplyr)


#------------------------------------------------#
####       Read in Required Insect Data       ####
#------------------------------------------------#

##Read in the historical data
#Apidae data
api.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 2)
#Lepidoptera data
lep.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 1)


#------------------------------------------------#
####            Fix Proctor Data              ####
#------------------------------------------------#

##Apidae
#Create a dataframe for each voucher column
api.vouch1 <- dplyr::select(api.his.ALL, 12)
api.vouch2 <- dplyr::select(api.his.ALL, 13)
api.vouch3 <- dplyr::select(api.his.ALL, 14)
api.vouch4 <- dplyr::select(api.his.ALL, 15)
api.vouch5 <- dplyr::select(api.his.ALL, 16)
api.vouch6 <- dplyr::select(api.his.ALL, 17)
api.vouch7 <- dplyr::select(api.his.ALL, 18)
api.vouch8 <- dplyr::select(api.his.ALL, 19)
api.vouch9 <- dplyr::select(api.his.ALL, 20)
api.vouch10 <- dplyr::select(api.his.ALL, 21)
api.vouch11 <- dplyr::select(api.his.ALL, 22)
api.vouch12 <- dplyr::select(api.his.ALL, 23)
api.vouch13 <- dplyr::select(api.his.ALL, 24)


##Split and name columns
#1
api.vouch1[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch1$Voucher1, ';', 6)
api.vouch1 <- api.vouch1[,-1]
#2
api.vouch2[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch2$Voucher2, ';', 6)
api.vouch2 <- api.vouch2[,-1]
#3
api.vouch3[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch3$Voucher3, ';', 6)
api.vouch3 <- api.vouch3[,-1]
#4
api.vouch4[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch4$Voucher4, ';', 6)
api.vouch4 <- api.vouch4[,-1]
#5
api.vouch5[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch5$Voucher5, ';', 6)
api.vouch5 <- api.vouch5[,-1]
#6
api.vouch6[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch6$Voucher6, ';', 6)
api.vouch6 <- api.vouch6[,-1]
#7
api.vouch7[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch7$Voucher7, ';', 6)
api.vouch7 <- api.vouch7[,-1]
#8
api.vouch8[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch8$Voucher8, ';', 6)
api.vouch8 <- api.vouch8[,-1]
#9
api.vouch9[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch9$Voucher9, ';', 6)
api.vouch9 <- api.vouch9[,-1]
#10
api.vouch10[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch10$Voucher10, ';', 6)
api.vouch10 <- api.vouch10[,-1]
#11
api.vouch11[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch11$Voucher11, ';', 6)
api.vouch11 <- api.vouch11[,-1]
#12
api.vouch12[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch12$Voucher12, ';', 6)
api.vouch12 <- api.vouch12[,-1]
#13
api.vouch13[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch13$Voucher13, ';', 6)
api.vouch13 <- api.vouch13[,-1]


#Now bind all thse rows back together
api.vouch.all <- rbind(api.vouch1, api.vouch2)
api.vouch.all <- rbind(api.vouch.all, api.vouch3)
api.vouch.all <- rbind(api.vouch.all, api.vouch4)
api.vouch.all <- rbind(api.vouch.all, api.vouch5)
api.vouch.all <- rbind(api.vouch.all, api.vouch6)
api.vouch.all <- rbind(api.vouch.all, api.vouch7)
api.vouch.all <- rbind(api.vouch.all, api.vouch8)
api.vouch.all <- rbind(api.vouch.all, api.vouch9)
api.vouch.all <- rbind(api.vouch.all, api.vouch10)
api.vouch.all <- rbind(api.vouch.all, api.vouch11)
api.vouch.all <- rbind(api.vouch.all, api.vouch12)
api.vouch.all <- rbind(api.vouch.all, api.vouch13)


#Clean
api.vouch.all$species.name.and.authority <- api.vouch.all %>%
  dplyr::select(species.name.and.authority) %>%
  mutate(species.name.and.authority = str_replace(species.name.and.authority, '^[:punct:]\\s', '')) %>% 
  mutate(species.name.and.authority = str_replace(species.name.and.authority, '^(\\w*\\s\\w*\\s\\w*)[:punct:]$', '\\1'))
  
api.vouch.all$collector <- api.vouch.all %>%
  dplyr::select(collector) %>%
  mutate(collector = str_replace(collector, '\\w*[:punct:]\\s', ''))

api.vouch.all$collection.date <- api.vouch.all %>%
  dplyr::select(collection.date) %>%
  mutate(collection.date = str_replace(collection.date, '\\w*\\s\\w*[:punct:]\\s', ''))

api.vouch.all$collection.number <- api.vouch.all %>%
  dplyr::select(collection.number) %>%
  mutate(collection.number = str_replace(collection.number, '\\w*\\s[:punct:]*\\s', ''))

api.vouch.all$collection.location <- api.vouch.all %>%
  dplyr::select(collection.location) %>%
  mutate(collection.location = str_replace(collection.location, '\\w*\\s\\w*[:punct:]\\s', ''))

api.vouch.all$catalog.number <- api.vouch.all %>%
  dplyr::select(catalog.number) %>%
  mutate(catalog.number = str_replace(catalog.number, '\\w*\\s[:punct:]*\\s', '')) %>% 
  mutate(catalog.number = str_replace(catalog.number, '[:punct:]$', ''))


#Remove no specimens found and blank rows
api.vouch.all <- subset(api.vouch.all, api.vouch.all$species.name.and.authority != 'No specimens found')
api.vouch.all <- subset(api.vouch.all, api.vouch.all$species.name.and.authority != '')




##Lepidoptera
#Create a dataframe for each voucher column
lep.vouch1 <- dplyr::select(lep.his.ALL, 13)
lep.vouch2 <- dplyr::select(lep.his.ALL, 14)
lep.vouch3 <- dplyr::select(lep.his.ALL, 15)
lep.vouch4 <- dplyr::select(lep.his.ALL, 16)
lep.vouch5 <- dplyr::select(lep.his.ALL, 17)
lep.vouch6 <- dplyr::select(lep.his.ALL, 18)
lep.vouch7 <- dplyr::select(lep.his.ALL, 19)
lep.vouch8 <- dplyr::select(lep.his.ALL, 20)
lep.vouch9 <- dplyr::select(lep.his.ALL, 21)
lep.vouch10 <- dplyr::select(lep.his.ALL, 22)
lep.vouch11 <- dplyr::select(lep.his.ALL, 23)
lep.vouch12 <- dplyr::select(lep.his.ALL, 24)
lep.vouch13 <- dplyr::select(lep.his.ALL, 25)
lep.vouch14 <- dplyr::select(lep.his.ALL, 26)
lep.vouch15 <- dplyr::select(lep.his.ALL, 27)
lep.vouch16 <- dplyr::select(lep.his.ALL, 28)



##Split and name columns
#1
lep.vouch1[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch1$Voucher1, ';', 6)
lep.vouch1 <- lep.vouch1[,-1]
#2
lep.vouch2[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch2$Voucher2, ';', 6)
lep.vouch2 <- lep.vouch2[,-1]
#3
lep.vouch3[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch3$Voucher3, ';', 6)
lep.vouch3 <- lep.vouch3[,-1]
#4
lep.vouch4[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch4$Voucher4, ';', 6)
lep.vouch4 <- lep.vouch4[,-1]
#5
lep.vouch5[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch5$Voucher5, ';', 6)
lep.vouch5 <- lep.vouch5[,-1]
#6
lep.vouch6[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch6$Voucher6, ';', 6)
lep.vouch6 <- lep.vouch6[,-1]
#7
lep.vouch7[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch7$Voucher7, ';', 6)
lep.vouch7 <- lep.vouch7[,-1]
#8
lep.vouch8[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch8$Voucher8, ';', 6)
lep.vouch8 <- lep.vouch8[,-1]
#9
lep.vouch9[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch9$Voucher9, ';', 6)
lep.vouch9 <- lep.vouch9[,-1]
#10
lep.vouch10[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch10$Voucher10, ';', 6)
lep.vouch10 <- lep.vouch10[,-1]
#11
lep.vouch11[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch11$Voucher11, ';', 6)
lep.vouch11 <- lep.vouch11[,-1]
#12
lep.vouch12[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch12$Voucher12, ';', 6)
lep.vouch12 <- lep.vouch12[,-1]
#13
lep.vouch13[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch13$Voucher13, ';', 6)
lep.vouch13 <- lep.vouch13[,-1]
#14
lep.vouch14[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch14$Voucher14, ';', 6)
lep.vouch14 <- lep.vouch14[,-1]
#15
lep.vouch15[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch15$Voucher15, ';', 6)
lep.vouch15 <- lep.vouch15[,-1]
#16
lep.vouch16[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(lep.vouch16$Voucher16, ';', 6)
lep.vouch16 <- lep.vouch16[,-1]


#Now bind all thse rows back together
lep.vouch.all <- rbind(lep.vouch1, lep.vouch2)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch3)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch4)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch5)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch6)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch7)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch8)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch9)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch10)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch11)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch12)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch13)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch14)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch15)
lep.vouch.all <- rbind(lep.vouch.all, lep.vouch16)


#Clean
lep.vouch.all$species.name.and.authority <- lep.vouch.all %>%
  dplyr::select(species.name.and.authority) %>%
  mutate(species.name.and.authority = str_replace(species.name.and.authority, '^[:punct:]\\s', '')) %>% 
  mutate(species.name.and.authority = str_replace(species.name.and.authority, '^(\\w*\\s\\w*\\s\\w*)[:punct:]$', '\\1'))

lep.vouch.all$collector <- lep.vouch.all %>%
  dplyr::select(collector) %>%
  mutate(collector = str_replace(collector, '\\w*[:punct:]\\s', ''))

lep.vouch.all$collection.date <- lep.vouch.all %>%
  dplyr::select(collection.date) %>%
  mutate(collection.date = str_replace(collection.date, '\\w*\\s\\w*[:punct:]\\s', ''))

lep.vouch.all$collection.number <- lep.vouch.all %>%
  dplyr::select(collection.number) %>%
  mutate(collection.number = str_replace(collection.number, '\\w*\\s[:punct:]*\\s', ''))

lep.vouch.all$collection.location <- lep.vouch.all %>%
  dplyr::select(collection.location) %>%
  mutate(collection.location = str_replace(collection.location, '\\w*\\s\\w*[:punct:]\\s', ''))

lep.vouch.all$catalog.number <- lep.vouch.all %>%
  dplyr::select(catalog.number) %>%
  mutate(catalog.number = str_replace(catalog.number, '\\w*\\s[:punct:]*\\s', '')) %>% 
  mutate(catalog.number = str_replace(catalog.number, '[:punct:]$', ''))


#Remove no specimens found and blank rows
lep.vouch.all <- subset(lep.vouch.all, lep.vouch.all$species.name.and.authority != 'No specimens found')
lep.vouch.all <- subset(lep.vouch.all, lep.vouch.all$species.name.and.authority != '')



#------------------------------------------------#
####        Writing Out New .csv Files        ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Create pathway for exported files
drive.output.raw <- "https://drive.google.com/drive/u/5/folders/1HoIem13bRDtHGfTm23PCR0vuuabDJsJJ"

##Automate the newest file name output for processed Proctor's insect bird data
api.pull <- function(x) {
  insect <- basename(list.files(path = 'data/', pattern = 'apivoucher_rawdata_2')) 
  return(tail(insect, 1))
}

lep.pull <- function(x) {
  insect <- basename(list.files(path = 'data/', pattern = 'lepvoucher_rawdata_2')) 
  return(tail(insect, 1))
}


##File exporting
##Write out modern insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(api.vouch.all, paste('data/apivoucher_rawdata_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', api.pull()), path = as_id(drive.output))

##Write out historic insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(lep.vouch.all, paste('data/lepvoucher_rawdata_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', lep.pull()), path = as_id(drive.output))
