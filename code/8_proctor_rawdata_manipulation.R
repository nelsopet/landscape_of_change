#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(readxl)
require(tidyverse)
require(dplyr)
require(googledrive)

select <- dplyr::select

#------------------------------------------------#
####       Read in Required Insect Data       ####
#------------------------------------------------#

##Read in the historical data
#Apidae data
api.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 2)
#Lepidoptera data
lep.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 1)
#Historical insects for taxonomy
insect.his.analysis <- read.csv('data/proctorinsect_processed_readin.csv', header = TRUE)



#------------------------------------------------#
####            Fix Proctor Data              ####
#------------------------------------------------#

##Get just the vouchers column
#Lep
lep.vouch <- lep.his.ALL %>% 
  select('ALL VOUCHERS') %>% 
  rename("voucher"='ALL VOUCHERS')

#Api
api.vouch <- api.his.ALL %>% 
  select('ALL VOUCHERS') %>% 
  rename("voucher"='ALL VOUCHERS')


###Make each specimen its own column for now
##Lep
#Split
lep.vouch[c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q')] <- 
  str_split_fixed(lep.vouch$voucher, "\\* ", 17)
#Remove columns
lep.vouch.wide <- lep.vouch %>% 
  select(-c('voucher', 'a')) %>% 
  filter(b != 'No specimens found.')

##Api
#Split
api.vouch[c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n')] <- 
  str_split_fixed(api.vouch$voucher, "\\* ", 14)
#Remove columns
api.vouch.wide <- api.vouch %>% 
  select(-c('voucher', 'a')) %>% 
  filter(b != 'No specimens found.')


###Split each column to rbind and make data tall
##Lep
#Create a dataframe for each voucher column
lep.vouch1 <- dplyr::select(lep.vouch.wide, b)
lep.vouch2 <- dplyr::select(lep.vouch.wide, c)
lep.vouch3 <- dplyr::select(lep.vouch.wide, d)
lep.vouch4 <- dplyr::select(lep.vouch.wide, e)
lep.vouch5 <- dplyr::select(lep.vouch.wide, f)
lep.vouch6 <- dplyr::select(lep.vouch.wide, g)
lep.vouch7 <- dplyr::select(lep.vouch.wide, h)
lep.vouch8 <- dplyr::select(lep.vouch.wide, i)
lep.vouch9 <- dplyr::select(lep.vouch.wide, j)
lep.vouch10 <- dplyr::select(lep.vouch.wide, k)
lep.vouch11 <- dplyr::select(lep.vouch.wide, l)
lep.vouch12 <- dplyr::select(lep.vouch.wide, m)
lep.vouch13 <- dplyr::select(lep.vouch.wide, n)
#Now bind all thse rows together
lep.vouch.tall <- rbind(lep.vouch1, setNames(lep.vouch2, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch3, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch4, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch5, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch6, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch7, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch8, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch9, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch10, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch11, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch12, 'b'))
lep.vouch.tall <- rbind(lep.vouch.tall, setNames(lep.vouch13, 'b'))
#And remove blank rows
lep.vouch.tall2 <- lep.vouch.tall %>% 
  filter(b != '')

##Api
#Create a dataframe for each voucher column
api.vouch1 <- dplyr::select(api.vouch.wide, b)
api.vouch2 <- dplyr::select(api.vouch.wide, c)
api.vouch3 <- dplyr::select(api.vouch.wide, d)
api.vouch4 <- dplyr::select(api.vouch.wide, e)
api.vouch5 <- dplyr::select(api.vouch.wide, f)
api.vouch6 <- dplyr::select(api.vouch.wide, g)
api.vouch7 <- dplyr::select(api.vouch.wide, h)
api.vouch8 <- dplyr::select(api.vouch.wide, i)
api.vouch9 <- dplyr::select(api.vouch.wide, j)
api.vouch10 <- dplyr::select(api.vouch.wide, k)
api.vouch11 <- dplyr::select(api.vouch.wide, l)
api.vouch12 <- dplyr::select(api.vouch.wide, m)
api.vouch13 <- dplyr::select(api.vouch.wide, n)
#Now bind all thse rows together
api.vouch.tall <- rbind(api.vouch1, setNames(api.vouch2, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch3, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch4, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch5, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch6, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch7, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch8, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch9, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch10, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch11, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch12, 'b'))
api.vouch.tall <- rbind(api.vouch.tall, setNames(api.vouch13, 'b'))
#And remove blank rows
api.vouch.tall2 <- api.vouch.tall %>% 
  filter(b != '')


##Combine the two to work with one datasheet now
#rbind
insect.vouch <- rbind(lep.vouch.tall2, api.vouch.tall2)


##Split this newly created column to get the different columns out
#Have to split each section separately since there's an issue using ";" as a separator
insect.vouch[c('species.name.and.authority', 'split')] <- str_split_fixed(insect.vouch$b, '; Collector: ', 2)
insect.vouch[c('collector', 'split2')] <- str_split_fixed(insect.vouch$split, '; Collection Date: ', 2)
insect.vouch[c('collection.date', 'split3')] <- str_split_fixed(insect.vouch$split2, '; Collection #: ', 2)
insect.vouch[c('collection.number', 'split4')] <- str_split_fixed(insect.vouch$split3, '; Collection Location: ', 2)
insect.vouch[c('collection.location', 'split5')] <- str_split_fixed(insect.vouch$split4, '; Catalog #: ', 2)
insect.vouch[c('catalog.number', 'trash')] <- str_split_fixed(insect.vouch$split5, '\\.', 2)


##Make a final records list with clean columns
#First grab the right columns
his.specimens <- insect.vouch %>% 
  select('species.name.and.authority','collector','collection.date','collection.number','collection.location','catalog.number')
#split species from the naming authority
his.specimens[c('genus', 'species', 'trash')] <- str_split_fixed(his.specimens$species.name.and.authority, ' ', 3)
#Bind to make scientific name
his.specimens$scientific.name = paste(his.specimens$genus, his.specimens$species, sep=" ")
#Clean
his.specimens2 <- his.specimens %>% 
  select('genus','scientific.name','species.name.and.authority','collector','collection.date','collection.number','collection.location','catalog.number')




#------------------------------------------------#
####        Writing Out New .csv File        ####
#------------------------------------------------#

###Output data
##Create filedate to print the date the file is exported in the file name for uploading to Google Drive
filedate <- print(format(Sys.Date(), "%Y%m%d"))

##Create pathway for exported files
drive.output.raw <- "https://drive.google.com/drive/u/5/folders/1HoIem13bRDtHGfTm23PCR0vuuabDJsJJ"

##Automate the newest file name output for processed Proctor's insect bird data
spec.pull <- function(x) {
  insect <- basename(list.files(path = 'data/', pattern = 'historicspecimen_rawdata_2')) 
  return(tail(insect, 1))
}



##File exporting
##Write out modern insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
#write_csv(his.specimens2, paste('data/historicspecimen_rawdata_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', spec.pull()), path = as_id(drive.output.raw))
