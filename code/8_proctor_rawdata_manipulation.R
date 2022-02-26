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
api.vouch1[c('a', 'b', 'c', 'd', 'e')] <- 
  str_split_fixed(api.vouch1$Voucher1, 'Colle', 5)
api.vouch1 <- api.vouch1[,-1]


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


#Now bind all thse rows together
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
lep.vouch.com <- rbind(lep.vouch1, lep.vouch2)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch3)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch4)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch5)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch6)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch7)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch8)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch9)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch10)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch11)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch12)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch13)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch14)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch15)
lep.vouch.com <- rbind(lep.vouch.com, lep.vouch16)

#Fix the catalog number column
lep.vouch.com2 <- lep.vouch.com
lep.vouch.com2[c('split1', 'split2', 'split3', 'split4', 'split5')] <- str_split_fixed(lep.vouch.com$catalog.number, ';', 5)

lep.vouch.com3 <- lep.vouch.com2[grep("#", lep.vouch.com2$split1),]
lep.vouch.com4 <- lep.vouch.com2[grep("#", lep.vouch.com2$split2),]
lep.vouch.com5 <- lep.vouch.com2[grep("#", lep.vouch.com2$split3),]
lep.vouch.com6 <- lep.vouch.com2[grep("#", lep.vouch.com2$split4),]


lep.vouch.com7 <- lep.vouch.com2[grep("#", lep.vouch.com2$split5),]

lep.vouch.com7$collection.locations <- paste(lep.vouch.com7$collection.location, lep.vouch.com7$split1, sep=",")
lep.vouch.com7$collection.locations <- paste(lep.vouch.com7$collection.locations, lep.vouch.com7$split2, sep=",")
lep.vouch.com7$collection.locations <- paste(lep.vouch.com7$collection.locations, lep.vouch.com7$split3, sep=",")
lep.vouch.com7$collection.locations <- paste(lep.vouch.com7$collection.locations, lep.vouch.com7$split4, sep=",")


lep.vouch.com <- data.frame(lapply(lep.vouch.com,c))

lep.vouch.all <- lep.vouch.com

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

#lep.vouch.all$catalog.number <- lep.vouch.all %>%
  #dplyr::select(catalog.number) %>%
  #separate(col = catalog.number, into = c('trash', 'catalog.number2'), sep = '#')
  #mutate(catalog.number = str_replace(catalog.number, '.*(\\s[:punct:][:punct:]\\s\\w*\\d*[:punct:])', '')) 
  #mutate(catalog.number = str_replace(catalog.number, '[:punct:]$', ''))

lep.vouch.all$collection.location <- lep.vouch.all %>%
  dplyr::select(collection.location) %>%
  mutate(collection.location = str_replace(collection.location, '^\\s\\w*\\s\\w*[:punct:]\\s', ''))


#Remove no specimens found and blank rows
lep.vouch.all <- subset(lep.vouch.all, lep.vouch.all$species.name.and.authority != 'No specimens found')
lep.vouch.all <- subset(lep.vouch.all, lep.vouch.all$species.name.and.authority != '')


#------------------------------------------------#
####      Fixing columns in list format       ####
#------------------------------------------------#

#Coerce out a list format
api.vouch.clean <- data.frame(lapply(api.vouch.all,c))
lep.vouch.clean <- data.frame(lapply(lep.vouch.all,c))


#------------------------------------------------#
####     Merging data to get final output     ####
#------------------------------------------------#

##Rename columns for merge
#Api
api.his.merge <- api.his.ALL %>% 
  rename( "super.family"="Super Family", 'phylum'="Phylum", 'class'="Class", 'order'="ORDER",
          'family'="FAMILY", 'species.name.and.authority'="Species Name & authority", 
          'synonymy'="Synonymy/Other Names", 'locations'="Locations", 'abundance'="Abundance", 'notes'="Notes")
  
#Lep
lep.his.merge <- lep.his.ALL %>% 
  rename( "super.family"="Super Family", 'phylum'="Phylum", 'class'="Class", 'order'="ORDER",
        'family'="FAMILY", 'species.name.and.authority'="Species Name & authority", 
        'synonymy'="Synonymy/Other Names", 'locations'="Locations", 'abundance'="Abundance", 'notes'="Notes")


##Data manipulation for merge
#Split columns
api.his.merge[c('genus', 'se', 'naming.authority')] <- str_split_fixed(api.his.merge$species.name.and.authority, ' ', 3)
lep.his.merge[c('genus', 'se', 'naming')] <- str_split_fixed(lep.his.merge$species.name.and.authority, ' ', 3)
lep.his.merge[c('naming.authority', 'trash')] <- str_split_fixed(lep.his.merge$naming, ',', 2)
api.vouch.clean[c('genus', 'se', 'naming.authority')] <- str_split_fixed(api.vouch.clean$species.name.and.authority, ' ', 3)
lep.vouch.clean[c('genus', 'se', 'naming.authority')] <- str_split_fixed(lep.vouch.clean$species.name.and.authority, ' ', 3)

#Combine species.name
api.his.merge$species.name <- paste(api.his.merge$genus, api.his.merge$se, sep=" ")
lep.his.merge$species.name <- paste(lep.his.merge$genus, lep.his.merge$se, sep=" ")
api.vouch.clean$species.name <- paste(api.vouch.clean$genus, api.vouch.clean$se, sep=" ")
lep.vouch.clean$species.name <- paste(lep.vouch.clean$genus, lep.vouch.clean$se, sep=" ")

#Fix specific instances
api.vouch.clean['naming.authority'][api.vouch.clean['species.name'] == "Psithyrus laboriosus"] <- 'Fabricius'

#Clean up columns
api.his.merge2 <- api.his.merge %>% 
  dplyr::select(-c(11:31, 'se', 'species.name.and.authority'))
  
lep.his.merge2 <- lep.his.merge %>% 
  dplyr::select(-c(11:50, 'se', 'naming', 'trash', 'species.name.and.authority'))

api.vouch.clean2 <- api.vouch.clean %>% 
  dplyr::select(-c('se', 'species.name.and.authority'))

lep.vouch.clean2 <- lep.vouch.clean %>% 
  dplyr::select(-c('se', 'species.name.and.authority'))

#Remove parentheses in naming.authority
api.his.merge2$naming.authority <- api.his.merge2 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*)\\)', '\\1'))

lep.his.merge2$naming.authority <- lep.his.merge2 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '\\(', ''))

api.vouch.clean2$naming.authority <- api.vouch.clean2 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*)\\)', '\\1'))

#Extra work for leps
lep.vouch.clean2 <- data.frame(lapply(lep.vouch.clean2,c))

lep.vouch.clean2$naming.authority <- lep.vouch.clean2 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*)\\)', '\\1')) %>% 
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*\\s\\W\\s\\w*)\\)', '\\1'))

lep.vouch.clean3 <- data.frame(lapply(lep.vouch.clean2,c))

lep.vouch.clean3$naming.authority <- lep.vouch.clean3 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '([:upper:]\\w*)\\s\\w*', '\\1')) %>% 
  mutate(naming.authority = str_replace(naming.authority, '\\w*\\s([:upper:]\\w*)', '\\1')) %>% 
  mutate(naming.authority = str_replace(naming.authority, '\\s[:lower:]\\w*', '')) %>% 
  mutate(naming.authority = str_replace(naming.authority, '^[:lower:]\\w*', '')) 

lep.vouch.clean4 <- data.frame(lapply(lep.vouch.clean3,c))

lep.vouch.clean4$naming.authority <- lep.vouch.clean4 %>%
  dplyr::select(naming.authority) %>%
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*)\\)', '\\1')) %>% 
  mutate(naming.authority = str_replace(naming.authority, '\\((\\w*\\W\\w*)\\)', '\\1'))

#Ensure correct formatting
api.his.merge2 <- data.frame(lapply(api.his.merge2,c))
lep.his.merge2 <- data.frame(lapply(lep.his.merge2,c))
api.vouch.clean2 <- data.frame(lapply(api.vouch.clean2,c))
lep.vouch.clean4 <- data.frame(lapply(lep.vouch.clean4,c))


##Merge 
#api.his.merge with api.vouch.clean
api.vouch.final <- api.vouch.clean %>% 
  merge(api.his.merge, by = 'species.name.and.authority', all.x = TRUE)

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
write_csv(api.vouch.final, paste('data/apivoucher_rawdata_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', api.pull()), path = as_id(drive.output))

##Write out historic insect data as .csv and upload to google drive -- Commented out to stop repetition of downloads
write_csv(lep.vouch.final, paste('data/lepvoucher_rawdata_', filedate, '.csv', sep=''))
#drive_upload(paste0('data/', lep.pull()), path = as_id(drive.output))
