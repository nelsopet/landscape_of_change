#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(readxl)
require(tidyverse)
require(dplyr)
require(lubridate)


#------------------------------------------------#
####       Read in Required Insect Data       ####
#------------------------------------------------#

##Read in the historical data
#Apidae data
api.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 2)
#Lepidoptera data
lep.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 1)


#------------------------------------------------#
####       Fix Proctor Data Date Issue        ####
#------------------------------------------------#


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

api.vouch1[c('species.name.and.authority', 'collector', 'collection.date', 'collection.number', 'collection.location', 'catalog.number')] <- 
  str_split_fixed(api.vouch1$Voucher1, ';', 6)

practice <- api.vouch1 %>%
  dplyr::select(species.name.and.authority) %>%
  mutate(delimited = str_replace(species.name.and.authority, '^[:punct:]\\s(\\w*\\s\\w*\\s\\w*)[:punct:]$',
                                              '\\1'))







# practice <- api.vouch %>% 
#   dplyr::select(Voucher1) %>% 
#   mutate(delimited = str_replace(Voucher1, '^[:punct:]\\s(\\w*\\s\\w*\\s\\w*)[:punct:]\\s\\w*
#                                               [:punct:]\\s(\\w*[:punct:]\\s\\w*)', 
#                                               '\\1-\\2')) %>% 
#   separate(col = delimited,
#            into = c('species.name.authority', 'collector'), sep = '-', convert = TRUE)






