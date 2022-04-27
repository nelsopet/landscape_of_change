#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(dplyr)
require(sf)
require(lubridate)


#------------------------------------------------#
####       Read in Required Bird Data         ####
#------------------------------------------------#

#Read in modern bird data
bird.mod.ALL <- read.delim('data/ebd_US-ME-009_relDec-2021.txt', header = TRUE)

#Read in shapefile for filtering data
loc.circle <-  read_sf("data/MDI_Circle.shp")



#------------------------------------------------#
####        Manipulation of eBird Data        ####
#------------------------------------------------#

##Cleaning and subsetting
#Select the columns of interest for our purposes, and rename column headers to fit our conventions
bird.mod <- bird.mod.ALL[c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVER.ID', 'SAMPLING.EVENT.IDENTIFIER',
                           'COUNTY', 'LOCALITY', 'LATITUDE', 'LONGITUDE')] %>% 
  rename('ch.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 'scientific.name'='SCIENTIFIC.NAME', 
         'observer'='OBSERVER.ID', 'checklist'='SAMPLING.EVENT.IDENTIFIER', 'county'='COUNTY', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

#Change the date from chr to ymd format to easily subset data by years
bird.mod$date <- ymd(bird.mod$ch.date)

#Subset for correct date, delete the old date column, and reorder columns
bird.mod.v2 <- subset(bird.mod, format(as.Date(date),"%Y")==2021) %>% 
  dplyr::select(-('ch.date')) %>% 
  dplyr::select('common.name', 'scientific.name', 'date', everything())
bird.mod.v3 <- bird.mod.v2 %>% 
  filter(date > "2021-04-18" & date < "2021-11-01")


##Filter for obs inside LOC polygon
#Create coordinates for spatial overlap
pnts_sf <- st_as_sf(bird.mod.v3, coords = c('longitude', 'latitude'), crs = st_crs(loc.circle))

#Determine intersection
bird.mod.v4 <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, loc.circle))
  , area = if_else(is.na(intersection), '', loc.circle$Name[intersection])
) 

#filter by those records inside polygon
bird.mod.v5 <- filter(bird.mod.v4, intersection==1)
bird.mod.v6 <- bird.mod.v5 %>% 
  dplyr::select(-c('intersection', 'area'))

#Rename
bm.species.list <- bird.mod.v6

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



#------------------------------------------------#
####               Outputs                    ####
#------------------------------------------------#

#Determine number of observers
num.obs <- length(unique(bird.mod.v6$observer))

#Determine number of checklists
num.ckl <- length(unique(bm.species.list2$checklist))

#Determine number of species detected
num.spp <- length(unique(bm.species.list2$common.name))

#Determine the most observed species
bm.freq <- as.data.frame(table(bm.species.list2$common.name)) %>% 
  rename('common.name'='Var1', 'frequency'='Freq') %>% 
  arrange(desc(frequency))
#Write out this as csv becuase its cool
write.csv(bm.freq, "outputs/2021_ebirdspp_frequency.csv")




