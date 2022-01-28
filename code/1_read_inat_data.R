#!/usr/bin/env Rscript --vanilla


#-----------------------------------------------------------------#
####             Inputing Raw Data from iNaturalist            ####
#-----------------------------------------------------------------#

#https://github.com/ropensci/rinat
#Walk through example from above
require(rinat)
require(tidyverse)
require(mapview)

#Bounding box of Maine
bounds <- c(42.91713151272138,-71.08390873322881,47.459858919111994,-66.88543604916603)

schoodic_pen <- c(44.32006645732407,-67.96261999590361,44.47117918584468,-68.14526770098173 )

tst_area <- rinat::get_inat_obs(bounds = schoodic_pen, query = "Papilio glaucus")
tst_area %>% 
  dplyr::select(scientific_name, common_name) %>% 
  unique() %>% 
  View

inat_map(schoodic_pen)

colnames(tst_area)
plot(schoodic_pen)

#Get Eider data from iNat
Maine_Eider<-get_inat_obs(query = "Eider", bounds = bounds, maxresults = 10000)
plot(Maine_Eider$longitude,Maine_Eider$latitude)
## Map Eiders in mMaine
Maine_Eider_map <- inat_map(Maine_Eider, map='county', subregion='maine', plot = FALSE)

Maine_Eider_map 

#Grab dates to see range of observations
tst_date<-unique(Maine_Eider$observed_on)

#See distribution of observations
as.Date(tst_date) %>% hist(breaks=10)
