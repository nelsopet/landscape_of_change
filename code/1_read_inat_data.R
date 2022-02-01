#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(rinat)
require(tidyverse)
require(mapview)


#-----------------------------------------------------------------#
####             Inputing Raw Data from iNaturalist            ####
#-----------------------------------------------------------------#

#For information visit https://github.com/ropensci/rinat

##Create the bounds to filter the downloaded data through
#Create a bounding box of the LOC area
loc.area <- c(44.16919, -68.47352, 44.49284, -68.14983)

##Pull in the modern iNat data
#Download Apidae data
api.mod.ALL <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', bounds = loc.area, maxresults = 10000)
#Download Lepidoptera data
lep.mod.ALL <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', bounds = loc.area, maxresults = 10000)

