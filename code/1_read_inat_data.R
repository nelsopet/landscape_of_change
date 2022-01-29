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
#Create a bounding box of the greater MDI area
grmdi <- c(44.005564, -68.302656, 44.310216, -68.001644)

##Pull in the modern iNat data
#Download Apidae data
api.mod.ALL <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', bounds = grmdi)
#Download Lepidoptera data
lep.mod.ALL <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', bounds = grmdi, maxresults = 10000)
