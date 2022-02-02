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

##Pull in the modern iNat data
#Download Apidae data
api.mod.ALL <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', place_id = 174940, maxresults = 10000)
#Download Lepidoptera data
lep.mod.ALL <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', place_id = 174940, maxresults = 10000)

