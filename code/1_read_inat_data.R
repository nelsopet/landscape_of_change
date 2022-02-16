#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(rinat)
require(tidyverse)


#-----------------------------------------------------------------#
####             Inputing Raw Data from iNaturalist            ####
#-----------------------------------------------------------------#

#For information on downloading iNat data visit https://github.com/ropensci/rinat

##Get the modern insect iNat data
#Download Apidae data
api.mod.ALL <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', place_id = 174940, maxresults = 10000)
#Download Lepidoptera data
lep.mod.ALL <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', place_id = 174940, maxresults = 10000)


##Grab the modern bird iNat data'#Download Apidae data
bird.inat.ALL <- get_inat_obs(taxon_name = "Aves",  quality = 'research', place_id = 174940, maxresults = 10000)
