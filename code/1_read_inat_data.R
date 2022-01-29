#!/usr/bin/env Rscript --vanilla


#-----------------------------------------------------------------#
####             Inputing Raw Data from iNaturalist            ####
#-----------------------------------------------------------------#

require(rinat)
require(tidyverse)
require(mapview)


#For information visit https://github.com/ropensci/rinat
#schoodic_pen <- c(44.32006645732407,-67.96261999590361,44.47117918584468,-68.14526770098173)

#Create a bounding box of the greater MDI area
grmdi <- c(44.005564, -68.302656, 44.310216, -68.001644)

#Download Apidae data
apidae <- get_inat_obs(taxon_name = "Apidae",  quality = 'research', bounds = grmdi)

#Download Lepidoptera data
lepid <- get_inat_obs(taxon_name = "Lepidoptera",  quality = 'research', bounds = grmdi, maxresults = 10000)


