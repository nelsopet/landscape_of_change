#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(tidyverse)
require(googledrive)
require(utils)


#-----------------------------------------------------------------#
####                Inputing Raw Data from eBird               ####
#-----------------------------------------------------------------#

#Change WD to put download data into the 'data' folder
setwd(paste0(getwd(), "/data"))

#Bird data from eBird stored in the Google Drive
drive_download("https://drive.google.com/file/d/1U3UEQ3R2fzS0gGKVnby-5kdvJMqHKnjv/view?usp=sharing")
bird.mod.ALL <- read.delim('ebd_US-ME-009_relDec-2021.txt', header = TRUE)

#Return working directory to main folder
wd <- getwd()
setwd(gsub("/data", "", wd))

