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

#Bird data from eBird stored in the Google Drive
drive_download("https://drive.google.com/file/d/1U3UEQ3R2fzS0gGKVnby-5kdvJMqHKnjv/view?usp=sharing", path = "data/")
bird.mod.ALL <- read.delim('data/ebd_US-ME-009_relDec-2021.txt', header = TRUE)

