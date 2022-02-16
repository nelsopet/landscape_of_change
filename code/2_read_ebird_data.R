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

### ACCESSING EBIRD DATA
#To access eBird data you must first sign in to an eBird account
#Then go to the data access page and fill out the form
#It will take about a week at most for access to be granted
#You can then download a subset of data by Hancock County if you wish
#All instructions and best practices can be found here https://cornelllabofornithology.github.io/ebird-best-practices/intro.html
#Upload your downloaded file to Google Drive and read in the data as follows

#Bird data from eBird stored in the Google Drive
drive_download("https://drive.google.com/file/d/1U3UEQ3R2fzS0gGKVnby-5kdvJMqHKnjv/view?usp=sharing", path = "data/")
bird.mod.ALL <- read.delim('data/ebd_US-ME-009_relDec-2021.txt', header = TRUE)

