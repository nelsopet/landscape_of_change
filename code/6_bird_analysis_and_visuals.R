#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(doBy)
library(ggmap)


#------------------------------------------------#
####     Grabbing dataframes for analysis     ####
#------------------------------------------------#

#Bring in the processed cs bird data from Google Drive
drive_download((drive_find(pattern = 'csbirds_processed', n_max=1)), path = 'data/csbirds_processed_readin.csv', overwrite = TRUE)
bird.his.analysis <- read.csv('data/csbirds_processed_readin.csv', header = TRUE)

#Bring in the processed ebird data from Google Drive
drive_download((drive_find(pattern = 'ebird_processed', n_max=1)), path = 'data/ebird_processed_readin.csv', overwrite = TRUE)
drive_download((drive_find(pattern = 'ebird_mappingloc', n_max=1)), path = 'data/ebird_mappingloc_readin.csv', overwrite = TRUE)
bird.mod.analysis <- read.csv('data/ebird_processed_readin.csv', header = TRUE)
bird.mod.locs <- read.csv('data/ebird_mappingloc_readin.csv', header = TRUE)



#------------------------------------------------#
####   Analysis and Creating visualizations   ####
#------------------------------------------------#

##Manipulate data
#Combine the modern eBird data onto the historical sheet to only compare those species recorded historically
bird.analysis <- right_join(bird.mod.analysis, bird.his.analysis, by = 'scientific.name')

##Assign a numeric value to the frequencies to easily assign a change in frequency
#Modern freq
bird.analysis$freq.x <- ifelse(bird.analysis$frequency.x == 'common', '1',
                               ifelse(bird.analysis$frequency.x == 'uncommon', '2',
                                      ifelse(bird.analysis$frequency.x == 'rare', '3', '4')))

#Historical freq
bird.analysis$freq.y <- ifelse(bird.analysis$frequency.y == 'common', '1',
                               ifelse(bird.analysis$frequency.y == 'uncommon', '2',
                                      ifelse(bird.analysis$frequency.y == 'rare', '3', '4')))
  
#Create a column of frequency changes
bird.analysis$freq.changes <- ifelse(bird.analysis$freq.x == bird.analysis$freq.y, 'stable', 
                                    ifelse(bird.analysis$freq.y > bird.analysis$freq.x, 'increased', "decreased"))

#Remove the migrant only species for now
bird.analysis2 <- bird.analysis %>% filter(freq.y < 4)

#Add in values for species that were not captured in modern spreadsheet
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Antrostomus vociferus"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Progne subis"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Ectopistes migratorius"] <- 'decreased'



##Plot
#Create new dataframe to store the counts of each frequency change
freq.plot <- bird.analysis2 %>% 
  count(freq.changes) %>% 
  rename('species.number'='n') %>% 
  doBy::order_by('species.number')

#Reoder by largest to smallest
freq.plot$species.number <- factor(freq.plot$species.number, levels = freq.plot$species.number[order(freq.plot$freq.changes, decreasing = TRUE)])

#Create the ggplot
freq.plot %>% 
  ggplot(aes(freq.changes, species.number, fill = freq.changes)) +
  geom_bar(stat="identity", color="black") +
  ggtitle("Species Frequency Changes from 1880 to Present") + 
  geom_text(aes(label = species.number, vjust = 2)) +
  guides(fill = FALSE) +
  labs(x = "Change in Frequency", y = "Number of Species") +
  theme_classic(base_size=14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("forestgreen", "darkslategray4", "darkorange3"))



##Create table
freq.table <- bird.analysis2 %>% 
  dplyr::select('common.name.x', 'scientific.name', 'frequency.y', 'frequency.x', 'freq.changes') %>% 
  rename('common.name'='common.name.x', 'scientific.name'='scientific.name', 'frequency.1880'='frequency.y',
         'frequency.current'='frequency.x', 'frequency.changes'='freq.changes')

#Fix some entries
freq.table['common.name'][freq.table['scientific.name'] == "Antrostomus vociferus"] <- 'Eastern Whip-poor-will'
freq.table['common.name'][freq.table['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'Cliff Swallow'
freq.table['common.name'][freq.table['scientific.name'] == "Progne subis"] <- 'Purple Martin'
freq.table['common.name'][freq.table['scientific.name'] == "Ectopistes migratorius"] <- 'Passenger Pigeon'
freq.table['frequency.current'][freq.table['scientific.name'] == "Antrostomus vociferus"] <- 'not present'
freq.table['frequency.current'][freq.table['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'not present'
freq.table['frequency.current'][freq.table['scientific.name'] == "Progne subis"] <- 'not present'
freq.table['frequency.current'][freq.table['scientific.name'] == "Ectopistes migratorius"] <- 'extinct'



##Create data for table of modern species not recorded back in the 1880s
#Merge
modern <- right_join(bird.his.analysis, bird.mod.analysis, by = 'scientific.name')

#Filter correct data out
modern2 <- modern[is.na(modern$common.name.x), ]   
modern2 <- filter(modern2, modern2$frequency.y == 'common' | modern2$frequency.y == 'uncommon')

#Create final data for table
modern.table <- modern2 %>% 
  dplyr::select('common.name.y', 'scientific.name', 'frequency.y') %>% 
  rename('common.name'='common.name.y', 'scientific.name'='scientific.name', 'frequency'='frequency.y')



##Create another table for species with drastic increases
#Start with a new dataframe
bird.drastic <- bird.analysis

#Create a column of frequency changes greater than 1
#Make numeric
bird.drastic$freq.x <- as.numeric(bird.drastic$freq.x)
bird.drastic$freq.y <- as.numeric(bird.drastic$freq.y)

#Perform the math
bird.drastic$major.changes = bird.drastic$freq.x - bird.drastic$freq.y

#Filter by changes greater than 1
bird.drastic2 <- bird.drastic %>% 
  filter(bird.drastic$major.changes > 1 | bird.drastic$major.changes < -1) %>% 
  dplyr::select('common.name.x','scientific.name','frequency.y','frequency.x','freq.changes') %>% 
  rename('common.name'='common.name.x','frequency.1880'='frequency.y','frequency.current'='frequency.x')

#Table for drastically increased species
drastic.inc <- bird.drastic2 %>% 
  filter(bird.drastic2$freq.changes=='increased')

#Table for drastically decreased species
drastic.dec <- bird.drastic2 %>% 
  filter(bird.drastic2$freq.changes=='decreased')
  


##Plot select species
#Make separate data.frames for each species that is decreasing
AMWO <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='American Woodcock')
 
YBFL <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='Yellow-bellied Flycatcher')

BCNH <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='Black-crowned Night-Heron')

#Make separate data.frames for each species that is increasing
BTBW <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='Black-throated Blue Warbler')

EAPH <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='Eastern Phoebe')

BLJA <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='Blue Jay')


#Get base map
mdi.map <- get_stamenmap(
  bbox = c(left = -68.5, bottom = 44.17, right = -68.13, top = 44.5),
  maptype = 'terrain',
  zoom = 11)


#Plot
#BTBW
ggmap(mdi.map) +
  geom_point(data=BTBW,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = '#006699',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("Black-throated Blue Warbler Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")

#BLJA
ggmap(mdi.map) +
  geom_point(data=BLJA,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black',
             fill = '#0099FF',
             stroke = 1,
             alpha = 0.5) +
  theme_classic(base_size = 14 ) +
  ggtitle("Blue Jay Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")

#EAPH
ggmap(mdi.map) +
  geom_point(data=EAPH,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = '#996633',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("Eastern Phoebe Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")


#AMWO
ggmap(mdi.map) +
  geom_point(data=AMWO,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = '#FF9933',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("American Woodcock Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")

#BCNH
ggmap(mdi.map) +
  geom_point(data=BCNH,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = '#000000',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("Black-crowned Night-Heron Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")

#YBFL
ggmap(mdi.map) +
  geom_point(data=YBFL,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = '#FFFF66',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("Yellow-bellied Flycatcher Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")


#leaflet(BLJA) %>%
#  addTiles() %>%  
#  addCircleMarkers(
#    radius = 5,
#    color = 'blue',
#    stroke = FALSE, fillOpacity = 1
#  )



#------------------------------------------------#
####     Writing Out Files for R Markdown     ####
#------------------------------------------------##

##Write out dataframe for the bar graph
write_csv(freq.plot, paste('outputs/frequency_plot_data', '.csv', sep=''))

##Write out dataframe for the freq table
write_csv(freq.table, paste('outputs/frequency_table_data', '.csv', sep=''))

##Write out dataframes for some modern data table
write_csv(modern.table, paste('outputs/modern_table_data', '.csv', sep=''))
write_csv(modern, paste('outputs/modern_fulltable_data', '.csv', sep=''))

##Write out dataframes for the drastically changes species
write_csv(drastic.dec, paste('outputs/drasticdec_table_data', '.csv', sep=''))
write_csv(drastic.inc, paste('outputs/drasticinc_table_data', '.csv', sep=''))

##Write out plotting file
write_csv(bird.mod.locs, paste('outputs/species_plotting_data', '.csv', sep=''))

