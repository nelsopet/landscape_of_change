#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(doBy)


#------------------------------------------------#
####     Grabbing dataframes for analysis     ####
#------------------------------------------------#

#Bring in the processed cs bird data from Google Drive
drive_download((drive_find(pattern = 'csbirds_processed', n_max=1)), path = 'data/csbirds_processed_readin.csv', overwrite = TRUE)
bird.his.analysis <- read.csv('data/csbirds_processed_readin.csv', header = TRUE)

#Bring in the processed ebird data from Google Drive
drive_download((drive_find(pattern = 'ebird_processed', n_max=1)), path = 'data/ebird_processed_readin.csv', overwrite = TRUE)
bird.mod.analysis <- read.csv('data/ebird_processed_readin.csv', header = TRUE)



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
freq.table['frequency.current'][freq.table['scientific.name'] == "Antrostomus vociferus"] <- 'not present'
freq.table['frequency.current'][freq.table['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'not present'
freq.table['frequency.current'][freq.table['scientific.name'] == "Progne subis"] <- 'not present'


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


#------------------------------------------------#
####     Writing Out Files for R Markdown     ####
#------------------------------------------------##

##Write out dataframe for the bar graph
#write_csv(freq.plot, paste('outputs/frequency_plot_data', '.csv', sep=''))

##Write out dataframe for the freq table
#write_csv(freq.table, paste('outputs/frequency_table_data', '.csv', sep=''))

##Write out dataframe for the freq table
#write_csv(modern.table, paste('outputs/modern_table_data', '.csv', sep=''))
#write_csv(modern, paste('outputs/modern_fulltable_data', '.csv', sep=''))

