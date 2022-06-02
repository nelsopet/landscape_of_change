#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(googledrive)
require(ggplot2)
require(dplyr)
require(doBy)
require(ggmap)
require(cowplot)




#------------------------------------------------#
####     Grabbing dataframes for analysis     ####
#------------------------------------------------#

##Type "1" in command line if to reactivate API token from Google Drive or enter 0 to obtain your own token linked to your account
#Bring in the processed cs bird data from Google Drive
drive_download((drive_find(pattern = 'csbirds_processed', n_max=1)), path = 'data/csbirds_processed_readin.csv', overwrite = TRUE)
bird.his.analysis <- read.csv('data/csbirds_processed_readin.csv', header = TRUE)

#Bring in the processed eBird data from Google Drive
drive_download((drive_find(pattern = 'ebird_processed', n_max=1)), path = 'data/ebird_processed_readin.csv', overwrite = TRUE)
drive_download((drive_find(pattern = 'ebird_mappingloc', n_max=1)), path = 'data/ebird_mappingloc_readin.csv', overwrite = TRUE)
bird.mod.analysis <- read.csv('data/ebird_processed_readin.csv', header = TRUE)
bird.mod.locs <- read.csv('data/ebird_mappingloc_readin.csv', header = TRUE)

#Download JPEGS for Rmd
drive_download((drive_find(pattern = 'BTBW', n_max=1)), path = 'outputs/BTBW.jpg')
drive_download((drive_find(pattern = 'BLJA', n_max=1)), path = 'outputs/BLJA.jpg')
drive_download((drive_find(pattern = 'EAPH', n_max=1)), path = 'outputs/EAPH.jpg')
drive_download((drive_find(pattern = 'AMWO', n_max=1)), path = 'outputs/AMWO.jpg')
drive_download((drive_find(pattern = 'BCNH', n_max=1)), path = 'outputs/BCNH.jpg')
drive_download((drive_find(pattern = 'YBFL', n_max=1)), path = 'outputs/YBFL.jpg')
drive_download((drive_find(pattern = 'HOSP', n_max=1)), path = 'outputs/HOSP.jpg')



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
bird.analysis$freq.changes <- ifelse(bird.analysis$freq.x == bird.analysis$freq.y, 'no change', 
                                    ifelse(bird.analysis$freq.y > bird.analysis$freq.x, 'increased', "decreased"))

#Remove the migrant only species for now
bird.analysis2 <- bird.analysis #%>% filter(freq.y < 4)

#Add in values for species that were not captured in modern spreadsheet
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Antrostomus vociferus"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Progne subis"] <- 'decreased'
bird.analysis2['freq.changes'][bird.analysis2['scientific.name'] == "Ectopistes migratorius"] <- 'decreased'
bird.analysis2['freq.x'][bird.analysis2['scientific.name'] == "Antrostomus vociferus"] <- '5'
bird.analysis2['freq.x'][bird.analysis2['scientific.name'] == "Petrochelidon pyrrhonota"] <- '5'
bird.analysis2['freq.x'][bird.analysis2['scientific.name'] == "Progne subis"] <- '5'
bird.analysis2['freq.x'][bird.analysis2['scientific.name'] == "Ectopistes migratorius"] <- '5'



##Plot
#Change the data for the plot to only be for notable spp.
freq.plot.pre <- bird.analysis2

freq.plot.pre$freq.x <-as.numeric(freq.plot.pre$freq.x)
freq.plot.pre$freq.y <-as.numeric(freq.plot.pre$freq.y)

freq.plot.pre$math <- freq.plot.pre$freq.x - freq.plot.pre$freq.y
freq.plot.pre$math <- gsub("-", "", freq.plot.pre$math)

freq.plot.pre$notable <- ifelse(freq.plot.pre$math < 2, 'not dis', "notable")

freq.plot.sub <- subset(freq.plot.pre, (freq.plot.pre$notable == 'notable'))
freq.plot.sub2 <- subset(freq.plot.pre, (freq.plot.pre$notable != 'notable'))
freq.plot.sub2$freq.changes <- "no change"

freq.plot.ready <- rbind(freq.plot.sub2, freq.plot.sub)

  
#Create new dataframe to store the counts of each frequency change
freq.plot <- freq.plot.ready %>% 
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
  guides(fill = "none") +
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
freq.table['frequency.current'][freq.table['scientific.name'] == "Antrostomus vociferus"] <- 'very rare'
freq.table['frequency.current'][freq.table['scientific.name'] == "Petrochelidon pyrrhonota"] <- 'very rare'
freq.table['frequency.current'][freq.table['scientific.name'] == "Progne subis"] <- 'very rare'
freq.table['frequency.current'][freq.table['scientific.name'] == "Ectopistes migratorius"] <- 'extinct'


#Create table of the percent of each frequency status
p.1880 <- freq.table %>%
  as_tibble() %>% 
  select(frequency = frequency.1880) %>% 
  group_by(frequency) %>% 
  summarise(percent.1880 = 100*(length(frequency)/98), .groups = "drop")

p.curr <- freq.table %>%
  as_tibble() %>% 
  select(frequency = frequency.current) %>% 
  group_by(frequency) %>% 
  summarise(percent.current = 100*(length(frequency)/98), .groups = "drop")

p.curr %>% 
  left_join(p.1880, by = "frequency") %>% 
  select("Frequency status" = frequency, "1880 %" = percent.1880, "Modern %" = percent.current) %>% 
  arrange(., c("common", "uncommon", "rare", "very rare", "extinct")) #%>% 
  #write.csv("outputs/percent_table.csv", row.names = F)



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



##Table for drastically increased species
drastic.incr <- bird.drastic2 %>% 
  filter(bird.drastic2$freq.changes=='increased')

drastic.mod <- as.data.frame(rep("not detected", times = 52))
drastic.mod.in <- as.data.frame(rep("increased", times = 52))
colnames(drastic.mod) <- "frequency.1880"
colnames(drastic.mod.in) <- "freq.changes"

drastic.mod2 <- cbind(modern.table, drastic.mod)
drastic.mod2 <- cbind(drastic.mod2, drastic.mod.in)

drastic.mod2 <- drastic.mod2 %>% 
  dplyr::select('common.name','scientific.name','frequency.1880','frequency','freq.changes') %>% 
  rename('frequency.current'='frequency')

drastic.inc <- rbind(drastic.incr, drastic.mod2)



##Table for drastically decreased species
drastic.dec <- bird.drastic2 %>% 
  filter(bird.drastic2$freq.changes=='decreased')
  
missers1 <- c("Passenger Pigeon", "Ectopistes migratorius", "rare", "extinct",	"decreased")
missers2 <- c("Cliff Swallow", "Petrochelidon pyrrhonota", "common", "very rare", "decreased")

mat1 <- data.frame(t(sapply(missers1,c)))
colnames(mat1) <- c("common.name",'scientific.name','frequency.1880','frequency.current','freq.changes')
mat2 <- data.frame(t(sapply(missers2,c)))
colnames(mat2) <- c("common.name",'scientific.name','frequency.1880','frequency.current','freq.changes')

missers <- rbind(mat1, mat2)

drastic.dec <- rbind(missers, drastic.dec) %>% 
  doBy::order_by('common.name')




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

HOSP <- bird.mod.locs %>% 
  filter(bird.mod.locs$common.name=='House Sparrow')

#Get base map
mdi.map <- get_stamenmap(
  bbox = c(left = -68.5, bottom = 44.17, right = -68.13, top = 44.5),
  maptype = 'toner-lite',
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


#HOSP
ggmap(mdi.map) +
  geom_point(data=HOSP,
             aes(x=longitude, y=latitude),
             shape = 21,
             size = 2.5,
             color = 'black', 
             fill = 'black',
             stroke = 1,
             alpha = 0.6) +
  theme_classic(base_size = 14 ) +
  ggtitle("House Sparrow Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude")



#------------------------------------------------#
####     Writing Out Files for R Markdown     ####
#-----------------------------------------------##

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



#------------------------------------------------#
####        Export Tables and Figures         ####
#-----------------------------------------------##

# #Figure 1
# freq.plot.ex <- read.csv('outputs/frequency_plot_data.csv', header = TRUE)
# 
# png(filename="outputs/bird_figure_1.png", width=600, height=600)
# freq.plot.ex %>% 
#   ggplot(aes(freq.changes, species.number, fill = freq.changes)) +
#   geom_bar(stat="identity", color="black") +
#   ggtitle("Species Frequency Changes from 1880 to Present") + 
#   geom_text(aes(label = species.number, vjust = 2)) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = c(0,0), limits = c(0,80)) +
#   guides(fill = 'none') +
#   labs(x = "change in frequency", y = "number of species") +
#   theme_classic(base_size=14) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(values=c("forestgreen", "darkslategray4", "darkorange3"))
# dev.off()
# 
# #Figure 2
# png(filename="outputs/bird_figure_2.png", width=1200, height=600)
# map1 <- ggmap(mdi.map) +
#   geom_point(data=BTBW,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black', 
#              fill = '#006699',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x="", y="")
# ggdraw() +
#   draw_image("outputs/BTBW.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map1)
# dev.off()
# 
# #Figure 3
# png(filename="outputs/bird_figure_3.png", width=1200, height=600)
# map2 <- ggmap(mdi.map) +
#   geom_point(data=BLJA,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black',
#              fill = '#0099FF',
#              stroke = 1,
#              alpha = 0.5) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/BLJA.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map2)
# dev.off()
# 
# #Figure 4
# png(filename="outputs/bird_figure_4.png", width=1200, height=600)
# map3 <- ggmap(mdi.map) +
#   geom_point(data=EAPH,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black', 
#              fill = '#996633',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/EAPH.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map3)
# dev.off()
# 
# #Figure 5
# png(filename="outputs/bird_figure_5.png", width=1200, height=600)
# map4 <- ggmap(mdi.map) +
#   geom_point(data=AMWO,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black', 
#              fill = '#FF9933',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/AMWO.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map4)
# dev.off()
# 
# #Figure 6
# png(filename="outputs/bird_figure_6.png", width=1200, height=600)
# map5 <- ggmap(mdi.map) +
#   geom_point(data=BCNH,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black', 
#              fill = '#000000',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/BCNH.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map5)
# dev.off()
# 
# #Figure 7
# png(filename="outputs/bird_figure_7.png", width=1200, height=600)
# map6 <- ggmap(mdi.map) +
#   geom_point(data=YBFL,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black', 
#              fill = '#FFFF66',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/YBFL.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map6)
# dev.off()

# #Figure 7
# png(filename="outputs/house_sparrow_map.png", width=1200, height=600)
# map6 <- ggmap(mdi.map) +
#   geom_point(data=HOSP,
#              aes(x=longitude, y=latitude),
#              shape = 21,
#              size = 2,
#              color = 'black',
#              fill = 'black',
#              stroke = 1,
#              alpha = 0.6) +
#   theme_classic(base_size = 14 ) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1.5)) +
#   labs(x = "", y = "")
# ggdraw() +
#   draw_image("outputs/HOSP.jpg",  x = 0.34, y = 0, scale = .4) +
#   draw_plot(map6)
# dev.off()

# 
# #Table 1
# inc.table <- drastic.inc
# colnames(inc.table) <- c('Common name', 'Scientific name', "R. a. 1880's", 'R. a. modern', 'R. a. changes')
# write_csv(inc.table, paste('outputs/bird_table_1', '.csv', sep=''))
# 
# #Table 2
# dec.table <- drastic.dec
# colnames(dec.table) <- c('Common name', 'Scientific name', "R. a. 1880's", 'R. a. modern', 'R. a. changes')
# write_csv(dec.table, paste('outputs/bird_table_2', '.csv', sep=''))
# 
# #Table 3
# colnames(modern.table) <- c('Common name', 'Scientific name', "Relative abundance")
# write_csv(modern.table, paste('outputs/bird_table_3', '.csv', sep=''))
# 
# #Table 4
# freq.table2 <- freq.table %>% 
#   select(-c('frequency.changes'))
# colnames(freq.table2) <- c('Common name', 'Scientific name', "R. a. 1880's", 'R. a. modern')
# write_csv(freq.table2, paste('outputs/bird_table_4', '.csv', sep=''))








