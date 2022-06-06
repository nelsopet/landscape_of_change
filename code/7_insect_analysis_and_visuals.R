#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(googledrive)
require(foreign)

select <- dplyr::select


#------------------------------------------------#
####     Grabbing dataframes for analysis     ####
#------------------------------------------------#

##Type "1" in command line if to reactivate API token from Google Drive or enter 0 to obtain your own token linked to your account
#Bring in the processed cs bird data from Google Drive
drive_download((drive_find(pattern = 'proctorinsect_processed', n_max=1)), path = 'data/proctorinsect_processed_readin.csv', overwrite = TRUE)
insect.his.analysis <- read.csv('data/proctorinsect_processed_readin.csv', header = TRUE)

#Bring in the processed ebird data from Google Drive
drive_download((drive_find(pattern = 'inatinsect_processed', n_max=1)), path = 'data/inatinsect_processed_readin.csv', overwrite = TRUE)
insect.mod.analysis <- read.csv('data/inatinsect_processed_readin.csv', header = TRUE)

#Get bombus data for comparisons
drive_download((drive_find(pattern = 'bombus_modern', n_max=1)), path = 'data/bombus_modern_readin.csv', overwrite = TRUE)
bombus.mod <- read.csv('data/bombus_modern_readin.csv', header = TRUE)

#Get a .png for the R Markdown from Google Drive
drive_download((drive_find(pattern = 'LandscapeOfChange', n_max=1)), path = 'outputs/loc_logo.png')

#Read in proctor mapping data
map.1 <- read.dbf('data/apidae_proctor_1900s.dbf')
map.2 <- read.dbf('data/lepidoptera_proctor_1900s.dbf')



#------------------------------------------------#
####   Analysis and Creating visualizations   ####
#------------------------------------------------#

###Create a count of species in each family
##Historical
#Create the count column
his.freq <- as.data.frame(table(insect.his.analysis$family)) %>% 
  rename('family'='Var1')

#Gather taxonomic columns for merge
his.simp <- insect.his.analysis %>% 
  dplyr::select('order','super.family','family')

#Merge with the count data
his.count.ALL <- merge(his.simp, his.freq, by = 'family')

#Clean a final family list with total species for each
his.count <- his.count.ALL[!duplicated(his.count.ALL$family), ] 



##Modern
#Create the count column
mod.freq <- as.data.frame(table(insect.mod.analysis$family)) %>% 
  rename('family'='Var1')

#Gather taxonomic columns for merge
mod.simp <- insect.mod.analysis %>% 
  dplyr::select('order','super.family','family')

#Merge with the count data
mod.count.ALL <- merge(mod.simp, mod.freq, by = 'family')

#Clean a final family list with total species for each
mod.count <- mod.count.ALL[!duplicated(mod.count.ALL$family), ] 


##Merge the historic and modern data for comparison
#join by family
insect.counts <- left_join(his.count, mod.count, by = 'family')

#filter and rename
insect.counts2 <- insect.counts %>% 
  select('order.x','super.family.x','family','Freq.x','Freq.y') %>% 
  rename('order'='order.x','super.family'='super.family.x','frequency.1900s'='Freq.x','frequency.current'='Freq.y')

#Fill in na with 0
insect.counts2[is.na(insect.counts2)] <- 0



##Create data for ggplot and make the ggplot
#Split modern
insect.counts2b <- insect.counts2 %>% 
  select('family','frequency.current')

#add column for time period and rename
insect.counts.mod <- cbind(insect.counts2b, data.frame(rep(c('modern day'), times=54))) 
colnames(insect.counts.mod) <- c('family', 'freq', 'period')

#Split historical
insect.counts2c <- insect.counts2 %>% 
  select('family','frequency.1900s')

#add column for time period and rename
insect.counts.his <- cbind(insect.counts2c, data.frame(rep(c('historical'), times=54))) 
colnames(insect.counts.his) <- c('family', 'freq', 'period')

#Join for final dataset
insect.plot <- rbind(insect.counts.his, insect.counts.mod)

#Create the ggplot
insect.plot %>% 
  ggplot(aes(freq, family, fill = period)) +
  geom_bar(stat="identity", color="black", width=.91, position=position_dodge()) +
  ggtitle("Species Diversity by Family") + 
  labs(y = "Family", x = "Number of species") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(0,300)) +
  scale_fill_brewer(name="Legend", palette = 'Paired', direction = -1) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(plot.title = element_blank(), 
        plot.margin = margin(.5,1.1,.5,.3, "cm"),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.88, 0.85))

ggsave("outputs/test_plot.png", height = 11, width = 8.5)



##Create data frame for a table to see how many species from Proctor were recorded today
#Left join by scientific name
mod.proc <- left_join(insect.mod.analysis, insect.his.analysis, by = 'scientific.name') %>% 
  select('order.x','order.y','super.family.x','family.x','genus.x','scientific.name','common.name') %>% 
  rename('super.family'='super.family.x','family'='family.x','genus'='genus.x') %>% 
  arrange('super.family')



##Create dataframe for looking at the species of Noctuidae, Geometridae, Torticidae, and Apidae
#iNat data
inat.sel <- insect.mod.analysis %>% 
  filter(family=='Noctuidae' | family=="Sphingidae" | family=='Geometridae' | family=='Apidae' | 
           family=='Lycaenidae' | family=='Nymphalidae' | family=='Papilionidae' | family=='Pieridae' |
           family=='Hesperiidae')
inat.sel <- inat.sel[order(inat.sel$family), ]

#Proctor data
proc.sel <- insect.his.analysis %>% 
  filter(family=='Noctuidae' | family=="Tortricidae" | family=='Geometridae' | family=='Apidae'| 
           family=='Lycaenidae' | family=='Nymphalidae' | family=='Papilionidae' | family=='Pieridae' |
           family=='Hesperiidae') %>% 
  select(-c('name.synonyms','locality'))
proc.sel <- proc.sel[order(proc.sel$family), ]

#Left join
sel.fam.all <- left_join(inat.sel, proc.sel, by = 'scientific.name')

length(which(sel.fam.all$order.y!='na')) #115 in common
length(which(is.na(sel.fam.all$order.y))) #34 species not recored by proctor
# length(which(proc.sel$family=='Noctuidae')) #298, 21.9%
# length(which(inat.sel$family=='Noctuidae')) #63, 17.4%
# length(which(proc.sel$family=='Tortricidae')) #189 13.9%
# length(which(inat.sel$family=='Tortricidae')) #24 6.6%
# length(which(proc.sel$family=='Geometridae')) #186 13.7%
# length(which(inat.sel$family=='Geometridae')) #52 14.4%
# length(which(proc.sel$family=='Apidae')) #25 1.8%
# length(which(inat.sel$family=='Apidae')) #10 2.8%
# 
# 
# proc.noct <- proc.sel %>% 
#   filter(family=='Noctuidae')
# inat.noct <- inat.sel %>% 
#   filter(family=='Noctuidae')
# unique(proc.noct$genus)
# unique(inat.noct$genus)




###Create data frame for bombus table
#Filter historic data for bombus
bombus.his <- insect.his.analysis %>% 
  filter(genus=='Bombus') %>% 
  select('scientific.name') %>% 
  filter(scientific.name!='Bombus fernaldae')

#Add in common names
bombus.his$common.name <- c('Rusty-patched bumble bee', "Ashton's cuckoo bumble bee", 'Two-spotted bumble bee',
                            'Northern amber bumble bee', 'Lemon cuckoo-bumble bee', 'Yellowish cuckoo-bumble bee',
                            'Confusing bumble bee', 'Tri-colored bumble bee', 'Yellow-banded bumble bee', 'Half-black bumble bee')

##Remove the geometry column
#Split into separate vectors
bombus.mod.a <- as.data.frame(bombus.mod[,1])
bombus.mod.b <- as.data.frame(bombus.mod[,2])

#recombine and fix colnames
bombus.mod2 <- cbind(bombus.mod.b, bombus.mod.a)
colnames(bombus.mod2) <- c('scientific.name', 'common.name')

##Make final table
#Make columns for presence
bombus.mod2$present.mod <- 'yes'
bombus.his$present.1900 <- 'yes'

#Combine
bombus.comb <- full_join(bombus.his, bombus.mod2, by = 'scientific.name') %>% 
  select('common.name.x','scientific.name','present.1900','present.mod') %>% 
  rename('common.name'='common.name.x')

#Fill in common names
bombus.comb['common.name'][bombus.comb['scientific.name'] == "Bombus impatiens"] <- 'Common eastern bumble bee'
bombus.comb['common.name'][bombus.comb['scientific.name'] == "Bombus sandersoni"] <- "Sanderson's bumble bee"


#Fill na's to finalize
bombus.comb[is.na(bombus.comb)] <- 'no'

#Add iNaturalist bumbles
inat.bombus <- insect.mod.analysis %>% 
  filter(scientific.name=='Bombus griseocollis') %>% 
  select('common.name','scientific.name')

inat.bombus['common.name'][inat.bombus['scientific.name'] == "Bombus griseocollis"] <- 'Brown-belted bumble bee'
inat.bombus$present.1900 <- 'no'
inat.bombus$present.mod <- 'yes'

bombus.comb2 <- rbind(bombus.comb, inat.bombus)
  
bombus.comb2 <- bombus.comb2 %>% 
  arrange('common.name')





#------------------------------------------------#
####     Writing Out Files for R Markdown     ####
#------------------------------------------------#

#Write out dataframe for the summary numbers and first table
write_csv(mod.proc, paste('outputs/modernproctor_comparison_data', '.csv', sep=''))

#Write out dataframes for pollinator data
write_csv(inat.sel, paste('outputs/inat_pollinator_data', '.csv', sep=''))
write_csv(proc.sel, paste('outputs/proctor_pollinator_data', '.csv', sep=''))
write_csv(sel.fam.all, paste('outputs/all_pollinator_data', '.csv', sep=''))
write_csv(bombus.comb2, paste('outputs/bombus_table_data', '.csv', sep=''))



#------------------------------------------------#
####        Export Tables and Figures         ####
#------------------------------------------------#

# #Table 1
# mod.not.proc <- mod.proc %>% 
#   filter(is.na(order.y)) %>% 
#   select('order.x','super.family','family','scientific.name','common.name') %>% 
#   rename('order'='order.x')
# colnames(mod.not.proc) <- c('Order', 'Super family', 'Family', 'Scientific name', 'Common name')
# mod.not.proc <- mod.not.proc %>% 
#   arrange(desc(Order))
# write_csv(mod.not.proc, paste('outputs/insect_table_1', '.csv', sep=''))
# 
# #Table 2
# bombus.table <- bombus.comb2
# colnames(bombus.table) <- c('Common name', 'Scientific name', 'Present 1900', 'Present modern')
# write_csv(bombus.table, paste('outputs/insect_table_2', '.csv', sep=''))
# 
# #Figure 1 was already exported in the code above.


