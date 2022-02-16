#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(googledrive)

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
  dplyr::select('order.x','super.family.x','family','Freq.x','Freq.y') %>% 
  rename('order'='order.x','super.family'='super.family.x','frequency.1900s'='Freq.x','frequency.current'='Freq.y')

#Fill in na with 0
insect.counts2[is.na(insect.counts2)] <- 0

#Final table for Rmd
insect.table <- insect.counts2



##Create dataframe for ggplot
#Split modern
insect.counts2b <- insect.counts2 %>% 
  select('family','frequency.current')

#add column for time period and rename
insect.counts.mod <- cbind(insect.counts2b, data.frame(rep(c('modern day'),times=54))) 
colnames(insect.counts.mod) <- c('family', 'freq', 'period')


#Split historical
insect.counts2c <- insect.counts2 %>% 
  select('family','frequency.1900s')

#add column for time period and rename
insect.counts.his <- cbind(insect.counts2c, data.frame(rep(c('historical'),times=54))) 
colnames(insect.counts.his) <- c('family', 'freq', 'period')



##Join for final dataset
insect.plot <- rbind(insect.counts.his, insect.counts.mod)


##Create the ggplot
png(filename="outputs/insect_freq_changes.png", width=1200, height=1500)

insect.plot %>% 
  ggplot(aes(freq, family, fill = period)) +
  geom_bar(stat="identity", color="black", width=.85, position=position_dodge()) +
  ggtitle("Species Totals by Family") + 
  labs(y = "Family", x = "Number of Species") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(0,300)) +
  scale_fill_brewer(name="Legend", palette = 'Paired') +
  theme_classic(base_size = 30) +
  scale_y_discrete(limits=rev) +
  theme(plot.title = element_text(hjust = 0.5, size = 40), axis.text = element_text(size=20, color = "black"))

dev.off()




#------------------------------------------------#
####     Writing Out Files for R Markdown     ####
#------------------------------------------------#

###Write out dataframe for the bar graph
write_csv(freq.plot, paste('outputs/frequency_plot_data', '.csv', sep=''))

