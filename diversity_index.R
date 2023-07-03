#Import####
load("Cleaned_Data/main_dfs.RData")
library(vegan)
library(tidyverse)

#the general equation is vegan::diversity(x, "shannon")
#Examples ####
diversity(final.df$Replicate, "shannon")
diversity(final.df$PHAU, "shannon")

#make wide
View(cover_dat)

#only want the final cover
final.dat <- cover_dat %>% 
  filter(Date == "2023-03-01")

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Mix', 'Density', 'Phrag_Presence', 'Replicate'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#make all percentages
final.dat2 <- mutate_if(final.dat2, is.numeric, ~.*100)

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- select(final.dat3, -c("Tub", 'Date', 'Total', 'Notes'))
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#compare the results
final.dat %>% 
  select('Mix', 'Phrag_Presence', "Density", "shannon") %>% 
  arrange('Mix', 'Phrag_Presence', "Density")

#Now make a box and whisper of the results to compare
#Calculate the evenness?
