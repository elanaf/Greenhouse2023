#Import####
load("Cleaned_Data/main_dfs.RData")
library(vegan)
library(tidyverse)

#the general equation is vegan::diversity(x, "shannon")
#Examples ####
diversity(final.df$Replicate, "shannon")
diversity(final.df$PHAU, "shannon")

#Real thing now ####
#make wide
#View(cover_dat)

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

#Plot ####
#change order of phrag presence and also labels
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                 labels = c("Absent", "Present"))

final.dat %>% 
  filter(Mix != "PHAU") %>% 
  ggplot(aes(x = Mix, y = shannon, fill = Density)) +
  geom_boxplot() +
  facet_wrap(~Phrag_Presence) +
  ylab("Shannon Diversity Index") +
  scale_fill_hue(labels = c('High', 'Low')) #change legend labels

ggsave("cover_shannon.jpeg")

# Biomass ####
#make wide
View(biomass_dat)

final.dat <- biomass_dat

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Mix', 'Density', 'Phrag_Presence', 'Replicate'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- select(final.dat3, -"Tub")
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#Plot ####
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                   labels = c("Absent", "Present"))
final.dat %>% 
  filter(Mix != "PHAU") %>% 
  ggplot(aes(x = Mix, y = shannon, fill = Density)) +
  geom_boxplot() +
  facet_wrap(~Phrag_Presence) +
  ylab("Shannon Diversity Index")+
  scale_fill_hue(labels = c('High', 'Low')) #change legend labels

ggsave("biomass_shannon.jpeg")