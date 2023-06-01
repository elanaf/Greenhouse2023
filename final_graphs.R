##Notes for improving graphs in the future:
#Karin wants them in the order: grass - bulriush - forb - equal
#I need graphs of the raw data over time
#Include species in the stacked bar charts - Jes has code for this - might be interesting if there's a pattern in the forbs

#Import####
load("Cleaned_Data/main_dfs.RData")
library(tidyverse)
library(viridis)

#Prep####
gh <- cover_dat
gh_final <- gh %>%
  filter(gh$Date == "2023-03-01") 

#Native cover####
gh_final %>%
  select(-Phrag) %>%
  filter(Mix != "PHAU")%>%
  ggplot(aes(x = Mix, y = Total, color = Density)) +
  facet_wrap(~Phrag_Presence) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Total Native Cover")

#Phrag Cover####
gh_final %>% 
  ggplot(aes(x = Mix, y = Phrag, color = Density)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Phragmites cover")

#Stacked bargraph####
gh_totals <- gh_final %>%
  filter(Mix != "PHAU") %>%
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU),
            Forb = sum(EUOC, EUMA, SOCA),
            Bulrush = sum(SCAM, SCAC, BOMA),
            Phragmites = Phrag)

gh_tw <- gh_totals %>%
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

gh_tw %>%
  filter(Phrag_Presence != "WO") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "Proportional Cover", fill = "Group")+
  facet_wrap(~Mix) 

ggsave("Stacked_Bargraph.jpeg")

#Reduction graph####

#I averaged across all the treatments/controls first because this was fully randomized - no blocks

#get the treatment PHAU values
b <- gh_final %>%
  filter(Phrag_Presence == "W") %>%
  select(Mix, Density, Phrag_Presence, Replicate, Phrag)

#get the control values
final.matrix <- gh_final %>% 
  filter(Mix == "PHAU") %>%
  select(Mix, Replicate, Total)

c <- mean(final.matrix$Total)

#calculate the percent cover across all 
final.df <- b %>%
  mutate(P.Cover.Red = (Phrag - c)/c)

#graph
final.df %>%
  mutate(Mix = factor(Mix,
                      levels = c("Forb", "Bulrush", "Grass", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = P.Cover.Red, color = Density), size = 2) +
  stat_summary(aes(group = Density),
               size = 2,
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               size = 1,
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown()) +
  labs(y = "Percent Change in *Phragmites* Cover", x = "Mix")

ggsave("Reduction_graph_23.jpeg")
