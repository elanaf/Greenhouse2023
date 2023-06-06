#Import####
load("Cleaned_Data/main_dfs.RData")
library(tidyverse)
library(viridis)
library(patchwork)

# Cover graphs ####
##Prep####
gh <- cover_dat
gh_final <- gh %>%
  filter(gh$Date == "2023-03-01") 

##Native cover####
trt_label <- c("W" = "Present", 
               "WO" = "Absent")
gh_final %>%
  select(-Phrag) %>%
  filter(Mix != "PHAU")%>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = Total, color = Density)) +
  facet_wrap(~Phrag_Presence, labeller = as_labeller(trt_label)) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Final Total Native Cover") +
  scale_color_hue(labels = c('High', 'Low'))

ggsave("native_cover_by-mix.jpeg")

##Phrag Cover####
gh_final %>% 
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal", "PHAU"))) %>% 
  ggplot(aes(x = Mix, y = Phrag, color = Density)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Final *Phragmites* Cover")+
  scale_color_hue(labels = c('High', 'Low', 'Control')) +
  theme(axis.title.y = ggtext::element_markdown()) +
  scale_x_discrete(labels = c("Grass" = "Grass",
                              'Bulrush' = "Bulrush",
                              'Forb' = 'Forb',
                              'Equal'= "Equal",
                              'PHAU' = 'Control'))

ggsave("phrag_cover_by-mix.jpeg")

##Stacked bargraph####
###Functional group ####
####With phrag ####
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

gh_totals <- gh_final %>%
  filter(Mix != "PHAU") %>%
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE),
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = Phrag)

gh_tw <- gh_totals %>%
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

a <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal", "PHAU"))) %>% 
  filter(Phrag_Presence != "WO") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Group", 
       title = "*Phragmites* Present")+
  facet_wrap(~Mix) +
  scale_fill_manual(values = colors)+
  theme(plot.title = ggtext::element_markdown(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

#### Without phrag ####
b <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal"))) %>% 
  filter(Phrag_Presence != "W") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Proportional Cover", fill = "Group", 
       title = "*Phragmites* Absent")+
  facet_wrap(~Mix) +
  scale_fill_manual(values = colors)+
  theme(plot.title = ggtext::element_markdown())+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

b + a

ggsave("stacked_group.jpeg")

### Stacked with Species ####
####with phrag ####
gh_sl <- gh_final %>% 
  pivot_longer(
    cols = 7:16,
    names_to = "Species",
    values_to = "Cover"
  )

#colors
cp <- c("khaki3", "yellow2", "wheat2",
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

c <- gh_sl %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag"))) %>% 
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Species",
       title = "*Phragmites* Present") +
  facet_wrap(~Mix) +
  scale_fill_manual(values = cp)+
  theme(plot.title = ggtext::element_markdown(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

####no phrag ####
#colors
d <- gh_sl %>% 
  filter(Phrag_Presence != "W") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag"))) %>% 
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Proprotional Cover", fill = "Species",
       title = "*Phragmites* Absent") +
  facet_wrap(~Mix) +
  scale_fill_manual(values = cp) +
  theme(plot.title = ggtext::element_markdown())+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))
d + c
ggsave("stacked_species.jpeg")

##Reduction graph####

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
  ggplot(aes(x = Mix, y = P.Cover.Red * -1, color = Density), size = 2) +
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
  labs(y = "Proportion Reduction in *Phragmites* Cover", x = "Mix")+
  scale_color_hue(labels = c('High', 'Low'))

ggsave("phrag_cover_red.jpeg")

## Raw Data over time #### 
### Native cover ####

cover_dat %>% 
  filter(Mix != "PHAU") %>% 
  mutate(Mix = factor(Mix,
                      levels = c("Forb", "Bulrush", "Grass", "Equal"))) %>% 
  ggplot(aes(x = Date, y = Total, color = Density, shape = Phrag_Presence)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(),
        legend.title = ggtext::element_markdown()) +
  labs(y = "Total Native Cover Over Time", x = "Date",
       shape = "*Phragmites* Presence") +
  facet_wrap(~Mix) +
  scale_color_hue(labels = c('High', 'Low')) +
  scale_shape(labels = c("Present", "Absent")) 

ggsave("native_cover_over-time.jpeg")

###Phrag cover ####
#NAs for density and phrag_presence are throwing off the graph
cover_dat$Density <- as.character(cover_dat$Density)
cover_dat$Phrag_Presence <- as.character(cover_dat$Phrag_Presence)
cover_dat$Density[is.na(cover_dat$Density)] <- "Control"
cover_dat$Phrag_Presence[is.na(cover_dat$Phrag_Presence)] <- "Control"
cover_dat$Density <- as.factor(cover_dat$Density)
cover_dat$Phrag_Presence <- as.factor(cover_dat$Phrag_Presence)

cp <-  c("#F8766D", "#619CFF", "#00BA38")

trt_label <- c("PHAU" = "Control",
               "Grass" = "Grass",
               "Bulrush" = "Bulrush",
               "Forb" = "Forb",
               "Equal" = "Equal")

cover_dat %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Mix = factor(Mix,
                      levels = c("Forb", "Bulrush", "Grass", "Equal", "PHAU"))) %>% 
  mutate(Density = factor(Density,
                      levels = c("H", "L", "Control"))) %>% 
  ggplot(aes(x = Date, y = Phrag, color = Density)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown()) +
  labs(y = "*Phragmites* Cover Over Time", x = "Date") +
  facet_wrap(~Mix, labeller = as_labeller(trt_label)) +   
  scale_color_manual(values = c("H" = "#F8766D", "L" = "#619CFF", "Control" = "#00BA38"),
                     labels = c("High", "Low", "Control"))

ggsave("phrag_cover_over-time.jpeg")

# Biomass graphs ####

##Native biomass####
trt_label <- c("W" = "Present", 
               "WO" = "Absent")

biomass_dat %>%
  group_by(Mix, Density, Phrag_Presence, Replicate) %>% 
  summarize(PHAU = PHAU,
            Total_Native = sum(EUMA,SOCA, EUOC, SCAC,
                               SCAM, DISP, MUAS, PUNU,BOMA, na.rm = TRUE)) %>% 
  select(-PHAU) %>%
  filter(Mix != "PHAU")%>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = Total_Native, color = Density)) +
  facet_wrap(~Phrag_Presence, labeller = as_labeller(trt_label)) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Total Native Biomass")+
  scale_color_hue(labels = c('High', 'Low'))

ggsave("native_biomass_by-mix.jpeg")

##Phrag Biomass####
biomass_dat %>% 
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal", "PHAU"))) %>% 
  ggplot(aes(x = Mix, y = PHAU, color = Density)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "*Phragmites* Biomass")+
  scale_color_hue(labels = c('High', 'Low', 'Control'))+
  scale_x_discrete(labels = c("Grass" = "Grass",
                              'Bulrush' = "Bulrush",
                              'Forb' = 'Forb',
                              'Equal'= "Equal",
                              'PHAU' = 'Control')) +
  theme(axis.title.y = ggtext::element_markdown())

ggsave("phrag_biomass_by-mix.jpeg")

##Stacked bargraph####

###by functional group ####

####With phrag ####
colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
biomass_t <- biomass_dat %>%
  filter(Mix != "PHAU") %>%
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE),
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = PHAU)

biomass_l <- biomass_t %>%
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

b <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal", "PHAU"))) %>% 
  filter(Phrag_Presence != "WO") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Group",
       title = "*Phragmites* Present")+
  facet_wrap(~Mix) +  
  theme(plot.title = ggtext::element_markdown(),
                            legend.key.size = unit(.25, "cm"),
                            legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))+
  scale_fill_manual(values = colors)

#### Without phrag ####
a <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c("Grass", "Bulrush", "Forb", "Equal"))) %>% 
  filter(Phrag_Presence != "W") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Proportional Biomass", fill = "Group",
       title = "*Phragmites* Absent")+
  facet_wrap(~Mix) +
  theme(plot.title = ggtext::element_markdown())+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))+
  scale_fill_manual(values = colors)

a + b
ggsave("stacked_biomass_group.jpeg")

### Stacked with Species ####
####with phrag ####
biomass_sl <- biomass_dat %>% 
  pivot_longer(
    cols = 5:14,
    names_to = "Species",
    values_to = "Biomass"
  )

#colors
cp <- c("khaki3", "yellow2", 
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

d <- biomass_sl %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU"))) %>% 
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Species",
       title = "*Phragmites* Present") +
  facet_wrap(~Mix) +
  scale_fill_manual(values = cp) +  
  theme(plot.title = ggtext::element_markdown(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

####no phrag ####
#colors
c <- biomass_sl %>% 
  filter(Phrag_Presence != "W") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU"))) %>% 
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Proprotional Biomass", fill = "Species",
       title = "*Phragmites* Absent") +
  facet_wrap(~Mix) +
  scale_fill_manual(values = cp) +  
  theme(plot.title = ggtext::element_markdown()) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

c + d

ggsave("stacked_biomass_species.jpeg")

##Reduction graph####
#I averaged across all the treatments/controls first because this was fully randomized - no blocks

#get the treatment PHAU values
b <- biomass_dat %>%
  filter(Phrag_Presence == "W") %>%
  select(Mix, Density, Phrag_Presence, Replicate, PHAU)

#get the control values
final.matrix <- biomass_dat %>% 
  filter(Mix == "PHAU") %>%
  select(PHAU)

c <- mean(final.matrix$PHAU)

#calculate the percent cover across all 
final.df <- b %>%
  mutate(P.Biomass.Red = (PHAU - c)/c)

#NA should be 100% because no PHAU grew but it was supposed to - final cover was <1
final.df[29, 5] <- 0
final.df[29, 6] <- -1

#graph
final.df %>%
  mutate(Mix = factor(Mix,
                      levels = c("Forb", "Bulrush", "Grass", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = P.Biomass.Red*-1, color = Density), size = 2) +
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
  labs(y = "Reduction in *Phragmites* Biomass", x = "Mix")+
  scale_color_hue(labels = c('High', 'Low'))

ggsave("phrag_biomass_red.jpeg")
