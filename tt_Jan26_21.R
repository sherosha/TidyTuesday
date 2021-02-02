---
  title: "TT-PlasticPollution"
author: "Sherosha Raj"
date: "26/01/2021"

---
  
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load('2021-01-26')

plastics <- tuesdata$plastics

head(plastics)



# Data needs to be tidy format->use pivot_longer to group by plastic type/count.
##use data for pet,pp and ps type plastics only

#remove grand total rows b/c looking only at specific plastics
plastics_tidy<- plastics %>% 
  filter(parent_company !="Grand Total") %>% 
  gather("type","count",8:10) %>%  
  select(country,year,type,count) %>% 
  filter(type==c("pet","pp","ps")) %>% 
  mutate(country=str_replace(country,"United Kingdom of Great Britain & Northern Ireland","UK")) %>% 
  mutate(country=str_replace(country,"United States of America","USA"))

#only year 2020

takeout_2020 <- plastics_tidy %>%
  filter(year == "2020") %>% 
  group_by(country,type) %>% 
  summarise(type_total=sum(count)) %>% 
  mutate(TOTAL = ifelse(row_number() == n(), sum(type_total) ,NA)) 

#take out NAs and filter for over 150 pieces
forplot<-takeout_2020 %>% 
  drop_na %>% 
  arrange(desc(TOTAL)) %>%  
  filter(TOTAL > 150)

#plot
plasticplot <-ggplot(forplot,aes(fct_reorder(country,TOTAL), 
                                 TOTAL))+
  geom_col(fill = "#D55E00")+
  theme_minimal()+
  labs(title="Countries with combined PP,PET & PS plastics collected in the year 2020",
       subtitle=">150 pieces collected",
       caption =  "Break Free from Plastic | Image: @SheroshaR") +
  theme(axis.title.y=element_blank())+
  coord_flip()

library(treemap)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plastictreemap <-treemap(forplot,
                         
                         # data
                         index="country",
                         vSize="TOTAL",
                         type="index",
                         
                         # Main
                         title="Take-out making a mess in your country? Food plastics (pet,pp and ps) collected in 2020",
                         palette=cbPalette,
                         
                         # Borders:
                         border.col=c("white"),             
                         border.lwds=2,                         
                         
                         # Labels
                         fontsize.labels=0.5,
                         fontcolor.labels="white",
                         fontface.labels=1,            
                         bg.labels=c("transparent"),              
                         align.labels=c("center", "center"),                                  
                         overlap.labels=0,
                         inflate.labels=T)                      
