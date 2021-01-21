

#title: "TidyTuesday-Kenya"
#author: Sherosha Raj
#date: 2021-01-19
#output: html_output



## Load the packages ##
library(tidyverse)
library(tidytuesdayR)


library(RColorBrewer)
library(scales)
library(janitor)

library(ggplot2)
library(dplyr)

library(rKenyaCensus)


#Look at the waste data 
#"Percentage Distribution of Conventional Households by Main Mode of 
#Solid Waste Disposal, Area of Residence, County and Sub-County.."
waste<-rKenyaCensus::V4_T2.17

waste<- waste %>% 
  janitor::clean_names() %>% 
  mutate(sub_county=str_to_title(sub_county)) %>% 
  mutate(county=str_to_title(county))

#pick only the 47 counties to work with
#then group collection methods and create new columns with totals per county

waste<-waste %>%  filter(admin_area =='County') %>% 
  mutate(collected=sum(c(collected_county_gvt,collected_community_association,collected_private_company))) %>% 
  mutate(dumped=sum(c(dumped_compound,dumped_street,dumped_latrine))) %>% 
  mutate(burned=sum(c(burnt_inopen,burnt_pit))) %>% 
  mutate(decomposition=sum(c(buried,compost_pit)))

#select only required columns
waste<-waste %>% select(county,collected,dumped,burned,decomposition)
  

#convert to long format
long <-waste %>% 
  gather(disposal,percent,2:5,convert = T) %>% 
  mutate(county = fct_reorder(county, percent)) %>% 
  mutate(disposal=fct_reorder(disposal,percent)) %>% 
    arrange(disposal)
  
#colorblind palette
cbp <- c("#56B4E9", "#009E73",
          "#F0E442", "#D55E00")

#plot
tt_kenyacensuswaste<-ggplot(long)+
  geom_col(aes(x = percent, y = county, fill = disposal))+
  theme_light()+
  theme(panel.grid.major=element_line(colour = "gray", linetype=3,size = 0.25),
        rect = element_rect(fill = "white", colour = "white", size = 0.25,linetype = 0),
        axis.text.y = element_text(hjust =1.0,size=8),
        legend.text=element_text(size=7),
        axis.ticks = element_line(colour = "grey50"),
        legend.position = "right") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100))+
  scale_fill_manual(values = cbp,name="Disposal method",
                    labels=c("Collected (government,community association and private companies)",
                             "Composted and Buried",
                             "Dumped (within compound, street and latrine)",
                             "Burned (in open and pits)"))+
  labs(title = "Waste disposal in Kenyan counties", subtitle="Data: 2019 Kenya Population and Housing Census", x="Percentage of households",y =element_blank())
ggsave("tt_kenyacensuswaste.png")  
   




