library(tidyverse)
library(gganimate)
library(scales)
# Get the Data



tt <- tidytuesdayR::tt_load('2021-02-09')

income_distribution<-tt$income_distribution


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p<-income_distribution %>%
  filter(race %in% c("Black Alone", "White Alone", "Hispanic (Any Race)")) %>%
  filter(year >= 1980) %>%
  ggplot(aes(x=year,y=income_median,color=race))+
  theme_minimal()+
  xlab("")+
  ylab("Median income")+
  geom_line()+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"))+
  annotate("text", x = 2019, y= 40000, label = "Black")+
  annotate("text", x = 2020, y= 51000, label = "Hispanic")+
  annotate("text", x = 2018, y= 66000, label = "White")+
  scale_y_continuous(labels=dollar,limits = c(25000,75000))+
  geom_vline(xintercept = c(1980,1981,1990,2001,2008), linetype="dotted", 
             color = "#CC79A7", size=1)+
  labs(title = "Family income by race 1980-2020", subtitle = "(Dotted lines denote the years with major recession events)",
       caption = "Data: Urban Institute-US Census-Wikipedia | TidyTuesday | @SheroshaR")+
  theme(legend.position="none") 

p
