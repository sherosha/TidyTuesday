---
  title: "TT_bechdel"
author: "Sherosha Raj"
date: "09/03/2021"
output: html_document
---
  
 
library(tidyverse)

library(tidytuesdayR)
library(scales)
library(hrbrthemes)
library(plotly)

library(patchwork)
#get the data
tuesdata <- tidytuesdayR::tt_load('2021-03-09')



movies<-tuesdata$movies


#filter,clean-up
movies_both<-movies %>% 
  filter(!is.na(domgross_2013),
         !is.na(intgross_2013)) %>% 
  mutate(domgross_2013 = as.numeric(domgross_2013),
         intgross_2013 =as.numeric(intgross_2013)) %>% 
  mutate(total_gross2013= domgross_2013+ intgross_2013) %>% 
  filter(budget_2013> 100000000,
         total_gross2013 > 5000000)
#plot
fail_pass_budget_rev_plot<- 
  ggplot(movies_both,aes(budget_2013,total_gross2013,name=title))+
  geom_point(aes(color = binary, size= as.numeric(imdb_votes)), alpha = 0.45)+
  scale_color_manual(values = c("#FC4E07","#00AFBB"))+
  scale_size(range = c(0.1, 10),
             breaks = 1000 * c(10, 50, 100, 150, 200),
             labels = c("10k", "50k", "100k", "150k", "200k"))     +
  scale_x_log10(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_y_log10(labels = unit_format(unit = "M", scale = 1e-6))+
  expand_limits(x = c(0, 500000000)) +
  
  labs(title = "Movie budgets, Total gross revenue, IMDB votes and Bechdel scores",
       subtitle = "Films with only budgets over 100m ('big budget'), and revenues (domestic + international) over 5m were included. Amounts normalized to 2013 levels",
       x = "Budget(log)",
       y = "Total gross revenue normalized to 2013 (log)",
       size = "IMDB votes",
       color = "Bechdel") +
  
  theme_classic()+
  theme(legend.position = "right",
        axis.line = element_line(color = "grey85"),
        axis.ticks = element_line(color = "grey85"))

#plot by year, revenue, Bechdel Pass/Fail

#Pass
movies_pass<-movies_both %>% 
  filter(binary=="PASS")

pass_plot<-ggplot(movies_pass,aes(year,total_gross2013))+
  geom_point(aes(), colour = "#00AFBB",  size= 3, alpha = 0.5)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  expand_limits(y = 0)+
  labs(title = "Films released with 'Pass'",
       x = "Year",
       y = "Total gross revenue")+
  theme_classic()+
  theme(legend.position = "right",
        axis.line = element_line(color = "grey85"),
        axis.ticks = element_line(color = "grey85"))
#fail
movies_fail<-movies_both %>% 
  filter(binary=="FAIL")

fail_plot<-ggplot(movies_fail,aes(year,total_gross2013))+
  geom_point(aes(),colour="#FC4E07" , size= 3,alpha = 0.5)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  expand_limits(y = 0)+
  labs(title = " and 'Fail' Bechdel scores",
       x = "Year",
       y=" ",
       caption = "Data: Bechdeltest.com, FiveThirtyEight | TidyTuesday | @SheroshaR") +
  theme_classic()+
  theme(legend.position = "right",
        axis.line = element_line(color = "grey85"),
        axis.ticks = element_line(color = "grey85"))

#patchwork and save
patch<-fail_pass_budget_rev_plot/
  (pass_plot|fail_plot)


ggsave("patch.png",width=12,height=7)