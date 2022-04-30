

library(tidyverse)
library(readxl)

Combined <- read_excel("C:/Users/enfield/Desktop/Diapause/Semi Field Study/HOBO/Combined.xlsx")


View(Combined)

#mutate out the column containing date and time so date can be separate
Combo <- Combined %>%
  mutate(Date_only = str_sub(Date, 1,10)) %>%
  as.data.frame()

View(Combo)


Combo <- Combined %>%
  group_by(Date, Year) %>%
  summarise(avet=mean(Temp)) %>%
  as.data.frame()

Combo <- Combo %>%
  mutate(Date_only = str_sub(Date, 1,10)) %>%
  as.data.frame()

View(Combo)

Combo %>%
  filter(Year==2020) %>%
  ggplot(aes(x=Date_only, y=avet)) +
  geom_line(size=1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) 


Combo %>%
  filter(Year==2021) %>%
  ggplot(aes(x=Week, y=avet, color=Site)) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  scale_x_continuous(breaks = c(31, 33, 35, 37, 39, 41, 43)) +
  geom_line(size=1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) 


Combo %>%
  ggplot(aes(x=Week, y=avet, color=Site, linetype=as.factor(Year))) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  scale_x_continuous(breaks = c(31, 33, 35, 37, 39, 41, 43)) +
  geom_line(size=1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) 



library(writexl)

TempSites <- Combo %>%
  group_by(Year, Week, Site) %>%
  as.data.frame()

write_xlsx(TempSites,'C:\\Users\\enfield\\Desktop\\TempSites.xlsx')


#DiurnalRangeWork
New <- Combo %>%
  group_by(Date_only) %>%
  summarise(max(Temp), min(Temp)) %>%
  as.data.frame()


New$DiurnalRange <- New$`max(Temp)` - New$`min(Temp)`

New %>%
  ggplot(aes(x=Date_only, y=DiurnalRange)) +
  geom_point() +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) 
