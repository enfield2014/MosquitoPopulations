library(tidyverse)
library(readxl)

centralIowa30yr <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/centralIowa30yr.xlsx")
View(centralIowa30yr)

##thirty year average for central Iowa
centralIowa30yr %>%
  group_by(Week, Year, Var) %>%
  summarise(ave=mean(Temp)) %>%
  ggplot(aes(x=Week, y = ave, color=as.factor(Var))) + 
  geom_point() +
  geom_smooth() +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position = "none")

#remove average out for seasonality paper
centralIowa30yr %>%
  group_by(Week, Year, Var) %>%
  filter(!Var=="avc", Week>=19, Week<=41) %>%
  summarise(ave=mean(Temp)) %>%
  ggplot(aes(x=Week, y = ave, color=as.factor(Var))) + 
  geom_point() +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position = "none")


#dataframe to export to excel for Graphpad figure
Temps30Years <- centralIowa30yr %>%
  group_by(Year,Var,Week) %>%
  summarise(ave=mean(Temp)) %>%
  as.data.frame()


library(writexl)
write_xlsx(Temps30Years,'C:\\Users\\enfield\\Desktop\\Temps30Years.xlsx')

TempsWeeks <- centralIowa30yr %>%
  group_by(Var,Week) %>%
  filter(Week>=20, Week<=45) %>%
  summarise(ave=mean(Temp)) %>%
  as.data.frame()


Temps2021 <- centralIowa30yr %>%
  group_by(Var,Week) %>%
  filter(Week>=20, Week<=45, Year==2021) %>%
  summarise(ave=mean(Temp)) %>%
  as.data.frame()




TempComparisons <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/Env variables/TempComparisons.xlsx")

TempComparisons %>%
  group_by(Week,Var) %>%
  summarise(ave=mean(Temp)) %>%
  ggplot(aes(x=Week, y = ave, color=as.factor(Var))) + 
  geom_line() +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))






Diurnal <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/Env variables/Diurnal.xlsx")


Diurnal %>%
  group_by(Week) %>%
  filter(Week<=41, Week>=20, Year==2021) %>%
  summarise(ave=mean(Diurnal)) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_line() +
  coord_cartesian(ylim=c(6,18)) +
  scale_y_continuous(breaks = c(6,8,10,12,14,16,18)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position = "none")


Diurnal %>%
  group_by(Week) %>%
  filter(Week<=45, Week>=20) %>%
  summarise(ave=mean(Diurnal)) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_point() +
  geom_smooth() +
  scale_y_continuous(breaks = c(11, 12, 13, 14)) +
  coord_cartesian(ylim = c(11,14)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position = "none")

Diurnal2 <- Diurnal %>%
  group_by(Week) %>%
  filter(Week<=45, Week>=20, Year==2021) %>%
  summarise(ave=mean(Diurnal)) %>%
  as.data.frame()





library(tidyverse)
library(readxl)
Env <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/Env variables/Env.xlsx")
View(Env)

##adjust for the decimal/minutes 
Env$hrslight.fixed <- with(Env, floor(hrslight) + (hrslight - floor(hrslight))*100/60)

## data is from MESONET and represents the years 2009-2019
View(Env)


Env  %>%
  ggplot(aes(x=day, y = Diurnal)) + 
  geom_point() +
  geom_smooth(method="loess") +
  coord_cartesian(ylim = c(0,25)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


Env  %>%
  group_by(Week)  %>%
  summarise(ave=mean(Diurnal), SE=sd(Diurnal)/sqrt(12)) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_line() +
  ylab("Avg diurnal temp range C") +
  #coord_cartesian(ylim = c(0,25)) +
  geom_errorbar(aes(ymin=ave-SE, ymax=ave+SE, width=0.2)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))



Env  %>%
  group_by(Week)  %>%
  summarise(ave=mean(Diurnal)) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_point() +
  ylab("Avg diurnal temp range C") +
  geom_smooth(method="loess") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))




Env  %>%
  filter(Week>=25, Week<=35) %>%
  summarise(ave=mean(Diurnal)) %>%
  as.data.frame()

Env  %>%
  filter(Week>=36) %>%
  summarise(ave=mean(Diurnal)) %>%
  as.data.frame()

Env  %>%
  group_by(Week, Year)  %>%
  summarise(ave=mean(Diurnal)) %>%
  ggplot(aes(x=Week, y = ave, color=as.factor(Year))) + 
  geom_line() +
  ylab("Avg diurnal temp range C") +
  #geom_smooth(method="loess") +
  #coord_cartesian(ylim = c(0,25)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))







Env$Date <- c(format(Env$date, format="%m/%d"))

View(Env)



Env  %>%
  group_by(Date)  %>%
  summarise(ave=mean(Diurnal)) %>%
  ggplot(aes(x=Date, y = ave)) + 
  geom_point() +
  geom_smooth() +
  ylab("Avg diurnal temp range C") +
  geom_smooth(method="loess") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))








## Celsius, temp 
Env  %>%
  group_by(Week, Year) %>%
  filter(Week>=33) %>%
  summarise(ave=mean(avc)) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_smooth(method="loess") +
  geom_point(color='#F4D03F') + 
  ggtitle(paste("Temperature Trends")) + 
  coord_cartesian(ylim = c(0,30)) +
  scale_x_continuous(breaks = c(33, 35, 37, 39)) +
  xlab("Week") + 
  ylab("Avg Deg Celsius") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))




averagetemps <- Env %>%
  group_by(Year) %>%
  summarise(ave=mean(lowc)) %>%
  as.data.frame()

averagetemps

## 2009 had lowest average temp, 7.5; 2017 and 2013 had highest, 17.0 and 17.5 

# Variable: Precipitation (rain+melted snow in mm)
Env  %>%
  group_by(Week, Year) %>%
  filter(Week>=33) %>%
  summarise(ave=mean(precipmm)) %>% 
  ggplot(aes(x=Week, y = ave)) + 
  geom_smooth()+
  geom_point(color='#A9CCE3') + 
  ggtitle(paste("Rainfall Trends")) + 
  coord_cartesian(ylim = c(0,20)) +
  scale_x_continuous(breaks = c(33, 35, 37, 39)) +
  xlab("Week") + 
  ylab("Avg Precip (mm)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))

## rainfall error bars
Env  %>%
  group_by(Week) %>%
  filter(Week>=33) %>%
  summarise(ave=mean(precipmm), SE=sd(precipmm)/sqrt(9)) %>%
  ggplot(aes(x = Week, y = ave)) +
  ggtitle(paste("Rainfall Trends")) + 
  geom_line(color='#A9CCE3') +
  xlab("Week") + 
  ylab("Avg Precip (mm)") +
  scale_x_continuous(breaks = c(33, 35, 37, 39)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) +
  geom_errorbar(aes(ymin=ave-SE, ymax=ave+SE, width=0.2), color=("#A9CCE3"))






## Photoperiod
Env  %>%
  group_by(Week, Year) %>%
  summarise(ave=mean(hrslight.fixed)) %>% 
  ggplot(aes(x=Week, y = ave)) + 
  geom_point(color='#C39BD3') + 
  geom_smooth() +
  xlab("Week") + 
  ylab("Hrs daylight") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


Env  %>%
  group_by(Week, Year) %>%
  summarise(ave=mean(hrslight.fixed)) %>% 
  filter(Week>=33) %>%
  ggplot(aes(x=Week, y = ave)) + 
  geom_point(color='#C39BD3') + 
  geom_smooth() +
  coord_cartesian(ylim = c(11, 14)) +
  ggtitle(paste("Photoperiod Trends")) +
  scale_x_continuous(breaks = c(33, 35, 37, 39)) +
  xlab("Week") + 
  ylab("Hrs daylight") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


PhotoYears <- Env %>%
  group_by(Year, Week) %>%
  summarise(ave=mean(hrslight.fixed)) %>%
  as.data.frame()

library(writexl)
write_xlsx(PhotoYears,'C:\\Users\\enfield\\Desktop\\PhotoYears.xlsx')




averageP12 <- Env %>%
  group_by(day) %>%
  filter(day>=240) %>%
  summarise(ave=mean(hrslight.fixed)) %>%
  as.data.frame()

averageP12


## Photoperiod threshold calculation
Env  %>%
  group_by(Week) %>%
  filter(Year==2017, Week>=30) %>%
  summarise(ave=mean(hrslight.fixed)) %>% 
  ggplot(aes(x=Week, y = ave)) + 
  geom_line(color="#C39BD3", linetype="dashed") + 
  #stat_smooth() +
  #ggtitle(paste("Photoperiod")) +
  ylab("Hrs daylight") +
  xlab("Reference Week") +
  scale_x_continuous(breaks = c(30, 32, 34, 36, 38, 40)) +
  #geom_hline(yintercept=12) +
  #geom_vline(xintercept=38.725) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


## Photo error bars
Env  %>%
  group_by(Week) %>%
  filter(Week>=33) %>%
  summarise(ave=mean(hrslight.fixed), SE=sd(hrslight.fixed)/sqrt(9)) %>%
  ggplot(aes(x = Week, y = ave)) +
  ggtitle(paste("Photoperiod Trends")) + 
  geom_line(color='#C39BD3') +
  xlab("Week") + 
  ylab("Hrs daylight") +
  coord_cartesian(ylim=c(11,14)) +
  scale_x_continuous(breaks = c(33, 35, 37, 39)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10)) +
  geom_errorbar(aes(ymin=ave-SE, ymax=ave+SE, width=0.2), color=('#C39BD3'))


## Plotting just the line (fit to 2020) to check against overfitting
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) 12.1773 + 3.0167 * sin(0.01721*(x-80))
p + stat_function(fun = fun.1) +
  xlab("Reference Week") +
  ylab("Hrs daylight") +
  geom_vline(xintercept=216) +
  geom_vline(xintercept=272) +
  coord_cartesian(xlim=c(120,300), ylim=c(11,15)) +
  scale_x_continuous(breaks = c(132, 167, 202, 237, 272), labels = c("20", "25", "30", "35", "40")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))



#216, 230, 244, 258, 272 are the weeks 32,34,36,38



p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) 12.1773 + 3.0167 * sin(0.01721*(x-80))
p + stat_function(fun = fun.1) +
  xlab("Reference Week") +
  ylab("Hrs daylight") +
  geom_vline(xintercept=290) +
  coord_cartesian(xlim=c(120,300), ylim=c(11,15)) +
  scale_x_continuous(breaks = c(132, 167, 202, 237, 272), labels = c("20", "25", "30", "35", "40")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


library(readxl)
library(tidyverse)
##This excel spreadsheet is for AMES for the past 10 years from May-november. I think Excel reformatted the decimals so I don't have to use James' code above.
Photoperiod <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/Env variables/Photoperiod.xlsx")

Photoperiod  %>%
  group_by(Week, Year) %>%
  summarise(ave=mean(Photo)) %>% 
  filter(Week>=20, Week<=45) %>%
  ggplot(aes(x=Week, y = ave, color=as.factor(Year))) + 
  geom_line() + 
  xlab("Week") + 
  ylab("Hrs daylight") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


Photoperiod  %>%
  group_by(Week, Year) %>%
  filter(Year=="2021", Week>=30, Week<=45) %>%
  summarise(ave=mean(Photo)) %>% 
  ggplot(aes(x=Week, y = ave)) + 
  geom_line(size=1) +
  ggtitle("2021") +
  xlab("Week") + 
  ylab("Hrs daylight") +
  scale_y_continuous(breaks = c(10,11,12,13,14,15)) +
  coord_cartesian(ylim=c(10,15)) +
  scale_x_continuous(breaks = c(30,33,36,39, 42, 45)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))



PhotoYears <- Photoperiod %>%
  filter(Year>=2021, Week>=30) %>%
  group_by(Year, Week) %>%
  summarise(ave=mean(Photo)) %>%
  as.data.frame()


write_xlsx(PhotoYears,'C:\\Users\\enfield\\Desktop\\PhotoYears.xlsx')

PhotoAll <- Photoperiod %>%
  group_by(Week) %>%
  summarise(ave=mean(Photo)) %>%
  as.data.frame()


## 2020 and 2021 Temperatures 

library(readxl)

AmesYearsTEMP %>%
  filter(week>=30,week<=45) %>%
  group_by(Year,doy) %>%
  summarise(ave=mean(avc)) %>%
  ggplot(aes(x=doy, y = ave, color=as.factor(Year))) + 
  geom_line(size=1) +
  scale_y_continuous(breaks = c(0,10,20,30)) +
  coord_cartesian(ylim=c(-2,30)) +
  scale_x_continuous(breaks = c(199,200,220,222,227,243,248,255,262,264,285,306)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.title = element_text(size=12), legend.text = element_text(size=10))


