
library(tidyverse)
library(readxl)
library(writexl)


Suffolk <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Suffolk.xlsx")

AdjustedVA <- Suffolk %>%
  filter(WOTY>=30, WOTY<=40) %>%
  group_by(Year, WOTY, Source) %>%
  summarise(count=sum(CxPipF)) %>%
  as.data.frame()


AdjustedVA %>%
  group_by(Year, WOTY) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y=ave)) +
  coord_cartesian(ylim=c(0,75)) +
  geom_point(size=1) +
  geom_smooth() +
  coord_cartesian(xlim=c(30,40), ylim=c(0,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




SuffolkAvg <- AdjustedVA %>%
  group_by(Year, WOTY) %>%
  summarise(ave=mean(count)) %>%
  as.data.frame()

write_xlsx(SuffolkAvg,'C:\\Users\\enfield\\Desktop\\SuffolkAvg.xlsx')

SuffolkTOTALAVG <- AdjustedVA %>%
  group_by(WOTY) %>%
  summarise(Ave=mean(count)) %>%
  as.data.frame()




Chicago <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Chicago.xlsx")
View(Chicago)


##Chicagos data is already adjusted for sites, with avg trap index by night already precalculated by their spreadsheets
Chicago  %>%
  group_by(TrapType, Year) %>%
  ggplot(aes(x=WOTY, y = TrapIndex, color=TrapType)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  coord_cartesian(xlim=c(30,40), ylim=c(0,100)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  scale_color_manual(values=c('#FB0E0E','#2145FF')) +
  ggtitle("Chicago 2016-2020") +
  xlab("Week") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ChicagoAvg <- Chicago %>%
  filter(WOTY>=30) %>%
  group_by(TrapType, WOTY, Year) %>%
  summarise(Avg=mean(TrapIndex)) %>%
  as.data.frame()

write_xlsx(ChicagoAvg,'C:\\Users\\enfield\\Desktop\\ChicagoAvg.xlsx')


#Regression set-up
num.years <- length(levels(as.factor(Chicago$Year)))
mos.slopes <- data.frame(year=rep.int(NaN, num.years), 
                         njlt=rep.int(NaN, num.years), 
                         gravid=rep.int(NaN, num.years))

#Regression filling in
for (i in levels(as.factor(Chicago$Year))){
  curr.year <- as.numeric(i)
  index <- match(curr.year,as.numeric(levels(as.factor(Chicago$Year))))
  mos.slopes[index,1] <- curr.year
  curr.data <- Chicago %>% 
    filter(Year==curr.year)
  curr.data.njlt <- curr.data %>% 
    filter(TrapType=="Light") 
  curr.data.gravid <- curr.data %>% 
    filter(TrapType=="Gravid")
  
  if (nrow(curr.data.njlt) > 1) {
    week.trap.NJLT <- curr.data.njlt %>% 
      lm(formula=TrapIndex~WOTY)
    mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
  } else {mos.slopes[index,2] <- NaN}
  
  if (nrow(curr.data.gravid) > 1) {
    week.trap.gravid <-  curr.data.gravid %>% 
      lm(formula=TrapIndex~WOTY)
    mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
  } else {mos.slopes[index,3] <- NaN}
}



##all together now
mos.slopes.long <- mos.slopes %>% gather(key = trap, value = slope, -year)

#check to see how it looks
View(mos.slopes.long)

##test it
with(mos.slopes.long, pairwise.t.test(slope, trap, paired = T))


##P = 0.12


## to calculate the average WOTY from the raw data, comparable to how I process the others I needed the site indices
LONGCHICAGO <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/ILdata/LONGCHICAGO.xlsx")

ChicagoTotalAVgs <- LONGCHICAGO %>%
  group_by(WOTY) %>%
  summarise(Ave=mean(TrapIndex))





Connecticut_1 <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Connecticut_1.xlsx", 
                            col_types = c("numeric", "numeric", "numeric", 
                                          "text", "text", "text", "text", "text", 
                                          "numeric", "numeric", "text"))
View(Connecticut_1)

#both gravid and light trap data
AllCT <- Connecticut_1 %>%
  filter(WOTY>=30) %>%
  group_by(Year,WOTY,Site,TrapType) %>%
  summarise(count=sum(Count)) %>%
  as.data.frame()

AllCT  %>%
  group_by(WOTY, Year, TrapType) %>%
  filter(WOTY<=40) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave, color=TrapType)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Connecticut 2006-2020") +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  scale_color_manual(values=c('#FB0E0E','#2145FF')) +
  coord_cartesian(ylim=c(0,50)) +
  xlab("Week") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



##Only gravid data
AdjustedCT <- Connecticut_1 %>%
  filter(WOTY>=30, TrapType=="Gravid Trap") %>%
  group_by(Year,WOTY,Site) %>%
  summarise(count=sum(Count)) %>%
  as.data.frame()
  
AdjustedCT  %>%
  group_by(WOTY, Year) %>%
  filter(WOTY<=40) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave)) + 
  geom_point() +
  geom_smooth() +
  ggtitle("Connecticut 2006-2020") +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  scale_color_manual(values=c('#CD6155','#B7950B')) +
  coord_cartesian(ylim=c(0,50)) +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#getting numbers
ConnecticutTotalAvgs <- AdjustedCT %>%
  group_by(WOTY) %>%
  summarise(Ave=mean(count)) %>%
  as.data.frame()

ConnecticutAvgs <- AdjustedCT %>%
  filter(WOTY>=30) %>%
  group_by(WOTY, Year) %>%
  summarise(Avg=mean(count)) %>%
  as.data.frame()


write_xlsx(ConnecticutAvgs,'C:\\Users\\enfield\\Desktop\\ConnecticutAvgs.xlsx')




#regression set-up
num.years <- length(levels(as.factor(AllCT$Year)))
mos.slopes <- data.frame(year=rep.int(NaN, num.years), 
                         njlt=rep.int(NaN, num.years), 
                         gravid=rep.int(NaN, num.years))

#filling it in
for (i in levels(as.factor(AllCT$Year))){
  curr.year <- as.numeric(i)
  index <- match(curr.year,as.numeric(levels(as.factor(AllCT$Year))))
  mos.slopes[index,1] <- curr.year
  curr.data <- AllCT %>% 
    filter(Year==curr.year)
  curr.data.njlt <- curr.data %>% 
    filter(TrapType=="Light Trap") 
  curr.data.gravid <- curr.data %>% 
    filter(TrapType=="Gravid Trap")
  
  if (nrow(curr.data.njlt) > 1) {
    week.trap.NJLT <- curr.data.njlt %>% 
      glm(formula=count~WOTY, family = "poisson")
    mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
  } else {mos.slopes[index,2] <- NaN}
  
  if (nrow(curr.data.gravid) > 1) {
    week.trap.gravid <-  curr.data.gravid %>% 
      glm(formula=count~WOTY, family = "poisson")
    mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
  } else {mos.slopes[index,3] <- NaN}
}

##all together now
mos.slopes.long <- mos.slopes %>% gather(key = trap, value = slope, -year)

#check to see how it looks
View(mos.slopes.long)

##test it
with(mos.slopes.long, pairwise.t.test(slope, trap, paired = T))



## P = 3.4e-06



library(readxl)

View(Minnesota_1)
Minnesota_1 <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Minnesota-1.xlsx")
Minnesota_1$Date <- c(format(Minnesota_1$s_enddate, format="%m/%d"))
View(Minnesota_1)


Minnesota_1$date <- as.Date(Minnesota_1$Date, format="%m/%d")


AdjustedMN <- Minnesota_1 %>%
  group_by(year, WOTY, loc_code) %>%
  summarise(count=sum(Indexall)) %>%
  as.data.frame()

AdjustedMN %>%
  group_by(year, WOTY) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave)) + 
  geom_point(color="#CD6155") +
  geom_smooth() +
  coord_cartesian(ylim=c(0,15), xlim = c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("MN 2006-2020") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


MinnesotaTOTALAvgs <- AdjustedMN %>%
  filter(WOTY>=30) %>%
  group_by(WOTY) %>%
  summarise(Avg=mean(count)) %>%
  as.data.frame()



MinnesotaAvgs <- AdjustedMN %>%
  filter(WOTY>=30) %>%
  group_by(year, WOTY) %>%
  summarise(Avg=mean(count)) %>%
  as.data.frame()

write_xlsx(MinnesotaAvgs,'C:\\Users\\enfield\\Desktop\\MinnesotaAvgs.xlsx')



California <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/California.xlsx")
View(California)

#Counties excluded were too southern, or in numbers too low to include (i.e. not enough trapping years)
#California provided data from counties ONLY, they have premerged counts from their sites (i.e. num sites)


AllCA <- California %>% 
  filter(disease_week>=30, disease_week<=40,
         species=="Cx pipiens",
         sex=="females - gravid" | sex=="females - bloodfed" | sex=="females - mixed", 
         county=="Alameda" | county=="Butte" | county=="Colusa" | county=="Contra Costa" | county=="Glenn" | county=="Monterey" | county=="Marin" | county=="Napa" | county=="Plumas" | county=="Placer" | county=="San Joaquin" | county=="Santa Cruz" | county=="Santa Clara" | county=="San Mateo" | county=="Sacramento" | county=="Shasta" | county=="Stanislaus" | county=="Sutter" | county=="Yolo" | county=="Yuba") %>%
  group_by(disease_week, disease_year) %>%
  as.data.frame()

AllCA  %>%
  group_by(disease_week, disease_year, trap_type) %>%
  summarise(ave=mean(Index)) %>%
  ggplot(aes(x=disease_week, y = ave, color=trap_type)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values=c('#FB0E0E','#2145FF')) +
  coord_cartesian(ylim=c(0,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("CA 2005-2020") + xlab("Week") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



## Gravid only
AdjustedCA <- California %>% 
  filter(disease_week>=30, disease_week<=40,
    trap_type=="GRVD",
    species=="Cx pipiens",
    sex=="females - gravid" | sex=="females - bloodfed" | sex=="females - mixed", 
    county=="Alameda" | county=="Butte" | county=="Colusa" | county=="Contra Costa" | county=="Glenn" | county=="Monterey" | county=="Marin" | county=="Napa" | county=="Plumas" | county=="Placer" | county=="San Joaquin" | county=="Santa Cruz" | county=="Santa Clara" | county=="San Mateo" | county=="Sacramento" | county=="Shasta" | county=="Stanislaus" | county=="Sutter" | county=="Yolo" | county=="Yuba") %>%
  group_by(disease_week, disease_year) %>%
  as.data.frame()



AdjustedCA  %>%
  group_by(disease_week, disease_year) %>%
  summarise(ave=mean(Index)) %>%
  ggplot(aes(x=disease_week, y = ave)) + 
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("CA 2005-2020") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



CaliforniaAvgs <- AdjustedCA %>%
  group_by(disease_year, disease_week) %>%
  summarise(Avg=mean(Index)) %>%
  as.data.frame()

write_xlsx(CaliforniaAvgs,'C:\\Users\\enfield\\Desktop\\CaliforniaAvgs.xlsx')

CaliforniaTOTALAvgs <- AdjustedCA %>%
  group_by(disease_week) %>%
  summarise(Ave=mean(Index)) %>%
  as.data.frame()



#Regression set-up
num.years <- length(levels(as.factor(AllCA$disease_year)))
mos.slopes <- data.frame(year=rep.int(NaN, num.years), 
                         njlt=rep.int(NaN, num.years), 
                         gravid=rep.int(NaN, num.years))

#filling it in
for (i in levels(as.factor(AllCA$disease_year))){
  curr.year <- as.numeric(i)
  index <- match(curr.year,as.numeric(levels(as.factor(AllCA$disease_year))))
  mos.slopes[index,1] <- curr.year
  curr.data <- AllCA %>% 
    filter(disease_year==curr.year)
  curr.data.njlt <- curr.data %>% 
    filter(trap_type=="NJLT") 
  curr.data.gravid <- curr.data %>% 
    filter(trap_type=="GRVD")
  
  if (nrow(curr.data.njlt) > 1) {
    week.trap.NJLT <- curr.data.njlt %>% 
      lm(formula=Index~disease_week)
    mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
  } else {mos.slopes[index,2] <- NaN}
  
  if (nrow(curr.data.gravid) > 1) {
    week.trap.gravid <-  curr.data.gravid %>% 
      lm(formula=Index~disease_week)
    mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
  } else {mos.slopes[index,3] <- NaN}
}

##all together now
mos.slopes.long <- mos.slopes %>% gather(key = trap, value = slope, -year)

#check to see how it looks
View(mos.slopes.long)

##test it
with(mos.slopes.long, pairwise.t.test(slope, trap, paired = T))



#P value = 0.00011




Colorado <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Colorado.xlsx")

View(Colorado)

AllCO <- Colorado %>%
  filter(Spp=="pipiens") %>%
  group_by(`Collection Site (Trap ID)`, Year, Week, Trap) %>%
  summarise(count=sum(Total)) %>%
  as.data.frame()


AllCO %>%
  group_by(Week, Year, Trap) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=Week, y=ave, color=Trap)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values=c('#FB0E0E','#2145FF')) +
  coord_cartesian(ylim= c(0,100), xlim= c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("CO 2015-2020") +
  xlab("Week") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#Gravid only
AdjustedColorado <- Colorado %>%
  filter(Spp=="pipiens", Trap=="GRAVID") %>%
  group_by(`Collection Site (Trap ID)`, Year, Week) %>%
  summarise(count=sum(Total)) %>%
  as.data.frame()

ColoradoTotalAvgs <- AdjustedColorado %>%
  filter(Week>=30) %>%
  group_by(Week) %>%
  summarise(ave=mean(count)) %>%
  as.data.frame()

ColoradoTotalAvgs

ColoradoAvgs <- AdjustedColorado %>%
  filter(Week>=30) %>%
  group_by(Year, Week) %>%
  summarise(Avg=mean(count)) %>%
  as.data.frame()

write_xlsx(ColoradoAvgs,'C:\\Users\\enfield\\Desktop\\ColoradoAvgs.xlsx')


##ZOOM 
AdjustedColorado  %>%
  group_by(Week, Year) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=Week, y=ave)) + 
  geom_point() +
  geom_smooth() +
  scale_color_manual(values=c('#CD6155','#B7950B')) +
  coord_cartesian(ylim= c(0,100), xlim= c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("CO 2015-2020") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Regression set-up
num.years <- length(levels(as.factor(AllCO$Year)))
mos.slopes <- data.frame(year=rep.int(NaN, num.years), 
                         njlt=rep.int(NaN, num.years), 
                         gravid=rep.int(NaN, num.years))

#filling it in
for (i in levels(as.factor(AllCO$Year))){
  curr.year <- as.numeric(i)
  index <- match(curr.year,as.numeric(levels(as.factor(AllCO$Year))))
  mos.slopes[index,1] <- curr.year
  curr.data <- AllCO %>% 
    filter(Year==curr.year)
  curr.data.njlt <- curr.data %>% 
    filter(Trap=="LIGHT") 
  curr.data.gravid <- curr.data %>% 
    filter(Trap=="GRAVID")
  
  if (nrow(curr.data.njlt) > 1) {
    week.trap.NJLT <- curr.data.njlt %>% 
      lm(formula=count~Week)
    mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
  } else {mos.slopes[index,2] <- NaN}
  
  if (nrow(curr.data.gravid) > 1) {
    week.trap.gravid <-  curr.data.gravid %>% 
      lm(formula=count~Week)
    mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
  } else {mos.slopes[index,3] <- NaN}
}

##all together now
mos.slopes.long <- mos.slopes %>% gather(key = trap, value = slope, -year)

#check to see how it looks
View(mos.slopes.long)

##test it
with(mos.slopes.long, pairwise.t.test(slope, trap, paired = T))



#P value = 0.035  




#FROM KPRICE:
#Counties selected based on continuous trapping efforts at multiple site locations across yrs. Note spp. besides Cx. found in traps are listed as well. So, Unique Sample Identifiers (USIs) may have multiple rows of data associated with ea. unique spp. Empty data fields indicate zero mosquitoes collected in trap.
#To the best of my knowledge, all traps set 18-28hrs.

Pennsylvania <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Trapping/Pennsylvania.xlsx")

Pennsylvania$Date <- c(format(Pennsylvania$COLLECTED, format="%m/%d"))

Pennsylvania$date <- as.Date(Pennsylvania$Date, format="%m/%d")
View(Pennsylvania)


AdjustedPA <- Pennsylvania %>%
  filter(SPECIES=="Culex pipiens") %>%
  group_by(YEAR, WOTY, COUNTY, USI, COLLECTED) %>%
  summarise(count=sum(COUNT)) %>%
  as.data.frame()

# loess smothing trend
AdjustedPA %>%
  group_by(YEAR, WOTY) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave)) + 
  geom_point(color="#CD6155") +
  geom_smooth() +
  coord_cartesian(xlim=c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("PA 2007-2017") +
  theme_bw() + theme(legend.position = "none") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## by county
AdjustedPA %>%
  group_by(WOTY, COUNTY) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave, color=as.factor(COUNTY))) + 
  geom_line(size=2) +
  coord_cartesian(xlim=c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("PA 2007-2017") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#all sites, by year
AdjustedPA %>%
  group_by(WOTY, YEAR) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave, color=as.factor(YEAR))) + 
  geom_line(size=2) +
  coord_cartesian(xlim=c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("PA 2007-2017") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


##average by week of the year
AdjustedPA %>%
  group_by(WOTY) %>%
  summarise(ave=mean(count)) %>%
  ggplot(aes(x=WOTY, y = ave)) + 
  geom_line(size=2) +
  coord_cartesian(xlim=c(30,40)) +
  scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
  ggtitle("PA 2007-2017") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#just double checking the data
View(AdjustedPA)




Pennsylvania2017Avgs <- AdjustedPA %>%
  filter(YEAR==2010, WOTY==40) %>%
  as.data.frame()


### Total gravid trend for all years, all counties
PennsylvaniaTotalAvgs <- AdjustedPA %>%
  filter(WOTY>=30) %>%
  group_by(WOTY) %>%
  summarise(ave=mean(count)) %>%
  as.data.frame()

PennsylvaniaTotalAvgs



## Code to use in Graphpad, to separate out years 
PennsylvaniaAvgs <- AdjustedPA %>%
  filter(WOTY>=30) %>%
  group_by(YEAR, WOTY) %>%
  summarise(ave=mean(COUNT)) %>%
  as.data.frame()

PennsylvaniaAvgs_Marie <- AdjustedPA %>%
  filter(WOTY>=30) %>%
  group_by(YEAR, WOTY) %>%
  summarise(ave=mean(count)) %>%
  ungroup() %>%
  select(-YEAR) %>%
  group_by(WOTY) %>%
  summarise(Final_avg = mean(ave)) %>%
  as.data.frame()

write_xlsx(PennsylvaniaAvgs,'C:\\Users\\enfield\\Desktop\\PennsylvaniaAvgs.xlsx')



### MAPPING

library(usmap)
library(ggplot2)

usmap::plot_usmap("counties", 
                  include = c("VA", "CA", "CO", "MN", "IL", "CT", "PA"))



### Environmental datasets from MESONET https://mesonet.agron.iastate.edu/request/coop/fe.phtml
library(readxl)
Chicago <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/Chicago.xlsx")
View(Chicago)

Chicago <- Chicago %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()

Colorado <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/Colorado.xlsx")
View(Colorado)

Colorado <- Colorado %>%
  filter(Week>=30) %>%
  group_by(Week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()

ScentralPA <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/ScentralPA.xlsx")
View(ScentralPA)

ScentralPA <- ScentralPA %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()



MinneapolisArea <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/MinneapolisArea.xlsx")
View(MinneapolisArea)

MinneapolisArea <- MinneapolisArea %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()


Connecticut <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/Connecticut.xlsx")
View(Connecticut)

Connecticut <- Connecticut %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()

Sacramento <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/Sacramento.xlsx")

California <- Sacramento %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()


##Suffolk data is listed as VA8192 SUFFOLK LAKE KILBY but its really an amalgamation of a TON of sites.
Suffolk <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/Suffolk.xlsx")

Suffolk <- Suffolk %>%
  filter(week>=30) %>%
  group_by(week) %>%
  summarise(ave=mean(avc)) %>%
  as.data.frame()


## Photoperiod values from https://www.timeanddate.com/

library(tidyverse)
library(readxl)
PhotoStates <- read_excel("C:/Users/enfield/Desktop/Diapause/OtherStateData/Environmental Data/PhotoStates.xlsx")
View(PhotoStates)

PhotoStates <- PhotoStates %>%
  filter(Week>=30) %>%
  group_by(Site, Week) %>%
  summarise(ave=mean(Decimals)) %>%
  as.data.frame()