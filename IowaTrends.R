
library(tidyverse)
library(MASS)
library(readxl)

ColdAbridged <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/ColdAbridged.xlsx")

## Need to adjust for multiple lines of mosquito counts originating from the same trap, so this sums lines from the same trap-pickup to combine counts to each TRAP NIGHT and not each LINE
#also filters to just the species of interest, and to the years of data integrity
#removed yellow banks because of its incontinous trapping efforts and norwoodville bc it only appears in one year
#had to include city of ames as a 'county' filter because of a random terminology switch in the excel spreadsheet.... can we get some consistency in our data uploading please people
#trimmed down to the weeks of interest, 20-40 where trapping efforts are more uniform across years
AdjustedIA <- ColdAbridged %>%
   filter(Species=="Cx pipiens", WOTY>=20, WOTY<=40, Year>=2016, County=="Story" | County=="Polk" | County=="City of Ames", Site!="Yellow Banks", Site!="Norwoodville") %>%
   group_by(Year,WOTY,Site,TrapPickup,Events) %>%
   summarise(count=sum(PoolSize)) %>%
   as.data.frame()

#Calculating the trap index average from counts / number of trap nights now that its cleaned
AdjustedIA$Index <- AdjustedIA$count / AdjustedIA$Events

#total mosquitoes to be included in the study 
TotalGravidMos <- AdjustedIA %>%
   summarise(total=sum(count)) %>%
   as.data.frame()

TotalGravidMos #14027 is total count

#graphical display by year
AdjustedIA %>%
   group_by(WOTY, Year) %>%
   summarise(ave=mean(Index)) %>%
   ggplot(aes(x=WOTY, y= ave, color=as.factor(Year))) +
   geom_line() + 
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


#average trap index by week across all years
TOTAL <- AdjustedIA %>%
   group_by(WOTY) %>%
   summarise(Avg=mean(Index)) %>%
   as.data.frame()

TOTAL


#grouped average 
ByWeek <- AdjustedIA %>%
   group_by(WOTY) %>%
   filter(WOTY>=30, WOTY<=40) %>%
   summarise(ave=mean(Index)) %>%
   as.data.frame()

#grouped average for percent change against week 40 
TOTAL %>%
   filter(WOTY>=30, WOTY<=33) %>%
   summarise(ave=mean(Avg)) %>%
   as.data.frame()

#average trap index by site across all year - Checking for integrity of the data filtering
AdjustedIA %>%
   group_by(Site, WOTY) %>%
   summarise(Avg=mean(Index)) %>%
   ggplot(aes(x=WOTY, y= Avg, color=Site)) +
   geom_line() + 
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


##trap index by week by year by site for Combo 
Gravid <- AdjustedIA %>%
   group_by(Year, WOTY, Site) %>%
   summarise(Avg=mean(Index)) %>%
   as.data.frame()

library(writexl)
write_xlsx(Gravid,'C:\\Users\\enfield\\Desktop\\Gravid.xlsx')

#average trap index by week by year
GravidforTemp <- AdjustedIA %>%
   filter(WOTY>=30, WOTY<=40) %>%
   group_by(Year, WOTY) %>%
   summarise(Avg=mean(Index)) %>%
   as.data.frame()

write_xlsx(GravidforTemp,'C:\\Users\\enfield\\Desktop\\GravidforTemp.xlsx')


#graphical display by year uncut
AdjustedIA %>%
   group_by(WOTY, Year) %>%
   summarise(ave=mean(Index)) %>%
   ggplot(aes(x=WOTY, y= ave)) +
   geom_point() +
   geom_smooth() +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


#graphical display by year cut
AdjustedIA %>%
   group_by(WOTY, Year) %>%
   summarise(ave=mean(Index)) %>%
   ggplot(aes(x=WOTY, y= ave)) +
   geom_point() +
   geom_smooth() +
   coord_cartesian(ylim=c(0,10)) +
   scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))






Combo <- read_excel("C:/Users/enfield/Desktop/Diapause/Iowa/Combo.xlsx")
View(Combo)


#filter to 2016-2021 to match the gravid data, remove sites for low/inconsistent trapping or only doing one year
Combo <- Combo %>%
   filter(Year>=2016, County!="PK-Yellow Banks", County!="SY0SUV", County!="SY0North River Valley Park", County!="SY0Reactor Woods", County!="DM0Crestwood", County!="Norwoodville") %>%
   as.data.frame()

#Correct if readout is 8594 obs of 6 var

TotalNJLT <- Combo %>%
   filter(TrapType=="NJLT") %>%
   summarise(total=sum(TrapIndex))

TotalNJLT #total count of included mosquitoes is: 25897

## Point plot
Combo  %>%
   group_by(WOTY,TrapType,Year) %>%
   filter(TrapType=="NJLT") %>%
   filter(WOTY<=40, WOTY>=20) %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave)) +
   geom_point()  +
   geom_smooth() +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   coord_cartesian(ylim=c(0,10)) +
   scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")


##Check NJLT for sites included
Combo  %>%
   group_by(WOTY,TrapType,Year, County) %>%
   filter(TrapType=="NJLT") %>%
   filter(WOTY<=40, WOTY>=20) %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave, color=County)) +
   geom_point()  



##check to make sure the gravid and NJLT data looks as it should (to the cleaned versions above) so the comparison can be made to the NJLT data.
#Coldabridged put together the gravid, NJLTtotals put together the NJLT data that went straight into the Combo excel spreadsheet
Combo  %>%
   filter(WOTY>=20, WOTY<=40) %>%
   group_by(WOTY,Year,TrapType) %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave, color=TrapType)) +
   geom_point()  +
   geom_smooth() +
   scale_color_manual(values=c('#DE1B55','#D89E83')) +
   coord_cartesian(ylim = c(0,8)) +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")

Combo <- Combo %>% filter(WOTY>=30)

##Both together
Combo  %>%
   group_by(WOTY,Year,TrapType) %>%
   filter(WOTY<=40) %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave, color=TrapType)) +
   geom_point()  +
   geom_smooth(method = "lm", se = TRUE) +
   scale_color_manual(values=c('#FB0E0E','#2145FF')) +
   coord_cartesian(ylim = c(0,8)) +
   scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")




#Gravid only

Combo  %>%
   group_by(WOTY,Year,TrapType) %>%
   filter(WOTY<=40, TrapType=="Gravid") %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave)) +
   geom_point()  +
   geom_smooth(method = "glm", se = FALSE,
               method.args = list(family = "poisson"), linetype="dashed", size=1) +
   coord_cartesian(ylim = c(0,8)) +
   scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")

#Gravid only

Combo  %>%
   group_by(WOTY,Year,TrapType) %>%
   filter(WOTY<=40, TrapType=="NJLT") %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave)) +
   geom_point()  +
   geom_smooth(method = "glm", se = FALSE,
               method.args = list(family = "poisson"), linetype="dashed", size=1) +
   coord_cartesian(ylim = c(0,8)) +
   scale_x_continuous(breaks=c(30,32,34,36,38,40)) +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")





## REGRESSION OUTPUTS

View(Combo)
# making an emtpy data frame with some non numbers ready to fill with goodness and juices
num.years <- length(levels(as.factor(Combo$Year)))
mos.slopes <- data.frame(year=rep.int(NaN, num.years), 
                         njlt=rep.int(NaN, num.years), 
                         gravid=rep.int(NaN, num.years))

## the log link codes it to allow for the lack of integers; POISSON SET UP
#codes NAs for years that aren't present. Set maximum interations higher so the model can converge. Saved the poisson stuff underneath. 
## the mos.slopes crap will pull the coefficients that we want and stuff it
library(MASS)

### NEGATIVE BINOMIAL SET UP
for (i in levels(as.factor(Combo$Year))){
   curr.year <- as.numeric(i)
   index <- match(curr.year,as.numeric(levels(as.factor(Combo$Year))))
   mos.slopes[index,1] <- curr.year
   curr.data <- Combo %>% 
      filter(Year==curr.year)
   curr.data.njlt <- curr.data %>% 
      filter(TrapType=="NJLT") 
   curr.data.gravid <- curr.data %>% 
      filter(TrapType=="Gravid")
   
   if (nrow(curr.data.njlt) > 1) {
      week.trap.NJLT <- curr.data.njlt %>% 
         glm.nb(formula=TrapIndex~WOTY, link = "log")
      mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
   } else {mos.slopes[index,2] <- NaN}
   
   if (nrow(curr.data.gravid) > 1) {
      week.trap.gravid <-  curr.data.gravid %>% 
         glm.nb(formula=TrapIndex~WOTY, link = "log")
      mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
   } else {mos.slopes[index,3] <- NaN}
}



## FOR POISSON
for (i in levels(as.factor(Combo$Year))){
   curr.year <- as.numeric(i)
   index <- match(curr.year,as.numeric(levels(as.factor(Combo$Year))))
   mos.slopes[index,1] <- curr.year
   curr.data <- Combo %>% 
      filter(Year==curr.year)
   curr.data.njlt <- curr.data %>% 
      filter(TrapType=="NJLT") 
   curr.data.gravid <- curr.data %>% 
      filter(TrapType=="Gravid")
   
   if (nrow(curr.data.njlt) > 1) {
      week.trap.NJLT <- curr.data.njlt %>% 
         glm(formula=TrapIndex~WOTY, family="poisson")
      mos.slopes[index,2] <- as.numeric(week.trap.NJLT$coefficients[2])
   } else {mos.slopes[index,2] <- NaN}
   
   if (nrow(curr.data.gravid) > 1) {
      week.trap.gravid <-  curr.data.gravid %>% 
         glm(formula=TrapIndex~WOTY, family="poisson")
      mos.slopes[index,3] <- as.numeric(week.trap.gravid$coefficients[2])
   } else {mos.slopes[index,3] <- NaN}
}




## Linear
for (i in levels(as.factor(Combo$Year))){
   curr.year <- as.numeric(i)
   index <- match(curr.year,as.numeric(levels(as.factor(Combo$Year))))
   mos.slopes[index,1] <- curr.year
   curr.data <- Combo %>% 
      filter(Year==curr.year)
   curr.data.njlt <- curr.data %>% 
      filter(TrapType=="NJLT") 
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

##Binomial: P = 0.016 from Week 30*****
##Poisson is: 0.0017 from week 30*****
##Linear is njlt 0.0079 from week 30***

#end season averages to export to graphpad
Avgs <- Combo %>%
   filter(WOTY>=35, TrapType=="Gravid") %>%
   group_by(WOTY, Year) %>%
   summarise(Avg=mean(TrapIndex))

## filter by year to check out indv trends if i want
Combo  %>%
   group_by(WOTY,Year,TrapType) %>%
   filter(Year=="2020") %>%
   summarise(ave=mean(TrapIndex)) %>% 
   ggplot(aes(x=WOTY, y = ave, color=TrapType)) +
   geom_point()  +
   geom_smooth() +
   scale_color_manual(values=c('#DE1B55','#D89E83')) +
   xlab("Reference Week") + ylab("Trap Index Avg") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size=11, color="black"), axis.text.y = element_text(size=11, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), legend.position= "none")


library(tidyverse)


## base totals for WOTY average
NJLTTOTALS <- Combo %>%
   filter(TrapType=="NJLT") %>%
   group_by(WOTY) %>%
   summarise(ave=mean(TrapIndex)) %>%
   as.data.frame()


