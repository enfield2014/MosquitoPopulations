
library(tidyverse)

## removed <20 weeks, >40 weeks prior to data upload
## create new columns for total counts of the species (combining male and female counts)
## I already combined Culex pipiens, Culex restuans, and CPG into a CPG M and CPG F and CPG column in Excel. Sorry!

MasterNJLTIndicesUNCUT$'Aedes albopictus' <- (MasterNJLTIndicesUNCUT$`Aedes albopictus F` + MasterNJLTIndicesUNCUT$`Aedes albopictus M`)
MasterNJLTIndicesUNCUT$'Aedes sollicitans' <- (MasterNJLTIndicesUNCUT$`Aedes sollicitans F` + MasterNJLTIndicesUNCUT$`Aedes sollicitans M`)
MasterNJLTIndicesUNCUT$'Aedes sticticus' <- (MasterNJLTIndicesUNCUT$`Aedes sticticus F` + MasterNJLTIndicesUNCUT$`Aedes sticticus M`)
MasterNJLTIndicesUNCUT$'Aedes japonicus' <- (MasterNJLTIndicesUNCUT$`Aedes japonicus F` + MasterNJLTIndicesUNCUT$`Aedes japonicus M`)
MasterNJLTIndicesUNCUT$'Aedes vexans' <- (MasterNJLTIndicesUNCUT$`Aedes vexans F` + MasterNJLTIndicesUNCUT$`Aedes vexans M`)
MasterNJLTIndicesUNCUT$'Aedes nigromaculis' <- (MasterNJLTIndicesUNCUT$`Aedes nigromaculis F` + MasterNJLTIndicesUNCUT$`Aedes nigromaculis M`)
MasterNJLTIndicesUNCUT$'Aedes triseriatus' <- (MasterNJLTIndicesUNCUT$`Aedes triseriatus F` + MasterNJLTIndicesUNCUT$`Aedes triseriatus M`)
MasterNJLTIndicesUNCUT$'Aedes trivittatus' <- (MasterNJLTIndicesUNCUT$`Aedes trivittatus F` + MasterNJLTIndicesUNCUT$`Aedes trivittatus M`)
MasterNJLTIndicesUNCUT$'Anopheles punctipennis' <- (MasterNJLTIndicesUNCUT$`Anopheles punctipennis M` + MasterNJLTIndicesUNCUT$`Anopheles punctipennis F`)
MasterNJLTIndicesUNCUT$'Coquilletidia perturbans' <- (MasterNJLTIndicesUNCUT$`Coquilletidia perturbans F` + MasterNJLTIndicesUNCUT$`Coquilletidia perturbans M`)
MasterNJLTIndicesUNCUT$'Anopheles quadrimaculatus' <- (MasterNJLTIndicesUNCUT$`Anopheles quadrimaculatus M` + MasterNJLTIndicesUNCUT$`Anopheles quadrimaculatus F`)
MasterNJLTIndicesUNCUT$'Culex erraticus' <- (MasterNJLTIndicesUNCUT$`Culex erraticus M` + MasterNJLTIndicesUNCUT$`Culex erraticus F`)
MasterNJLTIndicesUNCUT$'Culex territans' <- (MasterNJLTIndicesUNCUT$`Culex territans M` + MasterNJLTIndicesUNCUT$`Culex territans F`)
MasterNJLTIndicesUNCUT$'Culex tarsalis' <- (MasterNJLTIndicesUNCUT$`Culex tarsalis M` + MasterNJLTIndicesUNCUT$`Culex tarsalis F`)
MasterNJLTIndicesUNCUT$'Culex salinarius' <- (MasterNJLTIndicesUNCUT$`Culex salinarius M` + MasterNJLTIndicesUNCUT$`Culex salinarius F`)
MasterNJLTIndicesUNCUT$'Psorophora ferox' <- (MasterNJLTIndicesUNCUT$`Psorophora ferox M` + MasterNJLTIndicesUNCUT$`Psorophora ferox F`)
MasterNJLTIndicesUNCUT$'Psorophora horrida' <- (MasterNJLTIndicesUNCUT$`Psorophora horrida M` + MasterNJLTIndicesUNCUT$`Psorophora horrida F`)
MasterNJLTIndicesUNCUT$'Psorophora ciliata' <- (MasterNJLTIndicesUNCUT$`Psorophora ciliata M` + MasterNJLTIndicesUNCUT$`Psorophora ciliata F`)
MasterNJLTIndicesUNCUT$'Psorophora howardii' <- (MasterNJLTIndicesUNCUT$`Psorophora howardii M` + MasterNJLTIndicesUNCUT$`Psorophora howardii F`)
MasterNJLTIndicesUNCUT$'Psorophora columbiae' <- (MasterNJLTIndicesUNCUT$`Psorophora columbiae M` + MasterNJLTIndicesUNCUT$`Psorophora columbiae F`)
MasterNJLTIndicesUNCUT$'Psorophora cyanescens' <- (MasterNJLTIndicesUNCUT$`Psorophora cyanescens M` + MasterNJLTIndicesUNCUT$`Psorophora cyanescens F`)
MasterNJLTIndicesUNCUT$'Uranotaenia sapphirina' <- (MasterNJLTIndicesUNCUT$`Uranotaenia sapphirina M` + MasterNJLTIndicesUNCUT$`Uranotaenia sapphirina F`)
MasterNJLTIndicesUNCUT$'Culiseta inornata' <- (MasterNJLTIndicesUNCUT$`Culiseta inornata M` + MasterNJLTIndicesUNCUT$`Culiseta inornata F`)

View(MasterNJLTIndicesUNCUT)

## ooh the way the mosquito data is stored is horrific. every species has its own column! 
# re-order that into species and counts strings using tidyr and the gather function below
library(tidyr)
new.data <- gather(MasterNJLTIndicesUNCUT, "species", "counts", 5:137)

View(new.data)

#check the total count numbers to see what species are rare/not going to be included
new.data$counts <- as.numeric(new.data$counts)

moschk <- new.data %>%
  group_by(species) %>%
  summarise(Count=sum(counts)) %>%
  as.data.frame()

View(moschk)

#Aedes albopictus, Aedes nigromaculis, Aedes sollicitans, Psorophora ferox, Psorophora howardii, Psorophora cyanescens all too low to use <110

new.data$counts <- as.numeric(new.data$counts)

View(new.data)

library(tidyverse)


new.data %>% 
  filter(species %in% c("CPG", "Culex tarsalis", "Culex erraticus", "Culex territans", "Culex salinarius")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Culex NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index")


new.data %>% 
  filter(species %in% c("CPG F", "Culex tarsalis F", "Culex erraticus F", "Culex territans F", "Culex salinarius F")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Female Culex NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index")


new.data %>% 
  filter(species %in% c("CPG F", "CPG")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("CPG NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")


new.data %>% 
  filter(species %in% c("Aedes vexans", "Aedes sticticus", "Aedes trivittatus", "Aedes triseriatus", "Aedes japonicus")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Aedes NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")


new.data %>% 
  filter(species %in% c("Aedes vexans F", "Aedes vexans")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Aedes vexans NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")

new.data %>% 
  filter(species %in% c("Anopheles punctipennis", "Anopheles quadrimaculatus")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Anopheles NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")

new.data %>% 
  filter(species %in% c("Anopheles punctipennis F", "Anopheles quadrimaculatus F")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("F Anopheles NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")



new.data %>% 
  filter(species %in% c("Coquilletidia perturbans", "Culiseta inornata", "Psorophora horrida", "Psorophora columbiae", "Uranotaenia sapphirina")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Minor genera NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")

new.data %>% 
  filter(species %in% c("Culiseta inornata", "Culiseta inornata F")) %>%
  group_by(species, Week) %>%
  summarise(Count=mean(counts)) %>%
  ggplot(aes(x=Week, y = (Count), color=species)) + 
  geom_line(size=1.5) + ggtitle("Culiseta inornata NJLT 2009-2019 SY, DM") + xlab("Week of the Year") + ylab("Trap Index Ave")



### Let's package this up to combine it with the gravid data to make comparisons

mostotes <- new.data %>%
  filter(species %in% c("Aedes vexans", "Aedes vexans F", "Aedes japonicus", "Aedes japonicus F", "Aedes triseriatus", "Aedes triseriatus F", "Aedes trivittatus", "Aedes trivittatus F", "CPG", "CPG F", "Culex erraticus", "Culex erraticus F", "Culex tarsalis", "Culex tarsalis F", "Culex territans", "Culex territans F", "Culex salinarius", "Culex salinarius F", "Anopheles punctipennis", "Anopheles punctipennis F", "Anopheles quadrimaculatus", "Anopheles quadrimaculatus F", "Psorophora horrida", "Psorophora horrida F", "Psorophora ferox", "Psorophora ferox F", "Psorophora columbiae", "Psorophora columbiae F", "Culiseta inornata", "Culiseta inornata F", "Uranotaenia sapphirina", "Uranotaenia sapphirina F")) %>%
  group_by(Week, species, Year) %>%
  summarise(TrapI=mean(counts)) %>%
  as.data.frame()

View(mostotes)


library(xlsx)
write.xlsx(mostotes, "c:/mydata.xlsx")
##Access denied? okay I'll make an HTML table and copy it to excel

library(tableHTML)
tableHTML(mostotes)

