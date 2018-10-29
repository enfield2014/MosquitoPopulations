# Graphing script for Ellie
library(readxl)

data <- read_excel("data.xlsx")
data$groups <- rep("<1992", length(data$Year))
data$groups[which(data$Year > 1991)] <- ">=1992"
data$groups <- as.factor(data$groups)
summary(data$groups) # 15 data points 1987 and before, 21 data points 1988 and later, # 19 from before 1992 and 17 from after

# Here are some box plots with the pre and post years side by side
plot(data$CPG ~ data$groups)
plot(data$TotRain ~ data$groups)
plot(data$AvHeat ~ data$groups)

anova(lm(data$CPG ~ data$groups)) # simple anova, which I know you already did; P < 0.005
anova(lm(data$TotRain ~ data$groups)) # ns
anova(lm(data$AvHeat ~ data$groups)) # ns

# You can get all 3 into a single plot by changing the plotting parameters - this changes how things pop up in the plotting view in R and R Studio, so you'll have to reset them once youre done (or quit out and restart the session)
par(mfrow = c(1,3)) # this sets the plotting window to show 1 row of plots but 3 columns of plots
plot(data$CPG ~ data$groups, xlab = "Year Grouping", ylab = "Abundance", main = "Culex Abundance Before and After 1992")
plot(data$TotRain ~ data$groups, xlab = "Year Grouping", ylab = "Total Annual Rainfall (inches)", main = "Total Rainfal Before and After 1992")
plot(data$AvHeat ~ data$groups, xlab = "Year Grouping", ylab = "Average Heat in Growing DD", main = "Average Heat Before and After 1992")
par(mfrow = c(1,1)) # this resets the plotting window to 1 plot at a time

# Obviously I guessed as to what you might want the x and y axis labels to be and the title of each plot... which is not necessary
# The order of the grouping is defaulted to alphabetical, but if you want to name the year groupings something different, I'm sure it wouldn't be too hard to manually have the Pre years be first and the Post years be second.

# In R's base plot (which is what I've done here), you can change colors, add notations, etc 
# ggplot does fun things too, but a lot of the extra frills for that package seems unnecessary for this plot


plot(data$Year, data$CPG, typ = "o", xlab = "Year", ylab = "Number of Mosquitoes Caught", main = "Mosquitoes Caught 1969-2008", xaxt="n", pch=NA, data=data)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))
#this tells the program to create its own x values, does NOT alter y side at all

points(data$Year, data$Aedes_tri, col = "red", pch=NA)
lines(data$Year, data$Aedes_tri, col = "red")
points(data$Year, data$An_punc, col = "blue", pch=NA)
lines(data$Year, data$An_punc, col = "blue")
points(data$Year, data$Aedes_vex, col = "orange", pch=NA)
lines(data$Year, data$Aedes_vex, col = "orange")
points(data$Year, data$Cx_tarsalis, col = "green", pch=NA)
lines(data$Year, data$Cx_tarsalis, col = "green")
legend("topleft", legend=c("CPG", "Aedes tri","An punc", "Aedes vexans", "Cx tar"), col=c("black", "red","blue", "orange", "green"), lty=c(1,1,1,1,1), ncol=1, cex=0.70, pch=c(NA,NA,NA,NA,NA))


plot(Year, data$TotRain, typ = "o", xlab = "Year", ylab = "Total TotRain (in) in capture period", main = "TotRain Trends", xaxt="n", pch=NA)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))
