#[1]Loading Data, pre-processing to determine total number of cases for each unique category and thresholds to filter
library(readxl)
library(ggplot2)
maindata <- read.csv("hate_crime.csv")
popdata <- read_excel("uspopperyear.xlsx")
sumofoffenses = data.frame(table(maindata$STATE_ABBR))
offensesperyear = data.frame(table(maindata$DATA_YEAR))
racebias = data.frame(table(maindata$BIAS_DESC))
offenselocation = data.frame(table(maindata$LOCATION_NAME))
offensetype = data.frame(table(maindata$OFFENSE_NAME))
statenames <- data.frame(table(maindata$STATE_NAME))



#[2]Processing data for the rate of hate crime cases per year
#Load United States Population Data
popdata19912019 <- popdata[(popdata$year > 1990) & (popdata$year < 2020),]
#Rate of hate crimes for each year is calculated and added to a data frame
ratiodata <- data.frame(year = 1991:2019, ratio = 0)
for(i in  1:29){
  ratiodata$ratio[i] = (offensesperyear$Freq[i]/popdata19912019$population[i])*10000 
}
stacked1991 <- data.frame(maindata[which(maindata$DATA_YEAR == 1991),])
statename1991 <- data.frame(table(stacked1991$STATE_ABBR)) 
stacked1991 <- data.frame(year = rep(1991),table(stacked1991$STATE_NAME),stateabbr = statename1991$Var1)
#Plot rate of offenses per year
plot(ratiodata$ratio[1:29],type = "l",xaxt = "n", xlab = "Year", 
     ylab = "Cases per 100000 People", main = "Graph of Hate Crimes per 100000 of Population",
     ylim = c(0.1,0.4), lwd = 2, col = "#4a2b7a")
axis(1, at = 1:29, labels = 1991:2019)



#[3]Sorting the offense types and plotting the bar chart of offenses
offensetypeover10k <- offensetype[offensetype$Freq > 10000,]
#Categories with over 10000 cases are filtered and arranged according to number of cases
sortedoffense <- offensetypeover10k[order(offensetypeover10k$Freq),]
row.names(sortedoffense) <- NULL
sortedoffense$Var1 <- factor(sortedoffense$Var1)
h = factor(sortedoffense$Var1, levels = c("Aggravated Assault","Simple Assault",
                                          "Intimidation","Destruction/Damage/Vandalism of Property"))
barplot(sortedoffense$Freq~h,xlab = "Offense Type", ylab = "Number of Cases", 
        main = "Most Prevalent Types of Hate Crime Offenses",ylim = c(0,70000),border = "white", col = "#2f0c35")



#[4]Processing data and plotting the stacked area chart of offenses
typesperyear <- data.frame(maindata[which(maindata$DATA_YEAR == 1991 & 
                                            (maindata$OFFENSE_NAME == "Simple Assault" | maindata$OFFENSE_NAME == "Aggravated Assault" | 
                                               maindata$OFFENSE_NAME == "Intimidation" |maindata$OFFENSE_NAME == "Destruction/Damage/Vandalism of Property" )),])
typesperyear <- data.frame(year = rep(1991),table(typesperyear$OFFENSE_NAME))
for(i in 1992:2019){
  currentdata <- data.frame(maindata[which(maindata$DATA_YEAR == i & 
  (maindata$OFFENSE_NAME == "Simple Assault" | maindata$OFFENSE_NAME == "Aggravated Assault" | 
     maindata$OFFENSE_NAME == "Intimidation" |maindata$OFFENSE_NAME == "Destruction/Damage/Vandalism of Property" )),])
  currentdata <- data.frame(year = rep(i),table(currentdata$OFFENSE_NAME))
  typesperyear <- rbind(typesperyear,currentdata)
}
names(typesperyear)[names(typesperyear) == "year"] <- "Year"
names(typesperyear)[names(typesperyear) == "Var1"] <- "Offense"
names(typesperyear)[names(typesperyear) == "Freq"] <- "Cases"
ggplot(typesperyear, aes(x = Year, y = Cases, fill = Offense )) + 
geom_area() + scale_x_continuous(breaks = seq(1991, 2019, by = 1)) + 
  ggtitle("Types Of Offenses Over The Years") + scale_fill_manual(values=c("#6d1b7b", "#4e1458","#2f0c35","#100412"))



#[5]Sorting and plotting the bar chart of bias groups which have more than 10000 total cases
racebiasover10k <- racebias[racebias$Freq > 10000,]
sortedbias2 <- racebiasover10k[order(racebiasover10k$Freq),];row.names(sortedbias2) <- NULL
sortedbias2$Var1 <- factor(sortedbias2$Var1)
g = factor(sortedbias2$Var1, levels = c("Anti-Other Race/Ethnicity/Ancestry","Anti-Hispanic or Latino",
                                        "Anti-Gay (Male)","Anti-White","Anti-Jewish","Anti-Black or African American"))
barplot(sortedbias2$Freq~g,ylim = c(0,80000),ylab = "Number of Cases", xlab = "Bias Group", 
        main = "Highest Recorded Crime Biases",border = "white", col = "#2f0c35")



#[6]Processing data and plotting the stacked line chart for most frequently targeted bias groups
raceperyear <- data.frame(maindata[which(maindata$DATA_YEAR == 1991 & 
(maindata$BIAS_DESC == "Anti-Other Race/Ethnicity/Ancestry" | maindata$BIAS_DESC == "Anti-Jewish" |
   maindata$BIAS_DESC == "Anti-White" |maindata$BIAS_DESC == "Anti-Hispanic or Latino" |
   maindata$BIAS_DESC == "Anti-Gay (Male)" |maindata$BIAS_DESC == "Anti-Black or African American" )),])
raceperyear <- data.frame(year = rep(1991),table(raceperyear$BIAS_DESC))
for(i in 1992:2019){
  currentrace <- data.frame(maindata[which(maindata$DATA_YEAR == i & 
  (maindata$BIAS_DESC == "Anti-Other Race/Ethnicity/Ancestry" |maindata$BIAS_DESC == "Anti-Jewish" |
     maindata$BIAS_DESC == "Anti-White" |maindata$BIAS_DESC == "Anti-Hispanic or Latino" |
     maindata$BIAS_DESC == "Anti-Gay (Male)" |maindata$BIAS_DESC == "Anti-Black or African American" )),])
  currentrace <- data.frame(year = rep(i),table(currentrace$BIAS_DESC))
  raceperyear <- rbind(raceperyear,currentrace)
}
names(raceperyear)[names(raceperyear) == "Var1"] <- "Bias Group"
names(raceperyear)[names(raceperyear) == "year"] <- "Year"
names(raceperyear)[names(raceperyear) == "Freq"] <- "Cases"
ggplot(raceperyear, aes(x = as.factor(Year), y = Cases, colour = `Bias Group`, group = `Bias Group`)) +
  geom_line(size = 1) + xlab("Year") + ggtitle("Most Frequently Targeted Bias Groups per Year")




# Code not used in the final presentation and final report but was used in the first cut 

#Plotting total cases per year
offensesperyear$Var1 <- as.numeric(as.character(offensesperyear$Var1))
plot(offensesperyear$Freq[1:29],type ="l",ylim  = c(4000,10000),
xaxt = "n", ylab = "Cases Per Year", xlab = "Year", 
main = "Total Hate Crime Cases per Year (1991-2019)",lwd = 3, col = "#4a2b7a")
axis(1, at = 1:29, labels = 1991:2019)



#Sorting and plotting the bar chart of location types with over 10000 total cases
locationover10k <- offenselocation[offenselocation$Freq > 10000,]
sortedlocation <- locationover10k[order(locationover10k$Freq),];row.names(sortedlocation) <- NULL
sortedlocation$Var1 <- factor(sortedlocation$Var1)
l = factor(sortedlocation$Var1, levels = c("Parking/Drop Lot/Garage","School/College",
"Other/Unknown","Highway/Road/Alley/Street/Sidewalk","Residence/Home"))
barplot(sortedlocation$Freq~l,ylab = "Number of Cases", xlab = "Location of Incident", 
main = "Location Type With Highest Amount of Recorded Cases",ylim = c(0,70000),border = "white", col = "#2f0c35")


