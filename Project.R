frog<-read.csv("FrogTrackingHab_2026.csv", skip=1)
dryHabitats<-read.csv("DrySeasonRandomHab_2025.csv", skip=1)
wetHabitats<-read.csv("WetSeasonRandomHab_2025.csv", skip=1)

cleanFrogs<-subset(frog, Soil.sample == "F")
cleanFrogs$frogPresence <- 1
cleanDry<-subset(dryHabitats, Soil.sample == "P")
cleanDry$frogPresence <- 0
cleanWet<-subset(wetHabitats, Soil.sample == "P")
cleanWet$frogPresence <- 0

#create a DF of desired columns
columns<-c("frogPresence", "Fern.Density", "Canopy.cover....", 
           "BG", "LL", "SF", "FB", "TS", "VG", "WD", "WA",
           "NE", "SW", "NW", "SE","Latitude", "Longitude")

#combine the dataframes into one
frogData<-rbind(cleanFrogs[, columns], cleanWet[, columns])
#force factors into numerics
frogData$fernDensity<-as.numeric(as.character(frogData$Fern.Density))
frogData$canopyCover<-as.numeric(frogData$Canopy.cover....)
frogData<-na.omit(frogData)

#plot(frogData)

frogModel<-glm(frogPresence~fernDensity+BG+LL+SF+FB+TS,
               data=frogData, family = binomial)
summary(frogModel)

