library(stats)
library(MASS)

frog<-read.csv("FrogTrackingHab_2026.csv", skip=1, check.names=FALSE)

#cleanfrogs1 but a subset where frog id does not cointain r1 or r2 but it does cointain an ID, ie not blank
cleanFrogs1 <- subset(frog, !grepl("R1|R2", `Frog ID`) & `Frog ID` != "")
cleanFrogs1$frogPresence <- 1

cleanFrogs2 <- subset(frog, grepl("R1|R2", `Frog ID`))
cleanFrogs2$frogPresence <- 0

#create a DF of desired columns
columns <- c("frogPresence", "Fern Density", "Canopy cover (%)", "Frond count", 
             "Fern Health", "Fern diam (cm)", "Fern Complx", "Plant sp.", 
             "Slope   (deg)", "BG", "LL", "SF", "FB", "TS", "VG", "WD", "WA", 
             "NE", "SW", "NW", "SE", "Latitude", "Longitude")

frogData <- rbind(cleanFrogs1[, columns], cleanFrogs2[, columns])

#force factors into numerics
frogData$fernDensity<- as.numeric(frogData$`Fern Density`)
frogData$canopyCover<- as.numeric(frogData$`Canopy cover (%)`)
frogData$frondCount<- as.numeric(frogData$`Frond count`)
frogData$fernDiam<- as.numeric(frogData$`Fern diam (cm)`)
frogData$plantSp<- as.factor(frogData$`Plant sp.`)
frogData$fernHealth<- as.factor(frogData$`Fern Health`)
frogData$slope<- as.numeric(frogData$`Slope   (deg)`)

#removing nas from fern denisty and canopy cover
frogData <- frogData[!is.na(frogData$fernDensity) & 
                       !is.na(frogData$canopyCover) & 
                       !is.na(frogData$frondCount) & 
                       !is.na(frogData$fernDiam), ]
#cleaning dataset

plot(frogData)

frogModel <- glm.nb(frogPresence ~ fernDensity + canopyCover + frondCount + 
                      fernHealth + fernDiam + `Fern Complx` + slope + 
                      BG + LL + SF + FB + TS, 
                    data = frogData)

summary(frogModel)

