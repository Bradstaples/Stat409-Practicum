library(tidyr)
soil<-read.csv("WetSeasonRandomSoil_2025.csv")
season<-read.csv("WetSeasonRandomHab_2025.csv",skip=1)

soilClean<-separate(soil, col='Soil.Smpl..',
                    into=c('Point #', 'Soil Sample'), sep=-1)
head(soilClean)
seasonClean<-season[rep(1:nrow(season), by="Point.." each = 4), ]