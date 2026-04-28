library(tidyr)
library(dplyr)
soil<-read.csv("WetSeasonRandomSoil_2025.csv", check.names = FALSE)
season<-read.csv("WetSeasonRandomHab_2025.csv",skip=1, check.names = FALSE)
###################
seasonClean<-fill(season, 'Point #', .direction = "down")
seasonClean<-unite(seasonClean, col='Point #', c('Point #', 'Soil sample'), sep="", remove=FALSE)
data <- left_join(soil, seasonClean, by = c("Soil Smpl #" = "Point #"))

head(data)
#save mollusks as numeric
data$`# mollusks` <- as.numeric(data$`# mollusks`)

#save soil sample as a factor
data$`Soil sample` <- as.factor(data$`Soil sample`)
#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.
mod1<-lm(`% SM` ~ `Soil sample`, data=data)
summary(mod1)
mod2<-lm(`% OM` ~ `Soil sample`, data=data)
summary(mod2)
