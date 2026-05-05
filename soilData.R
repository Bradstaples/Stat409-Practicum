library(tidyr)
library(dplyr)
library(GGally)
drySoil<-read.csv("DrySeasonRandomSoil_2025.csv", check.names = FALSE)
drySeason<-read.csv("DrySeasonRandomHab_2025.csv", skip=1, check.names = FALSE)
wetSoil<-read.csv("WetSeasonRandomSoil_2025.csv", check.names = FALSE)
wetSeason<-read.csv("WetSeasonRandomHab_2025.csv",skip=1, check.names = FALSE)
frog<-read.csv("FrogTrackingHab_2026.csv", skip=1, check.names=FALSE)
############################################################################################
wetSeasonClean<-fill(wetSeason, 'Point #', .direction = "down")
wetSeasonClean<-unite(wetSeasonClean, col='Soil ID', c('Point #', 'Soil sample'), sep="", remove=FALSE)
dataWet <- left_join(wetSoil, wetSeasonClean, by = c("Soil Smpl #" = "Soil ID"))
############################################################################################
drySeason$`Point #`[drySeason$`Point #` == ""] <- NA
drySeasonClean<-fill(drySeason, 'Point #', .direction = "down")
drySeasonClean<-unite(drySeasonClean, col='Soil ID', c('Point #', 'Soil sample'), sep="", remove=FALSE)
dataDry <- left_join(drySoil, drySeasonClean, by = c("Soil Smpl #" = "Soil ID"))
############################################################################################
colnames(frog) <- make.unique(colnames(frog))
#change blanks in frog ID to NA
frog$`Frog ID`[frog$`Frog ID` == ""] <- NA
frog<-fill(frog, 'Frog ID', .direction = "down")
####################################################
##################################################################################################
#wet season data cleaning
dataWet<- dataWet|>
  transform(`# mollusks` = as.numeric(`# mollusks`),
         `Soil sample` = as.factor(`Soil sample`)
         )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`))
#use pivot_wider to make rows based on each point #and columns based on soil sample, with values as % SM
dataWetWide <- dataWet |> pivot_wider(id_cols=`Point #`,
                                names_from = `Soil sample`, 
                                values_from = c(`% SM`,`pH`,`% OM`,`EC`))
##################################################################################################
#dry season cleaning
dataDry<- dataDry|>
  transform(`# mollusks` = as.numeric(`# mollusks`),
            `Soil sample` = as.factor(`Soil sample`)
  )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`))
#pivotWide for dry season just like wet season
dataDryWide <- dataDry |> pivot_wider(id_cols=`Point #`,
                                      names_from = `Soil sample`, 
                                      values_from = c(`% SM`,`pH`,`% OM`,`EC`))
##################################################################################################
#frog data cleaning
frogClean<- frog|> 
  subset(!grepl("R1|R2", `Frog ID`)) |>
  transform(`# mollusks` = as.numeric(`# mollusks`),
            `Soil sample` = as.factor(`Soil sample`),
            `# burrows` = as.numeric(`# burrows`),
            `Soil pen` = as.numeric(`Soil pen`)
            
  )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`),
            `Soil pen` = ifelse(is.na(`Soil pen`), 0, `Soil pen`),
            `# burrows` = ifelse(is.na(`# burrows`), 0, `# burrows`))
frogWide<-frogClean|>pivot_wider(id_cols = `Frog ID`,
                                 names_from = `Soil sample`, 
                                 values_from = c(`Soil pen`, `# burrows`))
##################################################################################################
#ggpairs plot for dataWide
ggpairs(dataWetWide[,c("% SM_V", "% SM_P", "% SM_N", "% SM_E", "% SM_B")])
ggpairs(dataWetWide[, c("pH_V", "pH_P", "pH_N", "pH_E", "pH_B" )])
ggpairs(dataWetWide[, c("EC_V", "EC_P", "EC_N", "EC_E", "EC_B" )])
ggpairs(dataWetWide[, c("% OM_V", "% OM_P", "% OM_N", "% OM_E", "% OM_B" )])
##################################################################################################
plot(data$`Soil sample`, data$`% SM`)
plot(data$`Soil sample`, data$`# mollusks`)
plot(data$`Soil sample`, data$`ph`)
plot(data$`Soil sample`, data$`Fern Density`)
plot(data$`Soil sample`, data$`EC`)
plot(data$`Soil sample`, data$`Fern Density`)
prop.table(table(data$`Soil sample`, data$`Fern Density`))
prop.table(table(data$`Soil sample`))
plot(data$`% SM`, data$EC)
mod1<-lm(data$`EC`~data$`% SM`)
abline(mod1)


#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.
mod1<-lm(`% SM` ~ `Soil sample`, data=data)
summary(mod1)
mod2<-lm(`% OM` ~ `Soil sample`, data=data)
summary(mod2)

mod1<-lm(`% SM` ~ `Soil sample`, data=dataAgg)
summary(mod1)
mod2<-lm(`% OM` ~ `Soil sample`, data=dataAgg)
summary(mod2)
