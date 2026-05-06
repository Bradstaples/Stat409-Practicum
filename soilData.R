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
  subset(!is.na(`Soil sample`)) |>
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
  subset(!is.na(`Point #`))|>
  transform(`# mollusks` = as.numeric(`# mollusks`),
            `Soil sample` = as.factor(`Soil sample`)
  )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`))
#pivotWide for dry season just like wet season
dataDryWide <- dataDry |> pivot_wider(id_cols=`Point #`,
                                      names_from = `Soil sample`, 
                                      values_from = c(`% soil moisture`,`pH`,`% OM`,`EC`))
##################################################################################################
#frog data cleaning
frogClean<- frog|> 
  transform(`Soil sample` = ifelse(grepl("-R1", `Frog ID`, "R1"),
                            ifelse(grepl("-R2", `Frog ID`, "R2"), as.character(`Soil sample`))),) |>         
  #subset(!grepl("R1|R2", `Frog ID`)) |>
  transform(`# mollusks` = as.numeric(`# mollusks`),
            `Soil sample` = as.factor(`Soil sample`),
            `# burrows` = as.numeric(`# burrows`),
            `Soil pen` = as.numeric(`Soil pen`)
            
  )|>
  transform(`Frog ID` = gsub("-R1|-R2", "", `Frog ID`, ignore.case = TRUE),
            `# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`),
            `Soil pen` = ifelse(is.na(`Soil pen`), 0, `Soil pen`),
            `# burrows` = ifelse(is.na(`# burrows`), 0, `# burrows`))


frogWide<-frogClean|>pivot_wider(id_cols = `Frog ID`,
                                 names_from = `Soil sample`, 
                                 values_from = c(`Soil pen`, `# burrows`, `# mollusks`))
##################################################################################################
#ggpairs plot for wet data
ggpairs(dataWetWide[,c("% SM_V", "% SM_P", "% SM_N", "% SM_E", "% SM_B")])
ggpairs(dataWetWide[, c("pH_V", "pH_P", "pH_N", "pH_E", "pH_B" )], 
        title = "pH comparisons of soil samples in wet season")
ggpairs(dataWetWide[, c("EC_V", "EC_P", "EC_N", "EC_E", "EC_B" )])
ggpairs(dataWetWide[, c("% OM_V", "% OM_P", "% OM_N", "% OM_E", "% OM_B" )])
#ggparis for dry data
ggpairs(dataDryWide[, c("pH_V", "pH_P", "pH_N", "pH_E", "pH_B" )],
        title = "pH comparisons of soil samples in dry season")
ggpairs(dataDryWide[, c("EC_V", "EC_P", "EC_N", "EC_E", "EC_B" )])
ggpairs(dataDryWide[, c("% soil moisture_V", "% soil moisture_P", "% soil moisture_N", "% soil moisture_E", "% soil moisture_B" )])
ggpairs(dataDryWide[, c("% OM_V", "% OM_P", "% OM_N", "% OM_E", "% OM_B" )])
#ggpairs for frog data
ggpairs(frogWide[, c("Soil pen_V", "Soil pen_F", "Soil pen_N", "Soil pen_E", "Soil pen_B", 
                     "Soil pen_R1", "Soil pen_R2" )])
ggpairs(frogWide[, c("# burrows_V", "# burrows_F", "# burrows_N", "# burrows_E", "# burrows_B",
                     "# burrows_R1", "# burrows_R2")])
ggpairs(frogWide[, c("# mollusks_V", "# mollusks_F", "# mollusks_N", "# mollusks_E", "# mollusks_B",
                     "# mollusks_R1", "# mollusks_R2")])
##################################################################################################
plot(dataWet$`Soil sample`, dataWet$`% SM`, 
     main="Soil comparisons of Soil Moisture %", xlab-"Sample Region", ylab="Moisture %")
plot(dataWet$`Soil sample`, dataWet$`# mollusks`)
plot(dataWet$`Soil sample`, dataWet$`ph`)
plot(dataWet$`Soil sample`, dataWet$`Fern Density`)
plot(dataWet$`Soil sample`, dataWet$`EC`)
plot(dataWet$`Soil sample`, dataWet$`Fern Density`)
prop.table(table(dataWet$`Soil sample`, dataWet$`Fern Density`))
prop.table(table(dataWet$`Soil sample`))
plot(dataWet$`% SM`, dataWet$`EC`)
mod1<-lm(dataWet$`EC`~dataWet$`% SM`)
abline(mod1)

#gg boxplot
ggplot(dataWet, aes(x=`Soil sample`, y=`% SM`, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  labs(title="Soil Mositure by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="% Soil Mositure")
#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.
mod1<-lm(`% SM` ~ `Soil sample`, data=dataWet)
summary(mod1)
mod2<-lm(`% OM` ~ `Soil sample`, data=dataWet)
summary(mod2)
