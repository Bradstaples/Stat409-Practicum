library(tidyr)
library(dplyr)
library(GGally)
library(leaps)
library(car)
library(lmtest)
library(sjPlot)
library(emmeans)
library(sf)
library(btb)
##################################################################################################
drySoil<-read.csv("DrySeasonRandomSoil_2025-1.csv", check.names = FALSE)
drySeason<-read.csv("DrySeasonRandomHab_2025-1.csv", skip=1, check.names = FALSE)
wetSoil<-read.csv("WetSeasonRandomSoil_2025-1.csv", check.names = FALSE)
wetSeason<-read.csv("WetSeasonRandomHab_2025-1.csv",skip=1, check.names = FALSE)
frog<-read.csv("FrogTrackingHab_2026-1.csv", skip=1, check.names=FALSE)
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
###################################################################################################
####################### wet season data cleaning #########################################
dataWet <- dataWet |>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  subset(!is.na(`Soil sample`)) |>
  mutate(
    `# mollusks` = as.numeric(`# mollusks`),
    `Soil sample` = as.character(`Soil sample`)
  ) |>
  mutate(
    `# mollusks` = case_when(
      `Soil sample` %in% c("P", "F") ~ NA_real_, 
      is.na(`# mollusks`) ~ 0,                   
      TRUE ~ `# mollusks`
    )
  ) |> 
  mutate(`Soil sample` = case_when(
    `Soil sample` == "B" ~ "Base",
    `Soil sample` == "E" ~ "Edge",
    `Soil sample` == "N" ~ "Non-veg",
    `Soil sample` == "P" ~ "Point",
    `Soil sample` == "V" ~ "Veg",
    TRUE ~ `Soil sample` 
  )) |>
  mutate(`Soil sample` = as.factor(`Soil sample`))

# pivot_wider (Added values_fn = mean to prevent list-columns!)
dataWetWide <- dataWet |> 
  pivot_wider(
    id_cols = `Point #`,
    names_from = `Soil sample`, 
    values_from = c(`% SM`, `pH`, `% OM`, `EC`),
    values_fn = mean
  )

# pivot_longer
dataWetLong <- dataWet |>
  pivot_longer(
    cols = c("% SM", "pH", "% OM", "EC"), 
    names_to = "Soil_Property", 
    values_to = "Value"
  )
##################################################################################################
######################################### dry season cleaning ####################################
dataDry <- dataDry |> rename(`% SM` = `% soil moisture`)

dataDry <- dataDry |>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  subset(!is.na(`Point #`)) |>
  mutate(
    `# mollusks` = as.numeric(`# mollusks`),
    `Soil sample` = as.character(`Soil sample`),
    `Point #` = as.numeric(`Point #`)
  ) |>
  mutate(
    `# mollusks` = case_when(
      `Soil sample` %in% c("P", "F") ~ NA_real_,
      is.na(`# mollusks`) ~ 0,
      TRUE ~ `# mollusks`
    )
  ) |> 
  mutate(`Soil sample` = case_when(
    `Soil sample` == "B" ~ "Base",
    `Soil sample` == "E" ~ "Edge",
    `Soil sample` == "N" ~ "Non-veg",
    `Soil sample` == "P" ~ "Point",
    `Soil sample` == "V" ~ "Veg",
    TRUE ~ `Soil sample` 
  )) |>
  mutate(`Soil sample` = as.factor(`Soil sample`))

# pivot_wider (Added values_fn = mean)
dataDryWide <- dataDry |> 
  pivot_wider(
    id_cols = `Point #`,
    names_from = `Soil sample`, 
    values_from = c(`% SM`, `pH`, `% OM`, `EC`),
    values_fn = mean
  )

# pivot_longer
dataDryLong <- dataDry |>
  pivot_longer(
    cols = c("% SM", "pH", "% OM", "EC"), 
    names_to = "Soil_Property", 
    values_to = "Value"
  )
##################################################################################################
############################       binding seasons datasets          #############################
dataAll <- bind_rows(
  dataWet |> mutate(Season = "Wet"),
  dataDry |> mutate(Season = "Dry")
)

# pivot to longboi for graphs and stuff
dataAllLong <- dataAll |>
  pivot_longer(
    cols = c("% SM", "pH", "% OM", "EC"), 
    names_to = "Soil_Property", 
    values_to = "Value"
  )

##################################################################################################
################################## frog data cleaning     #####################################
frogClean <- frog |>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  transform(`Soil sample` = ifelse(grepl("-R1", `Frog ID`), "R1",
                                   ifelse(grepl("-R2", `Frog ID`), "R2", as.character(`Soil sample`)))
  ) |>
  mutate(
    `# mollusks` = as.numeric(`# mollusks`),
    `# burrows` = as.numeric(`# burrows`),
    `Soil pen` = as.numeric(`Soil pen`)) |>
  mutate(
    `# mollusks` = case_when(
      `Soil sample` %in% c("P", "F") ~ NA_real_,
      is.na(`# mollusks`) ~ 0,
      TRUE ~ `# mollusks`
    ),
    `# burrows`= case_when(
      `Soil sample` =="F" ~ NA_real_,
      is.na(`# burrows`) ~ 0,
      TRUE ~ `# burrows`
    ),
    `Soil pen` = case_when(
      `Soil sample` == "F" ~ NA_real_,
      TRUE ~ `Soil pen`
    )
  ) |> 
  mutate(`Soil sample` = case_when(
    `Soil sample` == "B" ~ "Base",
    `Soil sample` == "E" ~ "Edge",
    `Soil sample` == "N" ~ "Non-veg",
    `Soil sample` == "F" ~ "Frog",
    `Soil sample` == "V" ~ "Veg",
    `Soil sample` == "R1" ~ "Random 1",
    `Soil sample` == "R2" ~ "Random 2",
    TRUE ~ `Soil sample` 
  ))
#pivot_wider 
frogWide<-frogClean|>pivot_wider(id_cols = `Frog ID`,
                                 names_from = `Soil sample`, 
                                 values_from = c(`Soil pen`, `# burrows`, `# mollusks`),
                                 values_fn = mean)
#pivot_longer for graphing
frogLong<-frogClean|>pivot_longer(cols = c(`Soil pen`, `# burrows`, `# mollusks`), 
                             names_to = "Variable", 
                             values_to = "Value")

##################################################################################################
#ggpairs plot for wet data
ggpairs(dataWetWide[,c("% SM_Veg", "% SM_Point", "% SM_Non-veg", "% SM_Edge", "% SM_Base")])
ggpairs(dataWetWide[, c("pH_Veg", "pH_Point", "pH_Non-veg", "pH_Edge", "pH_Base" )], 
        title = "pH comparisons of soil samples in wet season")
ggpairs(dataWetWide[, c("EC_Veg", "EC_Point", "EC_Non-veg", "EC_Edge", "EC_Base" )])
ggpairs(dataWetWide[, c("% OM_Veg", "% OM_Point", "% OM_Non-veg", "% OM_Edge", "% OM_Base" )])
##################################################################################################
#ggparis for dry data
ggpairs(dataDryWide[, c("pH_Veg", "pH_Point", "pH_Non-veg", "pH_Edge", "pH_Base" )],
        title = "pH comparisons of soil samples in dry season")
ggpairs(dataDryWide[, c("EC_Veg", "EC_Point", "EC_Non-veg", "EC_Edge", "EC_Base" )])
ggpairs(dataDryWide[, c("% SM_Veg", "% SM_Point", "% SM_Non-veg", "% SM_Edge", "% SM_Base" )])
ggpairs(dataDryWide[, c("% OM_Veg", "% OM_Point", "% OM_Non-veg", "% OM_Edge", "% OM_Base" )])
##################################################################################################
#ggpairs for frog data
#ggpairs(frogWide[, c("Soil pen_Veg", "Soil pen_Non-veg", "Soil pen_Edge", "Soil pen_Base", 
#                     "Soil pen_Random 1", "Soil pen_Random 2" )])
#ggpairs(frogWide[, c("# burrows_Veg", "# burrows_Non-veg", "# burrows_Edge", "# burrows_Base",
#                     "# burrows_Random 1", "# burrows_Random 2")])
#ggpairs(frogWide[, c("# mollusks_Veg", "# mollusks_Non-veg", "# mollusks_Edge", "# mollusks_Base",
#                     "# mollusks_Random 1", "# mollusks_Random 2")])
##################################################################################################
##########################   WET SEASON GRAPHS               ####################################
#ggplot comparing soil properties between soil samples in wet season
ggplot(dataWetLong, aes(x=`Soil sample`, y=Value, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  facet_wrap(~Soil_Property, scales = "free_y")+
  theme_minimal()+
  labs(title="Soil Properties by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="Value")
#soil property trend graphs
ggpairs(dataWet, 
        columns = c("% SM", "pH", "EC", "% OM"),
        mapping = aes(color = `Soil sample`, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 3)), # Shrink the correlation text
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, size = 0.5))) +
  theme_minimal()
#gg boxplot
ggplot(dataWet, aes(x=`Soil sample`, y=`% SM`, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  labs(title="Soil Mositure by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="% Soil Mositure")
#ggplot of everything vs soil moisture
dataWet|>
  select(`Point #`, `Soil sample`, `% SM`, pH, EC, `% OM`) |>
  pivot_longer(cols=c(pH, EC, `% OM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% SM`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method=lm, se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs OM
dataWet|>
  select(`Point #`, `Soil sample`, `% OM`, pH, EC, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% OM`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs EC
dataWet|>
  select(`Point #`, `Soil sample`, `EC`, pH, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `EC`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs pH
dataWet|>
  select(`Point #`, `Soil sample`, `pH`, EC, `% OM`, `% SM`) |>
  pivot_longer(cols=c(EC, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `pH`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs fern density
dataWet|>
  select(`Point #`, `Soil sample`, `Fern Density`, pH, EC, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `Fern Density`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
dataWet|>
  select(`Point #`, `Soil sample`, `# mollusks`, pH, EC, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `# mollusks`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
############ ############ ############ Wet Season Spatial Graphing ############ ############ ############ 
dataWetSF <- st_as_sf(dataWet, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>% 
  st_transform(crs = 2154)
plot(dataWetSF$geometry)

dataWetSFM <- btb_add_centroids(dataWetSF, 
                                iCellSize = 200)
############## ORganic Matter ############## ############## 
centroValuesOM <- dataWetSFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanOM = mean(X..OM, na.rm = TRUE), .groups = "drop")

gridValuesOM <- btb_ptsToGrid(centroValuesOM, 
                             sEPSG = 2154, 
                             iCellSize = 200)
mf_map(x = gridValuesOM, 
       type = "choro", 
       var = "MeanOM",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean OM")

ptsDensity <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanOM = X..OM) %>%
  drop_na(MeanOM)

ptsDensity$sample_density <- 1L

smoothDensity <- btb_smooth(
  pts = ptsDensity,
  sEPSG = 2154,
  iBandwidth = 400,   
  iCellSize = 10     
)
mf_map(x = smoothDensity,
       type = "choro",
       var = "sample_density",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,          
       leg_val_rnd = 1,
       leg_title = "Sampling Density")

smoothDensity <- smoothDensity %>% mutate(meanOM=MeanOM/sample_density)

mf_map(x = smoothDensity,
       type = "choro",
       var="meanOM",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)
############## ############## Fern Denisty map ############## ##############
centroValuesFD <- dataWetSFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanFD = mean(Fern.Density, na.rm = TRUE), .groups = "drop")

gridValuesFD <- btb_ptsToGrid(centroValuesFD, 
                               sEPSG = 2154, 
                               iCellSize = 200)

mf_map(x = gridValuesFD, 
       type = "choro", 
       var = "MeanFD",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Fern Density")
############## ############## Soil Moisture map ############## ##############
centroValuesSM <- dataWetSFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanSM = mean(X..SM, na.rm = TRUE), .groups = "drop")

gridValuesSM <- btb_ptsToGrid(centroValuesSM, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesSM, 
       type = "choro", 
       var = "MeanSM",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Soil Moisture")
############## ############## ph map ############## ##############
centroValuesPH <- dataWetSFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanPH = mean(pH, na.rm = TRUE), .groups = "drop")

gridValuesPH <- btb_ptsToGrid(centroValuesPH, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesPH, 
       type = "choro", 
       var = "MeanPH",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean pH")

############## ############## EC map ############## ##############
centroValuesEC <- dataWetSFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanEC = mean(EC, na.rm = TRUE), .groups = "drop")

gridValuesEC <- btb_ptsToGrid(centroValuesEC, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesEC, 
       type = "choro", 
       var = "MeanEC",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean EC")

############## ############## mollusks map ############## ##############
centroValuesMollusks <- dataWetSFM %>%
  st_drop_geometry() %>%
  filter(Soil.sample %in% c("Edge", "Veg", "Non-veg")) %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanMollusks = mean(X..mollusks, na.rm = TRUE), .groups = "drop")
gridValuesMollusks <- btb_ptsToGrid(centroValuesMollusks, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesMollusks, 
       type = "choro", 
       var = "MeanMollusks",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Mollusks")
###################################################################################################
####################     DRY SEASON GRAPHS     ##########################################
#ggplot comparing soil properties between soil samples in wet season
ggplot(dataDryLong, aes(x=`Soil sample`, y=Value, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  facet_wrap(~Soil_Property, scales = "free_y")+
  theme_minimal()+
  labs(title="Soil Properties by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="Value")
#soil property trend graphs
ggpairs(dataDry, 
        columns = c("% SM", "pH", "EC", "% OM"),
        mapping = aes(color = `Soil sample`, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 3)), # Shrink the correlation text
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, size = 0.5))) +
  theme_minimal()
#gg boxplot
ggplot(dataDry, aes(x=`Soil sample`, y=`% SM`, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  labs(title="Soil Mositure by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="% Soil Mositure")
#ggplot of everything vs soil moisture
dataDry|>
  select(`Point #`, `Soil sample`, `% SM`, pH, EC, `% OM`) |>
  pivot_longer(cols=c(pH, EC, `% OM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% SM`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method=lm, se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs OM
dataDry|>
  select(`Point #`, `Soil sample`, `% OM`, pH, EC, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% OM`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs EC
dataDry|>
  select(`Point #`, `Soil sample`, `EC`, pH, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `EC`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs pH
dataDry|>
  select(`Point #`, `Soil sample`, `pH`, EC, `% OM`, `% SM`) |>
  pivot_longer(cols=c(EC, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `pH`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
#everything vs # mollusks
dataDry|>
  select(`Point #`, `Soil sample`, `# mollusks`, pH, EC, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `# mollusks`, y= Values, color=`Soil sample`))+
  geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(~Variable, scales="free_y", ncol=1)
############ ############ ############ Wet Season Spatial Graphing ############ ############ ############ 
dataDrySF <- st_as_sf(dataDry, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>% 
  st_transform(crs = 2154)
plot(dataDrySF$geometry)

dataDrySFM<-btb_add_centroids(dataDrySF, 
                              iCellSize = 200)
########  OM SPATIAL MAP  ###########################################################
centroValuesOM <- dataDrySFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanOM = mean(X..OM, na.rm = TRUE), .groups = "drop")
gridValuesOM <- btb_ptsToGrid(centroValuesOM, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesOM, 
       type = "choro", 
       var = "MeanOM",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean OM")
########  SM SPATIAL MAP  ###########################################################
centroValuesSM <- dataDrySFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanSM = mean(X..SM, na.rm = TRUE), .groups = "drop")
gridValuesSM <- btb_ptsToGrid(centroValuesSM, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesSM, 
       type = "choro", 
       var = "MeanSM",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Soil Moisture")
########  ph SPATIAL MAP  ###########################################################
centroValuesPH <- dataDrySFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanPH = mean(pH, na.rm = TRUE), .groups = "drop")
gridValuesPH <- btb_ptsToGrid(centroValuesPH, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesPH, 
       type = "choro", 
       var = "MeanPH",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean pH")
########  EC SPATIAL MAP  ###########################################################
centroValuesEC <- dataDrySFM %>%
  st_drop_geometry() %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanEC = mean(EC, na.rm = TRUE), .groups = "drop")
gridValuesEC <- btb_ptsToGrid(centroValuesEC, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesEC, 
       type = "choro", 
       var = "MeanEC",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean EC")

################################################################################################
########   SEASON COMPARISON GRAPHS   ###########################################################
#GGplot of season shifts in soil properties by soil sample
dataAllLong|>
  ggplot(aes(x=Season, y=Value, fill=Season))+
  geom_boxplot(alpha=0.65)+
  facet_grid(Soil_Property ~ `Soil sample`, scales = "free_y") +
  theme_minimal()+
  labs(title="Seasonal shifts in soil properties by soil sample",
       x= "Season",
       y="Value")
##############################################################################################
############################      FROG GRAPHS    #############################################
frogLong |>
  filter(`Soil sample` != "Frog") |> 
  ggplot(aes(x = `Soil sample`, y = Value, fill = `Soil sample`)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.015, alpha = 0.5, size = 1.5, color = "darkgray") +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "How Microhabitat Drives Soil Hardness and Fauna",
       x = "Microhabitat Type (Base, Edge, etc.)",
       y = "Measured Value") +
  theme(legend.position = "none")
#probabilty graph of finding a burrow
frogClean |>
  filter(`Soil sample` != "Frog", !is.na(`# burrows`),
         `Soil sample` != "Edge") |> 
  mutate(Burrow = ifelse(`# burrows` > 0, "Present", "Absent")) |>
  ggplot(aes(x = `Soil sample`, fill = Burrow)) +
  geom_bar(position = "fill", color = "black", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = c("Absent" = "#d3d3d3", "Present" = "#2ca25f")) +
  labs(title = "Probability of Finding a Burrow by Microhabitat",
       x = "Microhabitat",
       y = "Percentage of Samples")
#soil graph 
frogClean |>
  filter(`Soil sample` != "Frog") |> 
  pivot_longer(cols = c(`# burrows`, `# mollusks`), 
               names_to = "Animal", 
               values_to = "Count") |>
  ggplot(aes(x = `Soil pen`, y = Count, color = `Soil sample`)) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE) +
  facet_wrap(~Animal, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Does Soil Hardness Restrict Burrowers and Mollusks?",
       x = "Soil Penetration (Hardness)",
       y = "Count per Sample")
#graph comparing fern presence and soil pen
frogClean |>
  filter(`Soil sample` != "Frog") |>
  ggplot(aes(x = `Soil sample`, y = `Soil pen`, fill = `Soil sample`)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2, color = "darkgray") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "#d3d3d3", "1" = "#2ca25f")) +
  labs(title = "Does Fern Presence Affect Soil Hardness?",
       x = "Fern Presence (0 = Absent, 1 = Present)",
       y = "Soil Penetration (Hardness)") +
  theme(legend.position = "none")

############ ############ ############ FROGGO Spatial Graphing ############ ############ ############ 
#fill longitude and latitude valuesfopr frog data
frogMap<-fill(frogClean, Longitude, Latitude)
frogSF <- st_as_sf(frogMap, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>% 
  st_transform(crs = 2154)
plot(frogSF$geometry)

frogSFM<-btb_add_centroids(frogSF, 
                              iCellSize = 200)
############################################### FRRRROOOOOG SPATIAL GRPAHING #######################
####### Spatial graphing of mollusk
centroValuesMollusks <- frogSFM %>%
  st_drop_geometry() %>%
  filter(Soil.sample %in% c("Edge", "Veg", "Non-veg")) %>%
  mutate(X..mollusks = as.numeric(X..mollusks)) %>%
  drop_na(X..mollusks) %>%
  group_by(x_centro, y_centro) %>%
  summarise(MeanMollusks = mean(X..mollusks, na.rm = TRUE), .groups = "drop")
gridValuesMollusks <- btb_ptsToGrid(centroValuesMollusks, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesMollusks, 
       type = "choro", 
       var = "MeanMollusks",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Mollusks")
############SPATIAL graping of da burrows
centroValuesBurrows <- frogSFM %>%
  st_drop_geometry() %>%
  mutate(X..burrows = as.numeric(X..burrows)) %>%
  group_by(x_centro, y_centro) %>%
  drop_na(X..burrows) %>%
  summarise(MeanBurrows = mean(X..burrows, na.rm = TRUE), .groups = "drop")
gridValuesBurrows <- btb_ptsToGrid(centroValuesBurrows, 
                               sEPSG = 2154, 
                               iCellSize = 200)
mf_map(x = gridValuesBurrows, 
       type = "choro", 
       var = "MeanBurrows",        
       breaks = "quantile", 
       nbreaks = 5, 
       lwd = 1, 
       leg_val_rnd = 1,
       leg_title = "Mean Burrows")
##################################################################################################
#########################  MODELING AND TESTING  #################################################
##################################################################################################
#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.
####### Validation testing function #######
validateModel <- function(model) {
  # Check for multicollinearity
  print("Variance Inflation Factors:")
  print(vif(model))
  
  # Check for autocorrelation
  print("Durbin-Watson Test:")
  print(dwtest(model))
  
  # Check for homoscedasticity
  fitted_values <- model$fitted.values
  residuals <- model$residuals
  group <- fitted_values > median(fitted_values)
  
  print("Variance Test for Homoscedasticity:")
  print(var.test(residuals[group], residuals[!group]))
  
  #Check Confidence Intervals
  print("Confidence Intervals:")
  print(confint(model))
  
  #fitted vs residual plot
  plot(fitted_values, residuals, main = "Fitted vs Residuals", xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, col = "red")
  
  #qqplot
  qqnorm(residuals, main = "QQ Plot of Residuals")
  qqline(residuals, col = "red")
}

##stepwise model selection
dataStep<- dataAll|>
  select(`% SM`, `Soil sample`, Season, pH, `% OM`, EC, `# mollusks`)|>
  drop_na()


#SOil Moisture  
fullModel1<-lm(`% SM`~`Soil sample`*Season+`Season` +`pH`+`% OM`+`EC`+`# mollusks`, data=dataStep)
fullModel2<-lm(`% SM`~`Soil sample`+`Season` +`pH`+`% OM`+`EC`+`# mollusks`, data=dataStep)
nullModel1<-lm(`% SM`~1, data=dataStep)
stepwiseModel1<-step(nullModel1, scope=list(lower=nullModel1, upper=fullModel1), direction="forward")
stepwiseModel2<-step(nullModel1, scope=list(lower=nullModel1, upper=fullModel2), direction="forward")
summary(stepwiseModel1)
summary(stepwiseModel2)

#Organic Matter
fullModel3<-lm(`% OM`~`Soil sample`*Season+`Season` +`pH`+`% SM`+`EC`+`# mollusks`, data=dataStep)
fullModel4<-lm(`% OM`~`Soil sample`+`Season` +`pH`+`% SM`+`EC`+`# mollusks`, data=dataStep)
nullModel2<-lm(`% OM`~1, data=dataStep)
stepwiseModel1<-step(nullModel2, scope=list(lower=nullModel2, upper=fullModel3), direction="forward")
stepwiseModel2<-step(nullModel2, scope=list(lower=nullModel2, upper=fullModel4), direction="forward")
summary(stepwiseModel1)
summary(stepwiseModel2)

##################################################################################################
################  Wet Season Models w/soil sample ###################### 
modWetSM<- lm(`% SM`~`Soil sample`+`% OM`+pH+EC+`# mollusks`+`Fern Density`, data=dataWet)
summary(modWetSM)
modWetOM<- lm(`% OM`~`Soil sample`+`% SM`+pH+EC+`# mollusks`+`Fern Density`, data=dataWet)
summary(modWetOM)
modWetFern<- lm(`Fern Density`~`Soil sample`+`% SM`+`% OM`+pH+EC+`# mollusks`, data=dataWet)
summary(modWetFern)

#cValidation testing
validateModel(modWetSM)
validateModel(modWetOM)
validateModel(modWetFern)

#confint plots
plot_model(modWetSM,show.values = TRUE, value.offset = .3, title = "Predictors of Soil Moisture (Wet Season)")
plot_model(modWetOM, show.values = TRUE, value.offset = .3, title = "Predictors of Organic Matter (Wet Season)")

#pairwise comparisons of soil sample types with emmeans
emmeans(modWetSM, pairwise ~ `Soil sample`)
emmeans(modWetOM, pairwise ~ `Soil sample`)


##################################################################################################
################## Wet Season models w/ fern presence #####################
modWetSMFern<- lm(`% SM`~FernPresence+`% OM`+pH+EC, data=dataWet)
summary(modWetSMFern)
modWetOMFern<- lm(`% OM`~FernPresence+`% SM`+pH+EC, data=dataWet)
summary(modWetOMFern)
modWetFernFern<- lm(`Fern Density`~`% SM`+`% OM`+pH+EC, data=dataWet)
summary(modWetFernFern)

#validation testing
validateModel(modWetSMFern)
validateModel(modWetOMFern)
validateModel(modWetFernFern)

#confint plots
plot_model(modWetSMFern,show.values = TRUE, value.offset = .3, title = "Predictors of Soil Moisture (Wet Season, Fern Presence)")
plot_model(modWetOMFern, show.values = TRUE, value.offset = .3, title = "Predictors of Organic Matter (Wet Season, Fern Presence)")
plot_model(modWetFernFern, show.values = TRUE, value.offset = .3, title = "Predictors of Fern Density (Wet Season, Fern Presence)")

##################################################################################################
################################ Mollusks #################################################
dataWetMollusk <- dataWet |>
  filter(`Soil sample` %in% c("Edge", "Veg", "Non-veg"))
modWetMollusk1<- lm(`% SM`~`Soil sample`+`% OM`+pH+EC, data=dataWetMollusk)
summary(modWetMollusk1)
modWetMollusk2<- lm(`% OM`~`Soil sample`+`% OM`+pH+EC, data=dataWetMollusk)
summary(modWetMollusk2)
modWetMollusks3<-lm(`# mollusks`~`Soil sample`+`% SM`+`% OM`+pH+EC, data=dataWetMollusk)
summary(modWetMollusks3)

##################################################################################################
#simple models wet
modWetSimpleSM<-lm(`% SM`~`Soil sample`, data=dataWet)
summary(modWetSimpleSM)
emmeans(modWetSimpleSM, pairwise ~ `Soil sample`)

modWetSimpleOM<-lm(`% OM`~`Soil sample`, data=dataWet)
summary(modWetSimpleOM)
emmeans(modWetSimpleOM, pairwise ~ `Soil sample`)

modWetSimpleFern<-lm(`Fern Density`~`Soil sample`, data=dataWet)
summary(modWetSimpleFern)
emmeans(modWetSimpleFern, pairwise ~ `Soil sample`)

modWetSimplePH<-lm(pH~`Soil sample`, data=dataWet)
summary(modWetSimplePH)
emmeans(modWetSimplePH, pairwise ~ `Soil sample`)

modWetSimpleEC<-lm(EC~`Soil sample`, data=dataWet)
summary(modWetSimpleEC)
emmeans(modWetSimpleEC, pairwise ~ `Soil sample`)

modWetSimpleMollusks<-lm(`# mollusks`~`Soil sample`, data=dataWetMollusk)
summary(modWetSimpleMollusks)
emmeans(modWetSimpleMollusks, pairwise ~ `Soil sample`)

##################################################################################################
############## Dry Season Models ####################### 
modDrySM<- lm(`% SM`~`Soil sample`+`% OM`+pH+EC+`# mollusks`, data=dataDry)
summary(modDrySM)
modDryOM<- lm(`% OM`~`Soil sample`+`% SM`+pH+EC+`# mollusks`, data=dataDry)
summary(modDryOM)

#simple models dry
modDrySimpleSM<-lm(`% SM`~`Soil sample`, data=dataDry)
summary(modDrySimpleSM)
emmeans(modDrySimpleSM, pairwise ~ `Soil sample`)

modDrySimpleOM<-lm(`% OM`~`Soil sample`, data=dataDry)
summary(modDrySimpleOM)
emmeans(modDrySimpleOM, pairwise ~ `Soil sample`)

modDrySimplePH<-lm(pH~`Soil sample`, data=dataDry)
summary(modDrySimplePH)
emmeans(modDrySimplePH, pairwise ~ `Soil sample`)

modDrySimpleEC<-lm(EC~`Soil sample`, data=dataDry)
summary(modDrySimpleEC)
emmeans(modDrySimpleEC, pairwise ~ `Soil sample`)

##################################################################################################
################  Both Season Models ####################
modAll1<- lm(`% SM`~`Soil sample`*Season+`% OM`+pH+EC, data=dataAll)
summary(modAll1)
modAll2<- lm(`% OM`~`Soil sample`*Season+`% SM`+pH+EC, data=dataAll)
summary(modAll2)

emmeans(modAll1, pairwise ~ `Soil sample`*Season)
emmeans(modAll2, pairwise ~ `Soil sample`*Season)


##################################################################################################
################ froggie based models ################
modFrog1<- glm(`# mollusks` ~ `Soil sample`+`# burrows`, data=frogClean, family = "poisson")
summary(modFrog1)
modFrog2<- glm(`# burrows` ~ `Soil sample`, data=frogClean, family = "poisson")
summary(modFrog2)
modFrog3<- lm(`Soil pen` ~ `Soil sample`, data=frogClean)
summary(modFrog3)

#validation function for poisson models
validatePoissonModel <- function(model) {
  # Check for overdispersion
  dispersion <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  print(paste("Dispersion:", dispersion))
  
  # Check for zero-inflation
  zero_count <- sum(residuals(model, type = "response") == 0)
  total_count <- length(residuals(model, type = "response"))
  print(paste("Zero-inflation:", zero_count / total_count))
  
  # Check for multicollinearity
  print("Variance Inflation Factors:")
  print(vif(model))
}
validatePoissonModel(modFrog1)
