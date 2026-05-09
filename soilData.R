library(tidyr)
library(dplyr)
library(GGally)
library(leaps)
library(car)
library(lmtest)
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
dataWet<- dataWet|>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  subset(!is.na(`Soil sample`)) |>
  transform(`# mollusks` = as.numeric(`# mollusks`),
         `Soil sample` = as.factor(`Soil sample`)
         )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`)) |> 
  mutate(`Soil sample` = case_when(
    `Soil sample` == "B" ~ "Base",
    `Soil sample` == "E" ~ "Edge",
    `Soil sample` == "N" ~ "Non-veg",
    `Soil sample` == "P" ~ "Point",
    `Soil sample` == "V" ~ "Veg",
    TRUE ~ `Soil sample` 
  ))
#use pivot_wider to make rows based on each point #and columns based on soil sample, with values as % SM
dataWetWide <- dataWet |> pivot_wider(id_cols=`Point #`,
                                names_from = `Soil sample`, 
                                values_from = c(`% SM`,`pH`,`% OM`,`EC`))
#pivot_longer on soil properties for graphing 
dataWetLong <- dataWet |>
  pivot_longer(cols = c("% SM", "pH", "% OM", "EC"), 
               names_to = "Soil_Property", 
               values_to = "Value")
##################################################################################################
######################################### dry season cleaning ####################################
dataDry <- dataDry |> rename(`% SM` = `% soil moisture`)
dataDry<- dataDry|>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  subset(!is.na(`Point #`))|>
  transform(`# mollusks` = as.numeric(`# mollusks`),
            `Soil sample` = as.factor(`Soil sample`),
            `Point #` = as.numeric(`Point #`)
  )|>
  transform(`# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`))|> 
  mutate(`Soil sample` = case_when(
    `Soil sample` == "B" ~ "Base",
    `Soil sample` == "E" ~ "Edge",
    `Soil sample` == "N" ~ "Non-veg",
    `Soil sample` == "P" ~ "Point",
    `Soil sample` == "V" ~ "Veg",
    TRUE ~ `Soil sample` 
  ))
#pivotWide for dry season just like wet season
dataDryWide <- dataDry |> pivot_wider(id_cols=`Point #`,
                                      names_from = `Soil sample`, 
                                      values_from = c(`% SM`,`pH`,`% OM`,`EC`))
#pivot_longer on soil properties to help with plotting
dataDryLong <- dataDry |>
  pivot_longer(cols = c("% SM", "pH", "% OM", "EC"), 
               names_to = "Soil_Property", 
               values_to = "Value")
##################################################################################################
################################## frog data cleaning     #####################################
frogClean<- frog|>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  transform(`Soil sample` = ifelse(grepl("-R1", `Frog ID`), "R1",
                            ifelse(grepl("-R2", `Frog ID`), "R2", as.character(`Soil sample`))),
            `# mollusks` = as.numeric(`# mollusks`),
            `# burrows` = as.numeric(`# burrows`),
            `Soil pen` = as.numeric(`Soil pen`)
  ) |>
  transform(`Frog ID` = gsub("-R1|-R2", "", `Frog ID`, ignore.case = TRUE),
            `# mollusks` = ifelse(is.na(`# mollusks`), 0, `# mollusks`),
            `Soil pen` = ifelse(is.na(`Soil pen`), 0, `Soil pen`),
            `# burrows` = ifelse(is.na(`# burrows`), 0, `# burrows`))|> 
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

frogWide<-frogClean|>pivot_wider(id_cols = `Frog ID`,
                                 names_from = `Soil sample`, 
                                 values_from = c(`Soil pen`, `# burrows`, `# mollusks`),
                                 values_fn = mean)

frogLong<-frogClean|>pivot_longer(cols = c(`Soil pen`, `# burrows`, `# mollusks`), 
                             names_to = "Variable", 
                             values_to = "Value")
##################################################################################################
#binding seasons datasets
dataAll<- bind_rows(dataWet |> mutate(Season = "Wet"),
                dataDry |> mutate(Season = "Dry"))
#pivot to longboi for graphs and stuff
dataAllLong <- dataAll |>
  pivot_longer(cols = c("% SM", "pH", "% OM", "EC"), 
               names_to = "Soil_Property", 
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
ggpairs(frogWide[, c("Soil pen_Veg", "Soil pen_Frog", "Soil pen_Non-veg", "Soil pen_Edge", "Soil pen_Base", 
                     "Soil pen_Random 1", "Soil pen_Random 2" )])
ggpairs(frogWide[, c("# burrows_Veg", "# burrows_Frog", "# burrows_Non-veg", "# burrows_Edge", "# burrows_Base",
                     "# burrows_Random 1", "# burrows_Random 2")])
ggpairs(frogWide[, c("# mollusks_Veg", "# mollusks_Frog", "# mollusks_Non-veg", "# mollusks_Edge", "# mollusks_Base",
                     "# mollusks_Random 1", "# mollusks_Random 2")])
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

################################################################################################
########   SEASON COMPARISON GRAPHS   ###########################################################
#GGplot of season shifts in soil properties by soil sample
dataAllLong|>
  ggplot(aes(x=Season, y=Value, fill=Season))+
  geom_boxplot(alpha=0.65)+
  facet_wrap(~interaction(Soil_Property, `Soil sample`), scales = "free_y")+
  theme_minimal()+
  labs(title="Seasonal shifts in soil properties by soil sample",
       x= "Season",
       y="Value")
##############################################################################################
############################FROG GRAPHS########################################################
ggplot(frogLong, aes(x = `Soil sample`, y = Value, fill = `Soil sample`)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  # put dots on the graph for the actual counts for each sample, with some jitter to avoid overlap
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5, color = "darkgray") +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "How Microhabitat Drives Soil Hardness and Fauna",
       x = "Microhabitat Type (Base, Edge, etc.)",
       y = "Measured Value")
#probabilty graph of finding a burrow
frogClean |>
  mutate(Has_Burrow = ifelse(`# burrows` > 0, "Present", "Absent")) |>
  ggplot(aes(x = `Soil sample`, fill = Has_Burrow)) +
  geom_bar(position = "fill", color = "black", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = c("Absent" = "#d3d3d3", "Present" = "#2ca25f")) +
  labs(title = "Probability of Finding a Burrow by Microhabitat",
       x = "Microhabitat",
       y = "Percentage of Samples")
#soil graph 
frogClean |>
  pivot_longer(cols = c(`# burrows`, `# mollusks`), 
               names_to = "Animal", 
               values_to = "Count") |>
  ggplot(aes(x = `Soil pen`, y = Count, color = `Soil sample`)) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.7, size = 2) +
  # Use the Poisson curve designed for count data!
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE) +
  facet_wrap(~Animal, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Does Soil Hardness Restrict Burrowers and Mollusks?",
       x = "Soil Penetration (Hardness)",
       y = "Count per Sample")
##################################################################################################
##################################################################################################
#########################  MODELING AND TESTING  #################################################
##################################################################################################
#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.

##stepwise model selection
fullModel<-lm(`% SM`~`Soil sample`*Season+`pH`+`% OM`+`EC`, data=dataAll)
nullModel<-lm(`% SM`~1, data=dataAll)
stepwiseModel<-step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="both")
summary(stepwiseModel)
################  Wet Season Models ###################### 
modWetSM<- lm(`% SM`~`Soil sample`+`% OM`+pH+EC, data=dataWet)
summary(modWetSM)
modWetOM<- lm(`% OM`~`Soil sample`+`% SM`+pH+EC, data=dataWet)
summary(modWetOM)
############## Dry Season Models ####################### 
modDrySM<- lm(`% SM`~`Soil sample`+`% OM`+pH+EC, data=dataWet)
summary(modDrySM)
modDryOM<- lm(`% OM`~`Soil sample`+`% SM`+pH+EC, data=dataWet)
summary(modDryOM)
################  Both Season Models ####################
modAll1<- lm(`% SM`~`Soil sample`*Season+`% OM`, data=dataAll)
summary(modAll1)
modAll2<- lm(`% SM`~`Soil sample`*Season+`pH`, data=dataAll)
summary(modAll2)

##### froggie based models
modFrog1<- glm(`# mollusks` ~ `Soil sample`, data=frogClean, family = "poisson")
summary(modFrog1)
modFrog2<- glm(`# burrows` ~ `Soil sample`, data=frogClean, family = "poisson")
summary(modFrog2)
modFrog3<- lm(`Soil pen` ~ `Soil sample`, data=frogClean)
summary(modFrog3)

