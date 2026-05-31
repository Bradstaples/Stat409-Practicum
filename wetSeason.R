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
library(lme4)
library(lmerTest)
##################################################################################################
wetSoil<-read.csv("WetSeasonRandomSoil_2025-1.csv", check.names = FALSE)
wetSeason<-read.csv("WetSeasonRandomHab_2025-1.csv",skip=1, check.names = FALSE)
############################################################################################
wetSeasonClean<-fill(wetSeason, 'Point #', .direction = "down")
wetSeasonClean<-unite(wetSeasonClean, col='Soil ID', c('Point #', 'Soil sample'), sep="", remove=FALSE)
dataWet <- left_join(wetSoil, wetSeasonClean, by = c("Soil Smpl #" = "Soil ID"))
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
#ggpairs plot for wet data
ggpairs(dataWetWide[,c("% SM_Veg", "% SM_Point", "% SM_Non-veg", "% SM_Edge", "% SM_Base")])
ggpairs(dataWetWide[, c("pH_Veg", "pH_Point", "pH_Non-veg", "pH_Edge", "pH_Base" )], 
        title = "pH comparisons of soil samples in wet season")
ggpairs(dataWetWide[, c("EC_Veg", "EC_Point", "EC_Non-veg", "EC_Edge", "EC_Base" )])
ggpairs(dataWetWide[, c("% OM_Veg", "% OM_Point", "% OM_Non-veg", "% OM_Edge", "% OM_Base" )])


##################################################################################################
##########################   WET SEASON GRAPHS               ####################################
#ggplot comparing soil properties between soil samples in wet season
ggplot(dataWetLong, aes(x=`Soil sample`, y=Value, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  facet_wrap(~Soil_Property, scales = "free_y")+theme_minimal()+
  labs(title="Soil Properties by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="Value")
#soil property trend graphs
ggpairs(dataWet, columns = c("% SM", "pH", "EC", "% OM", "# mollusks"),
        mapping = aes(color = `Soil sample`, alpha = 0.5), upper = list(continuous = wrap("cor", size = 3)), 
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, size = 0.5))) +
  theme_minimal()+labs(title="Correlation Between Soil Properties in the Wet Season")
#gg boxplot
ggplot(dataWet, aes(x=`Soil sample`, y=`% SM`, fill=`Soil sample`))+geom_boxplot(alpha=0.65)+
  labs(title="Soil Mositure by sample type(Wet Season)",
       x= "Soil Sample Type",
       y="% Soil Mositure")
#ggplot of everything vs soil moisture
dataWet|>
  select(`Point #`, `Soil sample`, `% SM`, pH, EC, `% OM`, `# mollusks`) |>
  pivot_longer(cols=c(pH, EC, `% OM`, `# mollusks`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% SM`, y= Values, color=`Soil sample`))+geom_point()+geom_smooth(method=lm, se=F)+
  facet_wrap(~Variable, scales="free_y", ncol=1)+
  labs(title="How Soil Moisture Affects Other Soil Properties in the Wet Season",
       x="% Soil Moisture",
       y="Value of Other Soil Properties")
#everything vs OM
dataWet|>
  select(`Point #`, `Soil sample`, `% OM`, pH, EC, `% SM`) |>
  pivot_longer(cols=c(pH, EC, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `% OM`, y= Values, color=`Soil sample`))+geom_point()+geom_smooth(method="lm", se=F)+
  facet_wrap(~Variable, scales="free_y", ncol=1)+
  labs(title="How Organic Matter Affects Other Soil Properties in the Wet Season",
       x="% Organic Matter",
       y="Value of Other Soil Properties")
#everything vs EC
dataWet|>
  select(`Point #`, `Soil sample`, `EC`, pH, `% OM`, `% SM`) |>
  pivot_longer(cols=c(pH, `% OM`, `% SM`), names_to = "Variable", values_to = "Values") |>
  ggplot(aes(x= `EC`, y= Values, color=`Soil sample`))+geom_point()+geom_smooth(method="lm", se=F)+
  facet_wrap(~Variable, scales="free_y", ncol=1)
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
#multi plot initilization for maps
par(mfrow = c(2, 4), mar = c(1, 1, 2, 1))
dataWetAggregated <- dataWet %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    meanFern = mean(`Fern Density`, na.rm = TRUE),
    meanSM = mean(`% SM`, na.rm = TRUE),
    meanPH = mean(pH, na.rm = TRUE),
    meanOM = mean(`% OM`, na.rm = TRUE),
    meanEC = mean(EC, na.rm = TRUE),
    meanCanopy   = mean(`Canopy cover (%)`, na.rm = TRUE),
    meanSlope    = mean(`Slope (deg)`, na.rm = TRUE),
    meanMollusks = mean(`# mollusks`, na.rm = TRUE),
    .groups = "drop"
  ) 

dataWetSF <- st_as_sf(dataWetAggregated, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>% 
  st_transform(crs = 32610)
#plot(dataWetSF$geometry)

dataWetSFM <- btb_add_centroids(dataWetSF, 
                                iCellSize = 200)
################################################################################Fern Density Map
ptsDensityFern <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanFern = meanFern)%>%
  drop_na(MeanFern)

ptsDensityFern$sample_density <- 1L

smoothDensityFern <- btb_smooth(pts = ptsDensityFern,sEPSG = 32610,
                                iBandwidth = 500,iCellSize = 10)

smoothDensityFernMean <- smoothDensityFern %>% mutate(meanFern=MeanFern/sample_density)
smoothDensityFernWGS <- st_transform(smoothDensityFernMean, 4326)

ggplot(data=smoothDensityFernWGS)+
  geom_sf(aes(fill=meanFern), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Fern Density")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

############## ORganic Matter ############## ############## 

ptsDensityOM <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanOM = meanOM) %>%
  drop_na(MeanOM)

ptsDensityOM$sample_density <- 1L

smoothDensityOM <- btb_smooth(pts = ptsDensityOM,sEPSG = 32610,
                              iBandwidth = 450,iCellSize = 10)

smoothDensityMeanOM <- smoothDensityOM %>% mutate(meanOM=MeanOM/sample_density)
smoothDensityWGSOM <- st_transform(smoothDensityMeanOM, 4326)

ggplot(data=smoothDensityWGSOM)+
  geom_sf(aes(fill=meanOM), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))
############## ############## Soil Moisture map ############## ##############
ptsDensitySM <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanSM = meanSM) %>%
  drop_na(MeanSM)

ptsDensitySM$sample_density <- 1L

smoothDensitySM <- btb_smooth(pts = ptsDensitySM,sEPSG = 32610,
                              iBandwidth = 450,iCellSize = 10)

smoothDensityMeanSM <- smoothDensitySM %>% mutate(meanSM=MeanSM/sample_density)
smoothDensityWGSSM <- st_transform(smoothDensityMeanSM, 4326)

ggplot(data=smoothDensityWGSSM)+
  geom_sf(aes(fill=MeanSM), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Soil Moisture")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

############## ############## ph map ############## ##############
ptsDensityPH <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanPH = meanPH) %>%
  drop_na(MeanPH)

ptsDensityPH$sample_density <- 1L

smoothDensityPH <- btb_smooth(pts = ptsDensityPH,sEPSG = 32610,
                              iBandwidth = 450,iCellSize = 10)

smoothDensityMeanPH <- smoothDensityPH %>% mutate(meanPH=MeanPH/sample_density)
smoothDensityWGSPH <- st_transform(smoothDensityMeanPH, 4326)

ggplot(data=smoothDensityWGSPH)+
  geom_sf(aes(fill=meanPH), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "pH Levels")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))
############## ############## EC map ############## ##############
ptsDensityEC <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanEC = meanEC) %>%
  drop_na(MeanEC)

ptsDensityEC$sample_density <- 1L

smoothDensityEC <- btb_smooth(pts = ptsDensityEC,sEPSG = 32610,
                              iBandwidth = 450,iCellSize = 10)

smoothDensityMeanEC <- smoothDensityEC %>% mutate(meanEC=MeanEC/sample_density)
smoothDensityWGSEC <- st_transform(smoothDensityMeanEC, 4326)

ggplot(data=smoothDensityWGSEC)+
  geom_sf(aes(fill=meanEC), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Electric Conductivity")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))
############## ############## mollusks map ############## ##############
ptsDensityMol <- dataWetSFM%>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanMollusks = meanMollusks) %>%
  drop_na(MeanMollusks)

ptsDensityMol$sample_density <- 1L

smoothDensityMol <- btb_smooth(pts = ptsDensityMol,sEPSG = 32610,
                               iBandwidth = 450,iCellSize = 10)

smoothDensityMeanMol <- smoothDensityMol %>% mutate(meanMol=MeanMollusks/sample_density)
smoothDensityWGSMol <- st_transform(smoothDensityMeanMol, 4326)

mf_map(x = smoothDensityWGSMol,type = "choro",var="meanMol",breaks = "quantile",
       nbreaks = 5,border = NA,leg_val_rnd = 1,leg_horiz=TRUE)

mf_graticule(x = smoothDensityWGSMol, add = TRUE, col = "grey0", lty = 2,pos = c("bottom", "left"))

mf_layout(title = "Smoothed Mollusks Density", 
          credits = "Source: dataWet",
          arrow = FALSE)
###################### Canopy COver
ptsDensityCanopy <- dataWetSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, meanCanopy = meanCanopy) %>%
  drop_na(meanCanopy)

ptsDensityCanopy$sample_density <- 1L

smoothDensityCanopy <- btb_smooth(pts = ptsDensityCanopy, sEPSG = 32610, 
                                  iBandwidth = 450, iCellSize = 10)

smoothDensityMeanCanopy <- smoothDensityCanopy %>% mutate(meanCanopy = meanCanopy / sample_density)
smoothDensityWGSCanopy <- st_transform(smoothDensityMeanCanopy, 4326)

mf_map(x = smoothDensityWGSCanopy, type = "choro", var="meanCanopy", breaks = "quantile",
       nbreaks = 5, border = NA, leg_val_rnd = 1, leg_horiz=TRUE)

mf_graticule(x = smoothDensityWGSCanopy, add = TRUE, col = "grey0", lty = 2, pos = c("bottom", "left"))

mf_layout(title = "Smoothed Canopy Cover Density", 
          credits = "Source: dataWet", arrow = FALSE)
######### Slope 
ptsDensitySlope <- dataWetSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, meanSlope = meanSlope) %>%
  drop_na(meanSlope)

ptsDensitySlope$sample_density <- 1L

smoothDensitySlope <- btb_smooth(pts = ptsDensitySlope, sEPSG = 32610, 
                                 iBandwidth = 450, iCellSize = 10)

smoothDensityMeanSlope <- smoothDensitySlope %>% mutate(meanSlope = meanSlope / sample_density)
smoothDensityWGSSlope <- st_transform(smoothDensityMeanSlope, 4326)

ggplot(data=smoothDensityWGSSlope)+
  geom_sf(aes(fill=meanSlope), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Slope")+
  labs(
    title = "Smoothed Slope Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

mf_map(x = smoothDensityWGSSlope, type = "choro", var="meanSlope", breaks = "quantile",
       nbreaks = 5, border = NA, leg_val_rnd = 1, leg_horiz=TRUE)

mf_graticule(x = smoothDensityWGSSlope, add = TRUE, col = "grey0", lty = 2, pos = c("bottom", "left"))

mf_layout(title = "Smoothed Slope Density", 
          credits = "Source: dataWet", arrow = FALSE)

#####
par(mfrow = c(1, 1))


##################################################################################################
#########################  MODELING AND TESTING  #################################################
##################################################################################################
#does the presence of ferns affect microhabitat characteristics such as soil char, number of molllusk
#and number of burrows, as well as soil parameters.
####### Validation testing function #######
validateModelLM <- function(model) {
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

validateModelLMER<- function(model) {
  # Check for multicollinearity
  print("Variance Inflation Factors:")
  print(vif(model))
  
  #confidence intervals
  print("Confidence Intervals:")
  print(confint(model))
  
  # Check for homoscedasticity
  fitted_values <- fitted(model)
  residuals <- resid(model)
  
  plot(fitted_values, residuals, main = "Fitted vs Residuals", xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, col = "red")
  
  # QQ plot of residuals
  qqnorm(residuals, main = "QQ Plot of Residuals")
  qqline(residuals, col = "red")
  
  #Variance test
  group <- fitted_values > median(fitted_values)
  print(var.test(residuals[group], residuals[!group]))
}

##################################################################################################
################  Wet Season Models w/soil sample ###################### 

modWetSM<- lmer(`% SM`~`Soil sample`+`% OM`+pH+EC+`Fern Density`+(1|`Point #`), data=dataWet)
summary(modWetSM)
modWetOM<- lmer(`% OM`~`Soil sample`+`% SM`+pH+EC+`Fern Density`+(1|`Point #`), data=dataWet)
summary(modWetOM)
modWetPH<- lmer(pH~`Soil sample`+`% SM`+`% OM`+EC+(1|`Point #`), data=dataWet)
summary(modWetPH)
modWetFern<- lm(`Fern Density`~`Soil sample`+`% SM`+`% OM`+pH+EC, data=dataWet)
summary(modWetFern)

#Validation testing
validateModelLMER(modWetSM)
validateModelLMER(modWetOM)
validateModelLMER(modWetPH)
validateModelLM(modWetFern)

#confint plots
plot_model(modWetSM,show.values = TRUE, value.offset = .3, title = "Predictors of Soil Moisture (Wet Season)")
plot_model(modWetOM, show.values = TRUE, value.offset = .3, title = "Predictors of Organic Matter (Wet Season)")

#pairwise comparisons of soil sample types with emmeans
emmeans(modWetSM, pairwise ~ `Soil sample`)
emmeans(modWetOM, pairwise ~ `Soil sample`)
emmeans(modWetPH, pairwise ~ `Soil sample`)
#simple models wet
modWetSimpleSM<-lmer(`% SM`~`Soil sample`+(1|`Point #`), data=dataWet)
summary(modWetSimpleSM)
meansWetOM<-emmeans(modWetSimpleSM, pairwise ~ `Soil sample`)
summary(meansWetOM)
plot(meansWetOM, title = "Soil Moisture by Soil Sample (Wet Season)")+
  geom_vline(xintercept = 4.05, color = "red", linetype = "dashed", size = 1)

modWetSimpleOM<-lmer(`% OM`~`Soil sample`+(1|`Point #`), data=dataWet)
summary(modWetSimpleOM)
emmeans(modWetSimpleOM, pairwise ~ `Soil sample`)

modWetSimpleFern<-lm(`Fern Density`~`Soil sample`, data=dataWet)
summary(modWetSimpleFern)
emmeans(modWetSimpleFern, pairwise ~ `Soil sample`)

modWetSimplePH<-lmer(pH~`Soil sample`+(1|`Point #`), data=dataWet)
summary(modWetSimplePH)
emmeans(modWetSimplePH, pairwise ~ `Soil sample`)

modWetSimpleEC<-lmer(EC~`Soil sample`+(1|`Point #`), data=dataWet)
summary(modWetSimpleEC)
emmeans(modWetSimpleEC, pairwise ~ `Soil sample`)

##################################################################################################
################## Wet Season models w/ fern presence #####################
modWetSMFern<- lmer(`% SM`~FernPresence+`% OM`+pH+EC+(1|`Point #`), data=dataWet)
summary(modWetSMFern)
modWetOMFern<- lmer(`% OM`~FernPresence+`% SM`+pH+EC+(1|`Point #`), data=dataWet)
summary(modWetOMFern)
modWetFernFern<- lm(`Fern Density`~`% SM`+`% OM`+pH+EC, data=dataWet)
summary(modWetFernFern)

#validation testing
validateModelLMER(modWetSMFern)
validateModelLMER(modWetOMFern)
validateModelLM(modWetFernFern)

#confint plots
plot_model(modWetSMFern,show.values = TRUE, value.offset = .3, title = "Predictors of Soil Moisture (Wet Season, Fern Presence)")
plot_model(modWetOMFern, show.values = TRUE, value.offset = .3, title = "Predictors of Organic Matter (Wet Season, Fern Presence)")
plot_model(modWetFernFern, show.values = TRUE, value.offset = .3, title = "Predictors of Fern Density (Wet Season, Fern Presence)")

