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
drySoil<-read.csv("DrySeasonRandomSoil_2025-1.csv", check.names = FALSE)
drySeason<-read.csv("DrySeasonRandomHab_2025-1.csv", skip=1, check.names = FALSE)
############################################################################################
drySeason$`Point #`[drySeason$`Point #` == ""] <- NA
drySeasonClean<-fill(drySeason, 'Point #', .direction = "down")
drySeasonClean<-unite(drySeasonClean, col='Soil ID', c('Point #', 'Soil sample'), sep="", remove=FALSE)
dataDry <- left_join(drySoil, drySeasonClean, by = c("Soil Smpl #" = "Soil ID"))
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
#ggparis for dry data
#ggpairs(dataDryWide[, c("pH_Veg", "pH_Point", "pH_Non-veg", "pH_Edge", "pH_Base" )],
#        title = "pH comparisons of soil samples in dry season")
#ggpairs(dataDryWide[, c("EC_Veg", "EC_Point", "EC_Non-veg", "EC_Edge", "EC_Base" )])
#ggpairs(dataDryWide[, c("% SM_Veg", "% SM_Point", "% SM_Non-veg", "% SM_Edge", "% SM_Base" )])
#ggpairs(dataDryWide[, c("% OM_Veg", "% OM_Point", "% OM_Non-veg", "% OM_Edge", "% OM_Base" )])

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
        upper = list(continuous = wrap("cor", size = 3)), 
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, size = 0.5))) +
  labs(title="Correlations between Soil Properties")
theme_minimal()
#gg boxplot
ggplot(dataDry, aes(x=`Soil sample`, y=`% SM`, fill=`Soil sample`))+
  geom_boxplot(alpha=0.65)+
  labs(title="Soil Mositure by sample type(Dryt Season)",
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
#Aggregate data
dataDryAggregated <- dataDry %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    meanSM = mean(`% SM`, na.rm = TRUE),
    meanPH = mean(pH, na.rm = TRUE),
    meanOM = mean(`% OM`, na.rm = TRUE),
    meanEC = mean(EC, na.rm = TRUE),
    meanCanopy   = mean(`Canopy cover (%)`, na.rm = TRUE),
    FernIsPresent = ifelse(any(grepl("B|E", `Soil sample`, ignore.case = TRUE)), 1, 0),
    .groups = "drop"
  )
dataDrySF <- st_as_sf(dataDryAggregated, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>% 
  st_transform(crs = 32610)

dataDrySFM<-btb_add_centroids(dataDrySF, 
                              iCellSize = 200)
#par(mfrow = c(2, 3), mar = c(1, 1, 2, 1))
########  OM SPATIAL MAP  ###########################################################
ptsDensityOM <- dataDrySFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanOM = meanOM) %>%
  drop_na(MeanOM)

ptsDensityOM$sample_density <- 1L

smoothDensityOM <- btb_smooth(pts = ptsDensityOM, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanOM <- smoothDensityOM %>% mutate(meanOM = MeanOM / sample_density)
smoothDensityMapOM <- st_transform(smoothDensityMeanOM, 4326)

ggplot(data=smoothDensityMapOM)+
  geom_sf(aes(fill=meanOM), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))


########  SM SPATIAL MAP  ###########################################################
ptsDensitySM <- dataDrySFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanSM = meanSM) %>%
  drop_na(MeanSM)

ptsDensitySM$sample_density <- 1L

smoothDensitySM <- btb_smooth(pts = ptsDensitySM, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanSM <- smoothDensitySM %>% mutate(meanSM = MeanSM / sample_density)
smoothDensityMapSM <- st_transform(smoothDensityMeanSM, 4326)

ggplot(data=smoothDensityMapSM)+
  geom_sf(aes(fill=MeanSM), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))
########  ph SPATIAL MAP  ###########################################################
ptsDensityPH <- dataDrySFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanPH = meanPH) %>%
  drop_na(MeanPH)

ptsDensityPH$sample_density <- 1L

smoothDensityPH <- btb_smooth(pts = ptsDensityPH, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanPH <- smoothDensityPH %>% mutate(meanPH = MeanPH / sample_density)
smoothDensityMapPH <- st_transform(smoothDensityMeanPH, 4326)

ggplot(data=smoothDensityMapPH)+
  geom_sf(aes(fill=MeanPH), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))
########  EC SPATIAL MAP  ###########################################################
ptsDensityEC <- dataDrySFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanEC = meanEC) %>%
  drop_na(MeanEC)

ptsDensityEC$sample_density <- 1L

smoothDensityEC <- btb_smooth(pts = ptsDensityEC, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanEC <- smoothDensityEC %>% mutate(meanEC = MeanEC / sample_density)
smoothDensityMapEC <- st_transform(smoothDensityMeanEC, 4326)

ggplot(data=smoothDensityMapEC)+
  geom_sf(aes(fill=MeanEC), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

##########Canopy Cover
ptsDensityCanopy <- dataDrySFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanCanopy = meanCanopy) %>%
  drop_na(MeanCanopy)

ptsDensityCanopy$sample_density <- 1L

smoothDensityCanopy <- btb_smooth(pts = ptsDensityCanopy, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanCanopy <- smoothDensityCanopy %>% mutate(meanCanopy = MeanCanopy / sample_density)
smoothDensityMapCanopy <- st_transform(smoothDensityMeanCanopy, 4326)

ggplot(data=smoothDensityMapCanopy)+
  geom_sf(aes(fill=meanCanopy), color=NA)+
  scale_fill_viridis_c(option = "inferno", name = "Organic Matter")+
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################################
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
############## Dry Season Models ####################### 
modDrySM<- lmer(`% SM`~`Soil sample`+`% OM`+pH+EC+`# mollusks`+(1|`Point #`), data=dataDry)
summary(modDrySM)
modDryOM<- lmer(`% OM`~`Soil sample`+`% SM`+pH+EC+`# mollusks`+(1|`Point #`), data=dataDry)
summary(modDryOM)

#simple models dry
modDrySimpleSM<-lmer(`% SM`~`Soil sample`+(1|`Point #`), data=dataDry)
summary(modDrySimpleSM)
emmeans(modDrySimpleSM, pairwise ~ `Soil sample`)

modDrySimpleOM<-lmer(`% OM`~`Soil sample`+(1|`Point #`), data=dataDry)
summary(modDrySimpleOM)
emmeans(modDrySimpleOM, pairwise ~ `Soil sample`)

modDrySimplePH<-lmer(pH~`Soil sample`+(1|`Point #`), data=dataDry)
summary(modDrySimplePH)
emmeans(modDrySimplePH, pairwise ~ `Soil sample`)

modDrySimpleEC<-lmer(EC~`Soil sample`+(1|`Point #`), data=dataDry)
summary(modDrySimpleEC)
emmeans(modDrySimpleEC, pairwise ~ `Soil sample`)
