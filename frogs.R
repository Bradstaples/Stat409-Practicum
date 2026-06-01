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
library(patchwork)
##################################################################################################
frog<-read.csv("FrogTrackingHab_2026-1.csv", skip=1, check.names=FALSE)
############################################################################################
colnames(frog) <- make.unique(colnames(frog))
#change blanks in frog ID to NA
frog$`Frog ID`[frog$`Frog ID` == ""] <- NA
frog<-fill(frog, 'Frog ID', .direction = "down")
frog$`Fern Density`[frog$`Fern Density` == ""] <- NA
frog<-fill(frog, `Fern Density`, .direction = "down")
frog$`Canopy cover (%)`[frog$`Canopy cover (%)` == ""] <- NA
frog<-fill(frog, `Canopy cover (%)`, .direction = "down")
#######
frogClean <- frog |>
  mutate(FernPresence = as.factor(ifelse(`Soil sample` %in% c("B", "E"), 1, 0))) |>
  transform(`Soil sample` = ifelse(grepl("-R1", `Frog ID`), "R1",
                                   ifelse(grepl("-R2", `Frog ID`), "R2", as.character(`Soil sample`)))
  ) |>
  mutate(`Soil pen` = ifelse(`Soil pen` == "NR", "0", `Soil pen`)) |>
  mutate(
    `# mollusks` = as.numeric(`# mollusks`),
    `# burrows` = as.numeric(`# burrows`),
    `Soil pen` = as.numeric(`Soil pen`),
    `Fern Density` = as.numeric(`Fern Density`))|>
  mutate(
    `# mollusks` = case_when(
      `Soil sample` %in% c("P", "F", "R1", "R2") ~ NA_real_,
      is.na(`# mollusks`) ~ 0,
      TRUE ~ `# mollusks`
    ),
    `# burrows`= case_when(
      `Soil sample` =="F" ~ NA_real_,
      is.na(`# burrows`) ~ 0,
      TRUE ~ `# burrows`
    ),
    `Soil pen` = case_when(
      `Soil sample` %in% c("F", "R1", "R2") ~ NA_real_,
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
frogClean$`Latitude`[frogClean$`Latitude` == ""] <- NA
frogClean$`Longitude`[frogClean$`Longitude` == ""] <- NA
frogClean$`Fern Density`[frogClean$`Longitude` == ""] <- NA
frogClean<-fill(frogClean,Longitude, Latitude, `Fern Density`, .direction = "down")

frogAggregated <- frogClean %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    MeanFern = suppressWarnings(
      mean(as.numeric(`Fern Density`), na.rm = TRUE)
    ),
    MeanMollusks = suppressWarnings(
      mean(as.numeric(`# mollusks`[`Soil sample` %in% c("Edge", "Veg", "Non-veg")]), na.rm = TRUE)
    ),
    MeanBurrows = suppressWarnings(
      mean(as.numeric(`# burrows`), na.rm = TRUE)
    ),
    MeanCanopy = suppressWarnings(
      mean(as.numeric(`Canopy cover (%)`), na.rm = TRUE)
    ),
    MeanSlope = suppressWarnings(
      mean(as.numeric(`Slope   (deg)`), na.rm = TRUE)
    ),
    .groups = "drop"
  )
frogAggregated['area']= ifelse(frogAggregated$Longitude<122.82,1,ifelse(frogAggregated$Longitude>122.845,3,2)) 

frogMap<-fill(frogAggregated, Longitude, Latitude)
frogSF <- st_as_sf(frogMap, 
                   coords = c("Longitude", "Latitude"), 
                   crs = 4326) %>% 
  st_transform(crs = 32610)

frogSFM<-btb_add_centroids(frogSF, 
                           iCellSize = 200)

#par(mfrow = c(2, 2), mar = c(1, 1, 2, 1))
############################################### FRRRROOOOOG SPATIAL GRPAHING #######################
########Fern Density3############

ptsDensityFern <- frogSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanFern = MeanFern) %>%
  drop_na(MeanFern)
ptsDensityFern$sample_density <- 1L

smoothDensityFern <- btb_smooth(pts = ptsDensityFern, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanFern <- smoothDensityFern %>% mutate(meanFern = MeanFern / sample_density)
smoothDensityMapFern <- st_transform(smoothDensityMeanFern, 4326)
smoothDensityMapFern$grid_long <- st_coordinates(st_centroid(smoothDensityMapFern))[, 1]
smoothDensityMapFern$area <- ifelse(smoothDensityMapFern$grid_long < 122.82, 1, 
                                    ifelse(smoothDensityMapFern$grid_long > 122.845, 3, 2))
F1<-ggplot(data = filter(smoothDensityMapFern, area==1)) +
  geom_sf(aes(fill = meanFern), color = NA) +
  labs(
    title = "Burlington Creek",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

F2<-ggplot(data = filter(smoothDensityMapFern, area==2)) +
  geom_sf(aes(fill = meanFern), color = NA) +
  labs(
    title = "Burlington Bottoms",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

F3<-ggplot(data = filter(smoothDensityMapFern, area==3)) +
  geom_sf(aes(fill = meanFern), color = NA) +
  labs(
    title = "Migration Corridor",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

fernPlot<-F1 | F2 | F3 + plot_annotation(title = "Smoothed Fern Density Across the Study Area",
                                      caption = "Source: dataWet") &
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 10, face = "italic"),
        panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
print(fernPlot)

####### Spatial graphing of mollusk
ptsDensityMol <- frogSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanMollusks = MeanMollusks) %>%
  drop_na(MeanMollusks)

ptsDensityMol$sample_density <- 1L

smoothDensityMol <- btb_smooth(pts = ptsDensityMol, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)

smoothDensityMeanMol <- smoothDensityMol %>% mutate(meanMol = MeanMollusks / sample_density)
smoothDensityMapMol <- st_transform(smoothDensityMeanMol, 4326)

smoothDensityMapMol$grid_long <- st_coordinates(st_centroid(smoothDensityMapMol))[, 1]
smoothDensityMapMol$area <- ifelse(smoothDensityMapMol$grid_long < 122.82, 1, 
                                   ifelse(smoothDensityMapMol$grid_long > 122.845, 3, 2))

ggplot(data = filter(smoothDensityMapMol, area == 1)) +
  geom_sf(aes(fill = meanMol), color = NA) +
  labs(
    title = "Smoothed Mollusk Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapMol, area == 2)) +
  geom_sf(aes(fill = meanMol), color = NA) +
  labs(
    title = "Smoothed Mollusk Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapMol, area == 3)) +
  geom_sf(aes(fill = meanMol), color = NA) +
  labs(
    title = "Smoothed Mollusk Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

############SPATIAL graping of da burrows
ptsDensityBur <- frogSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanBurrows = MeanBurrows) %>%
  drop_na(MeanBurrows)

ptsDensityBur$sample_density <- 1L

smoothDensityBur <- btb_smooth(pts = ptsDensityBur, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)

smoothDensityMeanBur <- smoothDensityBur %>% mutate(meanBur = MeanBurrows / sample_density)
smoothDensityMapBur <- st_transform(smoothDensityMeanBur, 4326)
smoothDensityMapBur$grid_long <- st_coordinates(st_centroid(smoothDensityMapBur))[, 1]
smoothDensityMapBur$area <- ifelse(smoothDensityMapBur$grid_long < 122.82, 1, 
                                   ifelse(smoothDensityMapBur$grid_long > 122.845, 3, 2))

ggplot(data = filter(smoothDensityMapBur, area==1)) +
  geom_sf(aes(fill = meanBur), color = NA) +
  labs(
    title = "Smoothed Burrow Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapBur, area==2)) +
  geom_sf(aes(fill = meanBur), color = NA) +
  labs(
    title = "Smoothed Burrow Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapBur, area==3)) +
  geom_sf(aes(fill = meanBur), color = NA) +
  labs(
    title = "Smoothed Burrow Density",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

################ Canopy Cover Spatial Graph ################

ptsDensityCanopy <- frogSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanCanopy = MeanCanopy) %>%
  drop_na(MeanCanopy)

ptsDensityCanopy$sample_density <- 1L

smoothDensityCanopy <- btb_smooth(pts = ptsDensityCanopy, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanCanopy <- smoothDensityCanopy %>% mutate(meanCanopy = MeanCanopy / sample_density)
smoothDensityMapCanopy <- st_transform(smoothDensityMeanCanopy, 4326)
smoothDensityMapCanopy$grid_long <- st_coordinates(st_centroid(smoothDensityMapCanopy))[, 1]
smoothDensityMapCanopy$area <- ifelse(smoothDensityMapCanopy$grid_long < 122.82, 1, 
                                      ifelse(smoothDensityMapCanopy$grid_long > 122.845, 3, 2))

ggplot(data = filter(smoothDensityMapCanopy, area == 1)) +
  geom_sf(aes(fill = meanCanopy), color = NA) +
  labs(
    title = "Smoothed Canopy Cover",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapCanopy, area == 2)) +
  geom_sf(aes(fill = meanCanopy), color = NA) +
  labs(
    title = "Smoothed Canopy Cover",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapCanopy, area == 3)) +
  geom_sf(aes(fill = meanCanopy), color = NA) +
  labs(
    title = "Smoothed Canopy Cover",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))


################# Slope Spatial graph##
ptsDensitySlope <- frogSFM %>%
  st_drop_geometry() %>%
  select(x = x_centro, y = y_centro, MeanSlope = MeanSlope) %>%
  drop_na(MeanSlope)
ptsDensitySlope$sample_density <- 1L

smoothDensitySlope <- btb_smooth(pts = ptsDensitySlope, sEPSG = 32610, iBandwidth = 450, iCellSize = 10)
smoothDensityMeanSlope <- smoothDensitySlope %>% mutate(meanSlope = MeanSlope / sample_density)
smoothDensityMapSlope <- st_transform(smoothDensityMeanSlope, 4326)
smoothDensityMapSlope$grid_long <- st_coordinates(st_centroid(smoothDensityMapSlope))[, 1]
smoothDensityMapSlope$area <- ifelse(smoothDensityMapSlope$grid_long < 122.82, 1, 
                                    ifelse(smoothDensityMapSlope$grid_long > 122.845, 3, 2))

ggplot(data = filter(smoothDensityMapSlope, area == 1)) +
  geom_sf(aes(fill = meanSlope), color = NA) +
  labs(
    title = "Smoothed Slope",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapSlope, area == 2)) +
  geom_sf(aes(fill = meanSlope), color = NA) +
  labs(
    title = "Smoothed Slope",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = filter(smoothDensityMapSlope, area == 3)) +
  geom_sf(aes(fill = meanSlope), color = NA) +
  labs(
    title = "Smoothed Slope",
    caption = "Source: dataWet"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey50", linetype = "dashed"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))



#validation function for poisson models
validatePoissonModel <- function(model) {
  # 1. Check for overdispersion (Ratio > 1 means overdispersion)
  pearsonResids <- residuals(model, type = "pearson")
  residualDf <- df.residual(model)
  dispersionRatio <- sum(pearsonResids^2) / residualDf
  
  print(paste("Dispersion Ratio:", round(dispersionRatio, 3)))
  if(dispersionRatio > 1.5) {
    print("Warning: Model is highly overdispersed. Consider Negative Binomial.")
  }
  
  # 2. Check for zero-inflation (Observed zeros vs. mathematically Expected zeros)
  # Extract the actual raw counts you fed into the model
  actualData <- model.response(model.frame(model)) 
  
  observedZeros <- sum(actualData == 0)
  totalCount <- length(actualData)
  
  # Poisson math: the expected probability of getting a zero is e^(-lambda)
  expectedZeros <- sum(exp(-fitted(model)))
  
  print("--- Zero Inflation Check ---")
  print(paste("Observed Zeros:", observedZeros))
  print(paste("Expected Zeros:", round(expectedZeros, 1)))
  
  zeroRatio <- observedZeros / expectedZeros
  print(paste("Zero-Inflation Ratio (Obs/Exp):", round(zeroRatio, 3)))
  
  # 3. Check for multicollinearity
  print("--- Variance Inflation Factors ---")
  print(vif(model))
}
validatePoissonModel(modFrog1)


##################################################################################################
################ froggie based models ################

#Simple models
#mollusks
modFrogMollusk<-lmer(`# mollusks`~`Soil sample`+(1|`Frog ID`), data=frogClean)
summary(modFrogMollusk)
emmMollusks<-emmeans(modFrogMollusk, pairwise ~ `Soil sample`)
summary(emmMollusks)
plot(emmMollusks, title = "Soil Moisture by Soil Sample (Wet Season)")

#burrows
modFrogBurrow<-lmer(`# burrows`~`Soil sample`+(1|`Frog ID`), data=frogClean)
summary(modFrogBurrow)
emmMollusks<-emmeans(modFrogBurrow, pairwise ~ `Soil sample`)
summary(emmMollusks)
plot(emmMollusks, title = "Soil Moisture by Soil Sample (Wet Season)")

#soil Pen
modFrogSoil<- lmer(`Soil pen` ~ `Soil sample`+(1|`Frog ID`), data=frogClean)
summary(modFrog3)
emmSoil<-emmeans(modFrogSoil, pairwise ~ `Soil sample`)
summary(emmSoil)
plot(emmSoil, title = "Soil Penetration by Soil Sample (Wet Season)")

#fern density
modFrogFern <- lm(`Fern Density` ~ `Soil sample`, 
                  data = filter(frogClean, `Soil sample` %in% c("Frog", "Random 1", "Random 2")))
summary(modFrogFern)
emmFern <- emmeans(modFrogFern, pairwise ~ `Soil sample`)
summary(emmFern)
plot(emmFern, title = "Fern Density by Site")

#canopy cover
modFrogCanopy <- lm(`Canopy cover (%)` ~ `Soil sample`, 
                    data = filter(frogClean, `Soil sample` %in% c("Frog", "Random 1", "Random 2")))
summary(modFrogCanopy)
emmCanopy <- emmeans(modFrogCanopy, pairwise ~ `Soil sample`)
summary(emmCanopy)
plot(emmCanopy, title = "Canopy Cover by Site")

#slope
modFrogSlope <- lm(as.numeric(`Slope   (deg)`) ~ `Soil sample`, 
                   data = filter(frogClean, `Soil sample` %in% c("Frog", "Random 1", "Random 2")))
summary(modFrogSlope)
emmSlope <- emmeans(modFrogSlope, pairwise ~ `Soil sample`)
summary(emmSlope)
plot(emmSlope, title = "Slope by Site")
