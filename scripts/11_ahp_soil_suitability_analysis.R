# ============================================================
# Title: AHP-Based Soil Suitability Analysis using GIS
#
# Description:
# - Analytic Hierarchy Process (AHP) using pairwise comparisons
# - Criteria and sub-criteria weight estimation
# - Raster classification and reclassification
# - Weighted linear combination (WLC)
# - Final soil suitability mapping
#
# Criteria used:
# - Available Water Capacity (AWC)
# - Soil Texture
# - Organic Carbon (OC)
# - Slope
# ============================================================
#PWC <- read.csv("C:/Users/Nirmal/Desktop/R for soil science training/AHP/AHP Pairwise comparision.csv", header = T)
#PWC
#install.packages("easyAHP")
library(easyAHP)
criteria <- c("AWC", "Texture", "OC",  "Slope") #this is array, not list

mat <- matrix(c(
  1,   3,   5,   9,
  1/3, 1,   2,   5,
  1/5, 1/2, 1,   2,   
  1/9, 1/5, 1/2, 1   

), nrow=4, byrow=TRUE)

rownames(mat) <- criteria
colnames(mat) <- criteria

criteriaWeights  <- easyAHP(mat)

criteriaWeights 

####### for texture (Fine, Loam, Coarse)  #####
ClayMat <- matrix(c(
  1,   3,   7,
  1/3, 1,   3,
  1/7, 1/3, 1
), nrow=3, byrow=TRUE)

rownames(ClayMat) <- c("Fine", "Loam", "Coarse")
colnames(ClayMat) <- c("Fine", "Loam", "Coarse")
textureWeights <- easyAHP(ClayMat)
textureWeights

####### for AWC (High, Medium, Low)  #####
AWCMat <- matrix(c(
  1,   5,   9,
  1/5, 1,   3,
  1/9, 1/3, 1
), nrow=3, byrow=TRUE)

rownames(AWCMat) <- c("High", "Medium", "Low")
colnames(AWCMat) <- c("High", "Medium", "Low")
AWCWeights <- easyAHP(AWCMat)
AWCWeights



####### for OC (High, Medium, Low)  #####
OCMat <- matrix(c(
  1,   3,   7,
  1/3, 1,   5,
  1/7, 1/5, 1
), nrow=3, byrow=TRUE)

rownames(OCMat) <- c("High", "Medium", "Low")
colnames(OCMat) <- c("High", "Medium", "Low")
OCWeights <- easyAHP(OCMat)
OCWeights


####### for Slope (Steep, Moderate, Gentle, Level)  #####
SlopeMat <- matrix(c(
  1,   3,   5,   9,
  1/3, 1,   2,   5,
  1/5, 1/2, 1,   2,   
  1/9, 1/5, 1/2, 1   
), nrow=4, byrow=TRUE)

rownames(SlopeMat) <- c("Level", "Gentle", "Moderate", "Steep")
colnames(SlopeMat) <- c("Level", "Gentle", "Moderate", "Steep")
SlopeWeights <- easyAHP(SlopeMat)
SlopeWeights


##############
# Extract numeric vectors correctly
criteriaW <- criteriaWeights$Maker4$Weights
# Extract sub-criteria weights
textureW <- textureWeights$Makers$Weights
AWCW     <- AWCWeights$Makers$Weights
OCW      <- OCWeights$Makers$Weights
SlopeW   <- SlopeWeights$Makers$Weights

# Multiply criterion weight × sub-criteria weights
globalTextureWeights <- criteriaW["Texture"] * textureW
names(globalTextureWeights) <- paste("Texture", names(textureW), sep="_")

globalAWCWeights <- criteriaW["AWC"] * AWCW
names(globalAWCWeights) <- paste("AWC", names(AWCW), sep="_")

globalOCWeights <- criteriaW["OC"] * OCW
names(globalOCWeights) <- paste("OC", names(OCW), sep="_")

globalSlopeWeights <- criteriaW["Slope"] * SlopeW
names(globalSlopeWeights) <- paste("Slope", names(SlopeW), sep="_")

# Combine into one vector
globalWeights <- c(globalTextureWeights,
                   globalAWCWeights,
                   globalOCWeights,
                   globalSlopeWeights)

globalWeights

# Convert named vector to data frame
globalTable <- data.frame(
  Criterion = sub("_.*", "", names(globalWeights)),
  Category  = sub(".*_", "", names(globalWeights)),
  Weight    = round(globalWeights, 4)
)

globalTable




library(terra)

# 1. Read rasters (replace with your file paths)
awc_r <- rast("D:\\R_Workshop\\ICAR\\18th Nov\\AHP\\AWC.tif")
awc_r
clay_r <- rast("D:\\R_Workshop\\ICAR\\18th Nov\\AHP\\Clay.tif")
clay_r
oc_r <- rast("D:\\R_Workshop\\ICAR\\18th Nov\\AHP\\OC.tif")
oc_r
slope_r <- rast("D:\\R_Workshop\\ICAR\\18th Nov\\AHP\\Slope.tif")
slope_r
# 2. Classify each raster into categories
# Example thresholds (adjust based on your training criteria)

library(terra)


# Define classification rules
# Format: from, to, new_value
awc_rcl <- matrix(c(
  10, 13, 1,   # Class 1 = Low
  13, 15, 2,   # Class 2 = Medium
  15, 9999, 3  # Class 3 = High
), ncol=3, byrow=TRUE)

# Apply classification
awc_classes <- classify(awc_r, rcl=awc_rcl)

# Plot to check
plot(awc_classes, main="AWC Classes (1=Low, 2=Medium, 3=High)")

# --- Texture (Coarse, Loam, Fine)
# Suppose values are coded 1=Coarse, 2=Loam, 3=Fine

# Define classification rules
# Format: from, to, new_value
clay_rcl <- matrix(c(
  34, 45, 1,   # Class 1 = Coarse
  45, 55, 2,   # Class 2 = Loam
  55, 9999, 3  # Class 3 = Fine
), ncol=3, byrow=TRUE)

# Apply classification
clay_classes <- classify(clay_r, rcl=clay_rcl)

plot(clay_classes, main="Clay Texture Classes (1=Coarse, 2=Loam, 3=Fine)")

# --- OC (High, Medium, Low)
# Define classification rules: from, to, class_code
oc_rcl <- matrix(c(
  0.49, 0.75, 3,   # Class 3 = Low
  0.75, 1.2, 2,   # Class 2 = Medium
  1.2, 1.5, 1    # Class 1 = High
), ncol=3, byrow=TRUE)

# Apply classification
oc_classes <- classify(oc_r, rcl=oc_rcl)

# Plot to check
plot(oc_classes, main="OC Classes (1=High, 2=Medium, 3=Low)")


# --- Slope (Steep, Moderate, Gentle, Level)
# Define classification rules: from, to, class_code
slope_rcl <- matrix(c(
  -1, 3, 1,    # Class 1 = Level
  3, 5, 2,    # Class 2 = Gentle
  5, 8, 3,    # Class 3 = Moderate
  8, 9999, 4  # Class 4 = Steep
), ncol=3, byrow=TRUE)

# Apply classification
slope_classes <- classify(slope_r, rcl=slope_rcl)

# Plot to check
plot(slope_classes, main="Slope Classes (1=Level, 2=Gentle, 3=Moderate, 4=Steep)")


#################
# --- AWC weighted raster
awc_weight_rcl <- matrix(c(
  1, 1, globalWeights["AWC_Low"],
  2, 2, globalWeights["AWC_Medium"],
  3, 3, globalWeights["AWC_High"]
), ncol=3, byrow=TRUE)

awc_weighted <- classify(awc_classes, rcl=awc_weight_rcl)

# --- Texture weighted raster
clay_weight_rcl <- matrix(c(
  1, 1, globalWeights["Texture_Fine"],
  2, 2, globalWeights["Texture_Loam"],
  3, 3, globalWeights["Texture_Coarse"]
), ncol=3, byrow=TRUE)

clay_weighted <- classify(clay_classes, rcl=clay_weight_rcl)

# --- OC weighted raster
oc_weight_rcl <- matrix(c(
  1, 1, globalWeights["OC_High"],
  2, 2, globalWeights["OC_Medium"],
  3, 3, globalWeights["OC_Low"]
), ncol=3, byrow=TRUE)

oc_weighted <- classify(oc_classes, rcl=oc_weight_rcl)

# --- Slope weighted raster
slope_weight_rcl <- matrix(c(
  1, 1, globalWeights["Slope_Level"],
  2, 2, globalWeights["Slope_Gentle"],
  3, 3, globalWeights["Slope_Moderate"],
  4, 4, globalWeights["Slope_Steep"]
), ncol=3, byrow=TRUE)

slope_weighted <- classify(slope_classes, rcl=slope_weight_rcl)



# Ensure all rasters are aligned (same resolution, extent, CRS)
# If not, use resample() or project() before summing

suitability <- awc_weighted + clay_weighted + oc_weighted + slope_weighted

# Plot final suitability map
plot(suitability, main="Final Composite Suitability Map (AHP)")




writeRaster(suitability, "D:\\R_Workshop\\ICAR\\18th Nov\\AHP\\Suitability.tif", overwrite=TRUE)
