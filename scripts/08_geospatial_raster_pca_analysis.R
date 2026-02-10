# ============================================================
# Title: Geospatial Data Processing, Raster Analysis and PCA
#
# Description:
# - CSV to spatial points (sf, terra)
# - CRS transformation (WGS84 to UTM)
# - Vector and raster handling
# - Raster stacking and RGB visualization
# - Raster to table conversion
# - Principal Component Analysis (PCA) on raster data
# - Export PCA components as raster layers
# ============================================================
setwd("D:\\R_Workshop\\ICAR\\R for soil science training class2")
# read CSV
data <- read.csv("csv data class2.csv")
# Inspect structure
head(data)
anyNA(data)
str(data)
# Convert intiger column to numeric (float)
data$N <- as.numeric(data$N)
data$K <- as.numeric(data$K)
str(data)
#Remove NA
data <- data[complete.cases(data),]
anyNA(data)
library(sf)
#Convert to spatial dataframe
data_sf <- st_as_sf(
  data,
  coords = c("Longitude", "Latitude"),   # specify coordinate columns
  crs = 4326                  # WGS84 coordinate system
)

# Inspect structure of sf object
str(data_sf)

st_write(data_sf, "points.shp")


# Read shapefile back
shp <- st_read("points.shp")
plot(shp)
# Inspect
class(shp)
st_geometry_type(shp)

plot(shp["N"]) 


st_crs(shp)   # check current CRS


# convert to UTM Zone 43N (EPSG:32643)
shp_utm43 <- st_transform(shp, crs = 32643)

# 32 represents UTM projection, 6 represents North hemisphere (7 for south), 43 represents zone 43
plot(shp_utm43["N"]) 

st_write(shp_utm43, "pointsUTM3.shp")



library(terra)

# Convert to SpatVector (spatial dataframe)
data_vect <- vect(data,                                #your table
                  geom = c("Longitude", "Latitude"),   #your columns for latitude and longitude
                  crs = "EPSG:4326")                   #your projection in which you have noted in the filed

# Write shapefile
writeVector(data_vect, "terrapoints.shp")

# Read shapefile back
shp <- vect("terrapoints.shp")

# Convert to UTM Zone 43N (EPSG:32643)
shp_utm43 <- project(shp, "EPSG:32643")
head(shp_utm43)
str(shp_utm43)
# Write the reprojected shapefile
writeVector(shp_utm43, "terrapointsUTM3.shp", overwrite=TRUE)




library(sf)
# Read shapefile
poly_sf <- st_read("Nagpurblock.shp")

# Inspect
class(poly_sf)

# quick visualization
plot(poly_sf)   


library(terra)
# Read shapefile
poly_vect <- vect("Nagpurblock.shp")
# Inspect
class(poly_vect)
# quick visualization
plot(poly_vect)  


#####Raster#######
library(terra)
#### ** read raster** ####
B2 <- rast("Raster/B2.TIF")
B3 <- rast("Raster/B3.TIF")
B4 <- rast("Raster/B4.TIF")
B5 <- rast("Raster/B5.TIF")
B6 <- rast("Raster/B6.TIF")
B7 <- rast("Raster/B7.TIF")

plot(B2)

#Create a stack
stack <- c(B2, B3, B4, B5, B6, B7)
plotRGB(stack)                    #will throw an error
plotRGB(stack, stretch = "lin")
plotRGB(stack, r=4, g=3, b=2, stretch = "lin")
plotRGB(stack, r=4, g=3, b=2, stretch = "hist")

str(stack)
# Convert to data frame
stack_df <- as.data.frame(stack, xy = TRUE)

# Inspect
head(stack_df)


write.table(stack_df, "raster_text.txt")


text <- read.table("raster_text.txt", header = T)
head(text)

r <- rast(text[, c("x", "y", "B2")], crs="EPSG:32643")
plot(r)
crs(r)

r1 <- rast(text[, c("x", "y", "B5", "B4", "B3")], crs="EPSG:32643")
plot(r1)
plotRGB(r1, stretch="lin")
writeRaster(r1, "Stack2.tif")







##################### PCA ###############
#emember, there are packages for PCA of raster
data<- read.table("raster_text.txt", header = T)
head(data)
names(data)
data1 <- data[complete.cases(data),]
data2 <- data1[, c(3:8)] #removed bio014 as it has only one value
data3<- scale(data2)
library(ggplot2)
library(factoextra)
library(corrplot)
library(Hmisc)
install.packages("Hmisc")
cor_test <- rcorr(as.matrix(data2))
cor_df <- cor_test$r
p_values <- cor_test$P


# adds significant labels as **
corrplot(cor_test$r, type = "upper", order = "hclust", p.mat = cor_test$P, 
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white")

# Perform PCA
pca_result <- prcomp(data3, center = TRUE)
# Print summary of PCA
summary(pca_result)
# Visualize the PCA results
# Scree plot
fviz_eig(pca_result, ncp = 20)
# Extract eigenvalues
eigenvalues <- pca_result$sdev^2
print(eigenvalues)
# Extract loadings
loadings <- pca_result$rotation
print(loadings)
# Visualize the loadings
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)




#save PCs as raster
coor <- data1[ , 1:2]
export <- cbind(coor, pca_result[["x"]])
head(export)

PC1raster <- rast(export[, c("x", "y", "PC1")], crs="EPSG:32643")
plot(PC1raster)
writeRaster(PC1raster, "PC1.tif")

