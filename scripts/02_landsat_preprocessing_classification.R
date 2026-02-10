# ============================================================
# Title: Landsat Image Processing & Classification in R
# Description: Radiometric correction, cropping, masking,
#              unsupervised and supervised classification
# ============================================================
# For installing one package
install.packages("RStoolbox")
# For installing more than one package
install.packages(c("terra", "sf", "stars"))

library(sf) 
library(terra)
library(stars)
library(RStoolbox)
library(colorRamps)

# Import meta-data and bands based on MTL file (For Landsat 9, it does not work)
mtlFile <- "D:\\R_Workshop\\ICAR\\LC08_L1TP_146040_20231219_20240103_02_T1\\LC08_L1TP_146040_20231219_20240103_02_T1_MTL.txt"
metaData <- readMeta(mtlFile)
lsat <- stackMeta(mtlFile)
?stackMeta

# Correct DN to at-surface-reflectance with DOS (Chavez decay model)
lsat_sref <- radCor(lsat, metaData = metaData, method = "costz")
names(lsat_sref)
crs(lsat_sref, proj = T)

#ro check single band
plot(lsat_sref[[7]])

# to visualize multiple bands
plot(lsat_sref)

# Load the output area shapefiles
iari_shp <- vect("D:\\R_Workshop\\ICAR\\pusa boundary.shp")
crs(iari_shp, proj = T)
plot(iari_shp)

#Convert the crs of shapefile
iari_shp_utm <- project(iari_shp, crs(lsat))

# Plotting the shapefile on raster
plot(lsat_sref[[1]])
plot(iari_shp_utm, add = T, col = "red")

# Cropping the raster to the shapefile spatial extent 
lsat_sref.crop <- crop(lsat_sref, iari_shp_utm)
plot(lsat_sref.crop)

# Putting NA values in all the raster cells outside the shapefile boundaries 
lsat_sref.masked <- mask(x=lsat_sref.crop, mask=iari_shp_utm)
plot(lsat_sref.masked)
names(lsat_sref.masked)

# Plotting NCC
plotRGB(lsat_sref.masked, r = "B4_sre", g = "B3_sre", b = "B2_sre", 
        stretch = "hist")

# Plotting FCC
plotRGB(lsat_sref.masked, r = "B5_sre", g = "B4_sre", b = "B3_sre", 
        stretch = "hist")

# Create a shapefile from collected coordinates or GCPs
gcps <- read.csv("D:\\R_Workshop\\ICAR\\Test_data.csv")
head(gcps,2)
class(gcps)
str(gcps)

# data frame to vector conversion
gcps <- vect(gcps, geom=c("Longitude", "Latitude"), 
             crs="+proj=longlat +datum=WGS84")
class(gcps)
str(gcps)
crs(gcps, proj = T)

# Writing shapefile
writeVector(gcps, "D:\\R_Workshop\\ICAR\\test.shp", overwrite = T)
plot(gcps, add = T)

# Convert the crs of shapefile (Reprojection)
gcp_shp_utm <- project(gcps, crs(lsat_sref.masked))

plot(gcp_shp_utm, add = T, col = "yellow", pch = 19)

# K-means clustering
kmeans_raster <- unsuperClass(lsat_sref.masked, nSamples = 10000, nClasses = 9, 
                              nStarts = 25, nIter = 100, norm = TRUE, clusterMap = TRUE,
                              algorithm = "Hartigan-Wong")

plot(kmeans_raster$map)

# Maximum Likelihood Classification
set.seed(10)
img_mlc = superClass(lsat_sref.masked, trainData = st_as_sf(gcp_shp_utm), 
                     responseCol = "CLASS", nSamples = 100, model ="mlc", 
                     mode = "classification", trainPartition = 0.7, kfold = 5, 
                     verbose = TRUE)

plot(img_mlc$map)

#Write raster data using terra package
writeRaster(img_mlc$map, filename = "MLC.tif", overwrite=TRUE)

# RF
set.seed(10)
img_rf = superClass(lsat_sref.masked, trainData = sf::st_as_sf(gcp_shp_utm), 
                     responseCol = "CLASS", nSamples = 30, model ="rf", 
                     mode = "classification", trainPartition = 0.7, kfold = 5, 
                     verbose = TRUE)

plot(img_rf$map)

#Raster to vector conversion
rf_map_polygon <- as.polygons(img_rf$map)
plot(rf_map_polygon, col = )

#Vector data analysis
#Read the csv file containing GPS locations
df <- read.csv("D:\\R_Workshop\\ICAR\\Elevation.csv")
head(df, 2)

# make spatial
df_sp <- vect(df, geom = c("Longitude", "Latitude"), 
                  crs="+proj=longlat +datum=WGS84")

# Plot it
plot(df_sp)

# Convert the crs of shapefile (Reprojection)
df_utm <- project(x = df_sp, y = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs")

#Read the boundary shapefile
goa <- vect("D:\\R_Workshop\\ICAR\\Goa_Taluka-Copy_Project.shp")
head(goa, 2)
goa$Name

#Plot the shapefile
plot(goa)

#Plot the talukas
plot(goa, "Name", main = "Goa")

#Add north arrow
north("topright")

#Spatial subsetting
subset_goa <- goa[goa$District == "North Goa", ]

# Plot to check
plot(goa, col = "lightgray", main = "Subset: North Goa")
plot(subset_goa, add = TRUE, col = "lightblue", border = "darkblue", lwd = 2)

#Add north arrow
north("topright")

#Select the points within North Goa
northgoa_height = df_sp[subset_goa, ]

plot(northgoa_height, add = T)

#Dissolve the inside polygones to have Goa only
goa_dissolved <- aggregate(goa)

plot(goa_dissolved)

#Project the Goa shapefile to UTM zone 43 N
goa_utm <- project(x = goa_dissolved, y = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs")

#Buffer of 5 km
goa_buffer <- buffer(goa_utm, width=5000)

plot(goa_buffer)
plot(goa_utm, add = T)

#Inside buffer of 5 km
goa_buffer <- buffer(goa_utm, width=-5000)

plot(goa_utm)
plot(goa_buffer, add = T)



# Rasterisation
raster_template = rast(ext(goa_utm), resolution = 10,
                          crs = crs(goa_utm))

# Polygon rasterization
Boundary_raster = rasterize(goa_utm, raster_template) 
plot(Boundary_raster)

#Read elevation raster
dem <- rast("D:\\R_Workshop\\ICAR\\Elevation.tif")
plot(dem)

#To see the coordinate reference system
crs(dem, proj = T)

#Project the Goa shapefile to UTM zone 43 N
goa_taluka_utm <- project(x = goa, y = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs")
plot(goa_taluka_utm, add = T)

#Extract taluka-wise mean elevation
goa_elevation <- extract(dem, goa_taluka_utm, fun = "mean", na.rm = T, bind = T)

# Suppose the numeric column is "Elevation"
plot(goa_elevation, "Elevation", 
     main = "Elevation (m)")

#Add north arrow
north("topright")

