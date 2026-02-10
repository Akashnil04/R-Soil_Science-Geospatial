# ============================================================
# Title: Soil Moisture Spatial Interpolation using Kriging and IDW
#
# Description:
# - Soil moisture point data processing
# - Spatial subsetting using boundary polygon
# - CRS transformation (WGS84 to UTM)
# - Semivariogram analysis
# - Ordinary and Universal Kriging
# - IDW interpolation
# ============================================================
library(sf)
library(terra)
library(gstat)
library(stars)
library(automap)
library(openxlsx)
library(tidyverse)

df <- read.xlsx("SM_data.xlsx")
head(df, 2)

# make spatial
class(df)
df.sp <- st_as_sf(df, coords = c("Longitude", "Latitude"), 
                  crs = "+proj=longlat +datum=WGS84")
class(df.sp)

#Load the output area shapefiles
iari_shp <- vect("D:\\R_Workshop\\ICAR\\pusa boundary.shp")
crs(iari_shp, proj = T)
plot(iari_shp)
plot(df.sp, add = T)
# Keep only points inside polygon
points_inside <- st_intersection(df.sp, st_as_sf(iari_shp))

#Plot the data
ggplot(points_inside) + 
  geom_sf(color="blue", alpha=3/4) + 
  ggtitle("Soil moisture (%)") + 
  theme_bw()

#Convert the crs of shapefile (Reprojection)
df_utm <- st_transform(points_inside, 
                       crs = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs")

#Add coordinates to the spatial data
df_utm <- cbind(df_utm, st_coordinates(df_utm))
plot(df_utm["SM"])

# Automatically compute experimental semivariogram
variogram = autofitVariogram(SM~1, df_utm)
plot(variogram)

#Convert the crs of shapefile
iari_shp_utm <- project(x = iari_shp, y = "epsg:32643")

#Rasterisation (vector to raster)
raster_template = rast(ext(iari_shp_utm), resolution = 30,
                       crs = crs(iari_shp_utm))

#Polygon rasterization
Boundary_raster = rasterize(iari_shp_utm, raster_template) |> 
  st_as_stars() #Convert to stars object
plot(Boundary_raster)

#Ordinary kriging
kr = autoKrige(SM~1, df_utm, Boundary_raster)
plot(kr)
str(kr)
plot(kr$krige_output[1])
plot(kr$krige_output[2])
kr$var_model

write_stars(kr$krige_output[1], "SM.Krigging.tif")
#Universal kriging
kriging_result = autoKrige(SM ~ X + Y, df_utm, Boundary_raster |> 
                             mutate(X = st_coordinates(Boundary_raster)$x,
                               Y = st_coordinates(Boundary_raster)$y))
plot(kriging_result)

plot(kriging_result$krige_output[1] |> 
       st_crop(st_as_sf(iari_shp_utm)))

#Using gstat package
sm.vgm <- variogram(SM~1, df_utm) # calculates sample variogram values 
plot(sm.vgm)
sm.fit <- fit.variogram(sm.vgm, model=vgm(psill = 20, 
                                          model= c("Exp", "Sph", "Mat"), 
                                          range = 400, 
                                          nugget = 40),
                        fit.method = 1) # fit model
plot(sm.vgm, sm.fit) # plot the sample values, along with the fit model
sm.fit

#Calculate the sample variance
var(df_utm$SM)

# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot1 <- df_utm |> as.data.frame() |> 
  ggplot(aes(X, Y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot2 <- Boundary_raster |> as.data.frame() |> 
  ggplot(aes(x, y)) + 
  geom_point(size=1) + 
  coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

sm.kriged <- krige(SM ~ 1, df_utm, Boundary_raster, model=sm.fit)
plot(sm.kriged)

# IDW interpolation (power = 2 is common)
idw_result <- idw(formula = SM~1, locations = df_utm, newdata = Boundary_raster, 
                  idp = 2)
plot(idw_result)

