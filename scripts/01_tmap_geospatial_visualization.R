# ============================================================
# Title: Geospatial Visualization using tmap in R
# Description: Vector, raster, and RGB visualization using
#              shapefiles, CSV point data, and satellite data
# Author: Akashnil Kaibartta
# ============================================================

# Install required packages (run once)
install.packages(c("shinyjs", "kableExtra", "colorblindcheck", "plotly"))

# Load libraries
library(sf)
library(tmap)
library(terra)
library(classInt)
library(tmaptools)
library(stars)

#Read the shapefile
states <- st_read("D:\\R_Workshop\\ICAR\\Goa_Taluka-Copy_Project.shp")

#To show the head of the shapefile
head(states)

#Plot the taluka
plot(states["Name"])

# Plotting using tmap
tm_shape(states) + 
  tm_fill(fill = "Name") + 
  tm_borders(fill_alpha = 0.4)

#Add north arrow, scalebar and remove legend
tm_shape(states) + 
  tm_fill(fill = "Name", fill.legend = tm_legend_hide()) + 
  tm_borders(fill_alpha=.4) +
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP"))

#Add latitude and longitude
tm_shape(states) + 
  tm_fill(c("Name"), fill.legend = tm_legend_hide()) + 
  tm_borders(fill_alpha=.4) +
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_graticules(lines = F)

#Change the font type
tm_shape(states) + 
  tm_fill(c("Name"), fill.legend = tm_legend_hide()) + 
  tm_borders(fill_alpha=.4) +
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_layout(text.fontfamily ="serif", frame = T) + 
  tm_graticules(lines = F)

# Add the taluka name
tm_shape(states) + 
  tm_fill(c("Name"), fill.legend = tm_legend_hide()) + 
  tm_borders(fill_alpha=.4) +
  tm_text("Name", size = 1, fontfamily = "serif") +
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_layout(text.fontfamily ="serif", frame = T) + 
  tm_graticules(lines = F)      

# Rotate the y-axis labels and increase font size and colour
m1 <- tm_shape(states) + 
  tm_fill(fill = "Name", fill.legend = tm_legend_hide(), fill.scale = tm_scale(values = "brewer.set3")) + 
  tm_borders(fill_alpha=.4) +
  tm_text("Name", size = 1, fontfamily = "serif") +
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_layout(text.fontfamily ="serif", frame = T) + 
  tm_graticules(lines = F, 
                labels.size = 1,            # increase label size
                labels.col = "black",       # change label color
                labels.rot = c(0, 90))      # rotate y-axis labels by 90 degrees
m1
# Save as one PDF
tmap_save(m1, filename = "Goa_taluka.pdf", 
          width = 8, 
          height = 10)

#Poly line plotting
#Read the line shapefile
channels <- st_read("D:\\R_Workshop\\ICAR\\Channels.shp")

#See the head 
head(channels)

#Plot the order
plot(channels["ORDER"])

tm_shape(states) +
  tm_borders(fill_alpha=.4) +
  tm_shape(channels) + 
  tm_lines(col = "ORDER", col.scale = tm_scale_continuous(values = "blue"),
           col.legend = tm_legend(title = "Legend", frame = F)) + 
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_layout(text.fontfamily ="serif", frame = T,
            legend.position = c("left", "bottom")) + 
  tm_graticules(lines = F, 
                labels.size = 1,            # increase label size
                labels.col = "black",       # change label color
                labels.rot = c(0, 90))      # rotate y-axis labels by 90 degrees

#Point data plotting
#Read the point data
df <- read.csv("D:\\R_Workshop\\ICAR\\Survey_data.csv")
table(df$Class)
head(df,2)
points <- vect(df, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")

tm_shape(states) +
  tm_borders(fill_alpha=.4) +
  tm_shape(points) + 
  tm_dots(fill = "Class", fill.legend = tm_legend(title = "Legend", frame = F), size = 0.5) + 
  tm_compass(position=c("RIGHT", "TOP"))+ 
  tm_scalebar(breaks = c(0, 5, 10), text.size = 1, position=c("RIGHT", "TOP")) +
  tm_layout(text.fontfamily ="serif", frame = T,
            legend.position = c("left", "bottom"),
            legend.title.size = 1.2,     
            legend.text.size = 1.0) + 
  tm_graticules(lines = F, 
                labels.size = 1,            # increase label size
                labels.col = "black",       # change label color
                labels.rot = c(0, 90))      # rotate y-axis labels by 90 degrees

#Raster data plotting
#Read the raster file
dem <- rast("D:\\R_Workshop\\ICAR\\Elevation.tif")

#Continuous legend
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis"))

#Add the boundary shapefile
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis")) +
  tm_shape(states) +
  tm_borders()

#Change the boundary shapefile color for better visibility
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis")) +
  tm_shape(states) +
  tm_borders(col = "red")

#Add scalebar and north arrow
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis")) +
  tm_shape(states) +
  tm_borders(col = "red") +
  tm_compass(position = c("RIGHT", "TOP"), size = 1, text.size = 1) +
  tm_scalebar(breaks = c(0, 5, 10), position = c("RIGHT", "TOP"), text.size = 1)

#To change the font type and position the legend inside
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis"),
            col.legend = tm_legend(title = "Legend", frame = F, position = c("left", "bottom"))) +
  tm_shape(states) +
  tm_borders(col = "red") +
  tm_layout(panel.labels = c("Elevation (m)"),
            text.fontfamily = "serif", 
            text.fontface = 2,
            legend.format = list(digits = 1)) +
  tm_compass(position = c("RIGHT", "TOP"), size = 1, text.size = 1) +
  tm_scalebar(breaks = c(0, 5, 10), position = c("RIGHT", "TOP"), text.size = 1)

#Qunatile legend
tm_shape(dem) +
  tm_raster(col.scale = tm_scale_intervals(
    style = "quantile",       # a method to specify the classes
    n = 5,                    # number of classes
    midpoint = NA,
    label.format = list(digits = 2),
    values = "brewer.rd_yl_gn"),  # color palette; 
    col.legend = tm_legend(title = "Legend", frame = F, position = c("left", "bottom")))+
  tm_shape(states) +
  tm_borders() +
  tm_layout(panel.labels = c("Elevation (m)"),
            text.fontfamily = "serif", 
            text.fontface = 2, #bold is 2, italics 3
            legend.text.size = 1)+
  tm_compass(position = c("RIGHT", "TOP"), size = 1, text.size = 1) +
  tm_scalebar(breaks = c(0, 5, 10), position = c("RIGHT", "TOP"), text.size = 1)

#Add latitude and longitude
m2 <- tm_shape(dem) +
  tm_raster(col.scale = tm_scale_intervals(
    style = "quantile",       # a method to specify the classes
    n = 5,                    # number of classes
    midpoint = NA,
    label.format = list(digits = 2),
    values = "-brewer.rd_yl_gn"),  # color palette and to reverse the color use -
    col.legend = tm_legend(title = "Legend", frame = F, position = c("left", "bottom")))+
  tm_shape(states) +
  tm_borders() +
  tm_layout(panel.labels = c("Elevation (m)"),
            text.fontfamily = "serif", 
            text.fontface = 2, #bold is 2, italics 3
            legend.text.size = 1)+
  tm_compass(position = c("RIGHT", "TOP"), size = 1, text.size = 1) +
  tm_scalebar(breaks = c(0, 5, 10), position = c("RIGHT", "TOP"), text.size = 1) + 
  tm_graticules(lines = F, 
                labels.size = 1,            # increase label size
                labels.col = "black",       # change label color
                labels.rot = c(0, 90))      # rotate y-axis labels by 90 degrees
m2
# Save as one PDF
tmap_save(m2, filename = "Goa_DEM.pdf", 
          width = 8, 
          height = 10)

#RGB plotting
require(stars)

file <- list.files(path="D:\\R_Workshop\\ICAR\\LC08_L1TP_146040_20231219_20240103_02_T1\\New folder", #Provide the path containing the tif files
                   pattern=".TIF$", full.names=TRUE)
l81 <- rast(file)
head(l81)
names(l81)
names(l81) <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")
#Stack the raster
l81 <- c(b1, b2, b3, b4, b5, b6, b7)
l81
head(l81)
#Plotting true color composite
plotRGB(l81, "B3", "B2", "B1", stretch="lin")

#Plotting false color composite
plotRGB(l81, "B4", "B3", "B2")

tm_shape(L7) +
  tm_rgb(col = tm_vars(c(4, 3, 2), multivariate = TRUE)) +
  tm_compass(position = c("left", "top")) +
  tm_scalebar(position = c("left", "bottom"))


cols4all::c4a_gui()
