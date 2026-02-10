# ============================================================
# Title: Soil Profile and Soil Texture Analysis in R
# Description: Soil profile visualization using aqp and
#              soil texture classification using USDA system
# Author: Akashnil Kaibartta

# ============================================================
library(aqp)
library(openxlsx)

df <- read.xlsx("D:\\R_Workshop\\ICAR\\18th Nov\\aqp_data.xlsx", sheet = "Sheet1_F")
head(df, 2)
str(df)

## convert Munsell notation into R colors (sRGB)
df$soil_color <- with(df, munsell2rgb(Hue, Value, Chroma))

## init SoilProfileCollection for granite transect
depths(df) <- ID ~ Top + Bottom

# Open PNG device with 300 dpi
png("Profile.png", width = 6, height = 4, units = "in", res = 300)

par(mar=c(0, 0, 2, 2))#bottom, left, top, and right
plotSPC(df, width = 0.3)

# Close the device to save the file
dev.off()

# Open PNG device with 300 dpi
png("Profile_clay.png", width = 6, height = 4, units = "in", res = 300)

par(mar=c(0, 0, 3, 2))#bottom, left, top, and right
plotSPC(df, width = 0.3, color = 'Clay')

# Close the device to save the file
dev.off()

# fancy plot
par(mar = c(0, 0, 0, 0))
plotSPC(df, name.style = 'center-center', depth.axis = F, hz.depths = TRUE, 
        hz.depths.offset = 0.05, cex.names = 0.66, width = 0.3, shrink = TRUE)

install.packages( pkgs = "soiltexture" )
library( soiltexture )
library(plotrix)

soiltexture::soiltexture_gui()

data <- read.csv("D:\\traning\\Soil_texture.csv")

TT.plot(
  class.sys = "USDA.TT",      # mention the textural class system
  tri.data = data,          # mention data
  main = "Soil texture data"  # Title of the graph
)



