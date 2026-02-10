# ============================================================
# Title: Soil Texture, Soil Profile, Soil Colour and
#        Water Quality Visualization in R
#
# Description:
# - USDA & HYPRES soil texture triangle
# - Soil profile visualization using aqp
# - Soil colour analysis using Munsell system
# - Horizon transition graphs
# - Maucha and Piper diagrams for water quality
# ============================================================
install.packages("soiltexture")
library(soiltexture)

## plot an empty triangle
TT.plot(class.sys = "none")
## plot an USDA triangle
TT.plot( class.sys = "USDA.TT" )
#The labels in the above plot are not similar to the ones in use. 
#To get the version in use, type
TT.plot( class.sys = "USDA-NCSS.TT" )

TT.plot(class.sys = "HYPRES.TT")



TT.classes.tbl(class.sys = "USDA.TT")

mydata <- read.csv("D:\\R_Workshop\\ICAR\\Soil Texture.csv", header=T)
TT.plot(
  class.sys = "USDA.TT",
  tri.data = mydata,		# the soil particle size data
  main = "Soil texture data"
)



# Create some custom labels:
labelz <-mydata[,1]
# Display the text
labelz


#plot the triangle with points and sample number
geo <- TT.plot(
  class.sys = "USDA.TT",
  tri.data = mydata,
  main = "Soil texture data"
)
TT.text(
  tri.data = mydata,
  geo = geo,
  labels = labelz,
  font = 0,
  col = "blue"		#color of the labels
)



# Display an empty USDA texture triangle:
geo <- TT.plot(class.sys="USDA.TT")
# Create a label file with the sample number ("ID" in the data):
labelz <-mydata[,1]
# Display the text
labelz
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13
#plot the triangle with sample numbers only

TT.text(
  tri.data = mydata,
  geo = geo,
  labels = labelz,
  font = 1,
  col = "blue"
)




#Get a vector of textural class of the same length as the number of soil samples 
Tex.class<-TT.points.in.classes(
  tri.data = mydata,
  class.sys = "USDA.TT",
  PiC.type = "t"
)
Tex.class
 

#bind the data
x <- cbind(mydata, Tex.class)

##Write as csv file

write.csv(x, file = "D:\\R_Workshop\\ICARsoiltextureclassdata.csv", row.names=FALSE)



















# load sample data set, a data.frame object with horizon-level data from 5 profiles
install.packages("aqp")
library(aqp)
data<-read.csv("Soil Profile.csv", header = TRUE)

data
depths(data) <- id ~ top + bottom
class(data) # class name
str(data)
horizonNames(data) # column names from horizon data
names(data) 
siteNames(data) # field representing the profiles

length(data) # number of profiles in the collection

nrow(data) # number of horizons in the collection

min(data) # shallowest profile depth in collection

max(data) # deepest profile depth in collection


#plot soil profile collection plot
par(mar = c(0,2,0,4), xpd = NA) # set margins and turn off clipping

plotSPC(data)

par(mar=c(0,0,3,0)) # tighter figure margins

plotSPC(data, name='name', 
        color='name',			# define the theme
        col.palette=RColorBrewer::brewer.pal(9, 'Set1'), # define color
        col.label='Original Horizon Name',		# Title of plot
        cex.names = 1, 				# font size of labels
        width = 0.2				# profile plot width
)

# generalize horizon names into 5 groups
data$genhz <- generalize.hz(data$name, new = c('A','Bw', 'Bss', 'BC','C'),
                            pat=c('A[0-9]?', '^Bw', '^Bss', 'BC', '^C'))

plotSPC(data, name='name', color='genhz', col.palette=RColorBrewer::brewer.pal(3, 'Spectral'), col.label='Generalized Horizon Name')


#We can also generate profile plots based on the textural class, a categorical variable.
par(mar=c(0,0,3,0)) # tighter figure margins
plotSPC(data, name='name', cex.names = 1,color='Tex.class', 
        col.label='Textural Classes'
)


#Clay Content

plotSPC(data, name='name', cex.names = 1, color='Clay', 
        col.label='Clay Content (%)'
)
#change color
par(mar=c(0,0,3,0)) # tighter figure margins
plotSPC(data, name='name', color='Clay', col.palette=viridis::viridis(10), col.label='Clay Content (%)')


# tighter figure margins
par(mar=c(0,0,3,0)) 
plotSPC(data, name='name', color='Clay')
# symbolize volume fraction data
addVolumeFraction(data, colname = 'BD')



par(mar=c(0,0,3,0)) 
plotSPC(data, name='name', color='Clay')
# symbolize volume fraction data
addVolumeFraction(data, colname = 'BD', res = 50, cex.min = 0.5,
                  cex.max = 0.5)



library(sharpshootR)
library(igraph)
depths(data) <- ID ~ top + bottom

sort(table(data$name), decreasing=TRUE)
tp <- hzTransitionProbabilities(data, 'name')
par(mar=c(1,1,1,1))
#Plot the horizon sequence transition probability
plotSoilRelationGraph(tp, graph.mode = 'directed', edge.arrow.size=0.5, 
                      edge.scaling.factor=2, vertex.label.cex=0.75, vertex.label.family='sans')














library(aqp)
parseMunsell("10YR 3/5", return_triplets = TRUE)
# sRGB + CIELAB 
parseMunsell("10YR 3/5", return_triplets = TRUE, returnLAB = TRUE)
# CIELAB only
parseMunsell("10YR 3/5", return_triplets = FALSE, returnLAB = TRUE)


#print color chip
m <- sprintf('7.5YR 7/4')

# convert to hex representation
cols <- parseMunsell(m)
cols

# plot
soilPalette(cols, m)


#print munsell page
d <- expand.grid(hue='10YR', value=2:8, chroma=1:8, stringsAsFactors=FALSE)
d$color <- with(d, munsell2rgb(hue, value, chroma))

plot(value ~ chroma, data=d, col=d$color, pch=15, cex=3)


#Color of soil profile
data<-read.csv("Soil Profile.csv", header = TRUE)

data$soil_color <- with(data, munsell2rgb(hue, value, chroma))
depths(data) <- id ~ top + bottom
plot(data)

data$color <- paste0(data$hue, ' ', data$value, '/', data$chroma)
tp <- hzTransitionProbabilities(data, name="color", loopTerminalStates = FALSE)



library(sharpshootR)
par(mar = c(1, 1, 1, 1))
g <- plotSoilRelationGraph(tp, graph.mode = "directed", 
                           vertex.scaling.factor=2, 
                           edge.arrow.size = 0.5, edge.scaling.factor = 2.5, 
                           vertex.label.cex = 0.75, vertex.label.family =
                             "sans")


#Water Maucha
devtools::install_github("fkeck/oviz")
library(oviz)
data<-read.csv("Maucha.csv", header = TRUE)
maucha(data)

##Piper diagram
library(hydrogeo) 
p = piperPaper(size=1) 
plot(p)


LP <- piper(data)
plot( LP, main="Piper-Hill Diagram of Water Quality", cex.axis = 1,cex=1.4 )
