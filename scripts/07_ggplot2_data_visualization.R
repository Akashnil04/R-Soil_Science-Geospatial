# ============================================================
# Title: Data Visualization using ggplot2 in R
#
# Description:
# - Scatter plots, histograms, density plots
# - Boxplots, violin plots, dot plots
# - Regression lines and smoothing
# - Faceting (facet_wrap & facet_grid)
# - Themes, annotations, and labels
# - Time series visualization
# ============================================================


setwd("D:\\R_Workshop\\ICAR\\18th Nov")
library(ggplot2)
?mpg
View(mpg)

#====================================
attach(mpg)

#Only axes
ggplot(mpg)

#Only axes provided
ggplot(mpg, aes(x=displ, y=hwy))

#Plotting points
ggplot(mpg, aes(x=displ, y=hwy))+
       geom_point()

#----------Creating Layers----------
gg=ggplot(mpg, 
          aes(x=displ, y=hwy))

#Additional property of points
gg+geom_point(col=2, 
              size=5, 
              pch=19)

#Proving color as per the cylinder values
gg+geom_point(col=cyl, 
              size=2, 
              pch=19)

#Proving size as per the cylinder values
gg+geom_point(col=cyl, 
              size=cyl, 
              pch=19)

#Only colour
g=ggplot(mpg)+ 
  geom_point(aes(x=displ, y=hwy)) 

g+aes(col=class)

#Only Size
g+aes(size=class)

#Shape
g+aes(shape=class)

#Size & Colour
g+aes(size=class, 
      colour=(cyl))
#Size & Colour
g+aes(size=class, 
      colour=as.factor(cyl))

#Alpha
g+aes(alpha=class, 
      colour=class)


#Alpha in Density plot

ggplot(mpg,aes(x=displ,
               fill=as.factor(cyl)))+
       geom_density()

ggplot(mpg,aes(x=displ,
               fill=as.factor(cyl)))+
  geom_density(alpha=.5)

#Topics Covered:
#1. Horizontal Line
#2. Vertical line
#3. Pattern 
#4. Straight Line
#5. Annotations
#6. Titles & Labels
#7. Themes

gg=ggplot(data=mpg,aes(x=displ, y=hwy))+
  geom_point()

gg


#1. Horizontal Line
gg+geom_hline(yintercept = 25, 
              size=1, colour="red2")

#2. Vertical Line
gg+geom_vline(xintercept = 4, 
              size=2, colour="red2")

#3. Curvi-linear Pattern
gg+geom_smooth()

#4. Linear Regression Line  
gg+geom_smooth(size=1.5, 
               method = "lm")


#5. Additional Text Information  
gg+annotate(geom = "text", 
            label="R2=.5",
            x=5, y=40, size=5, 
            col=2)+
  geom_smooth(size=1.5, 
              method = "lm")

#6. Titles & Labels
gg+labs(title="My Chart", 
        subtitle="Highway Mileage", 
        y="Highway Mileage", 
        x="Displacement", 
        caption="First chart")


#7. Different Themes
gg+theme_bw()
gg+theme_gray()
gg+theme_grey()
gg+theme_linedraw()
gg+theme_light()
gg+theme_minimal()
gg+theme_classic()
gg+theme_void()
gg+theme_dark()
#########################################################
#==========Different geom===========
# 1. Univariate Analysis
# 2. Bivariate Analysis


#=======Univariate Analysis=========
c=ggplot(mpg, aes(displ))

#1.Histogram
# geom_histogram()

?geom_histogram
c+geom_histogram()
c+geom_histogram(bins=6)
c+geom_histogram(bins=6, fill=5)
c+geom_histogram(bins=6, fill=5, col=1)

#2.Frequency polygon()
# geom_freqpoly()

c+geom_freqpoly()
c+geom_histogram()

c+geom_histogram(fill=5, col=1)+
  geom_freqpoly(col=2,size=1)

#3.Density Plot
#geom_density()
?geom_density()
c+geom_density()
c+geom_density(stat = "count")

#4.Dotplot
#geom_dotplot()
c+geom_dotplot()
c+geom_dotplot(binwidth = .1, 
               fill=5)


#5.Boxplot
#geom_boxplot()

?geom_boxplot
c+geom_boxplot()
c+geom_boxplot(aes(x=hwy))
c+geom_boxplot(aes(x=hwy),
               outlier.color = 2,
               outlier.size = 3,
               outlier.stroke = 2,
               outlier.shape = 8)

#-------***---------***----------







#====================================

#------------------------------
#One Discrete Variable 
#------------------------------

d=ggplot(mpg, aes(drv))
summary(factor(drv))

 #1.Barplot- geom_bar() 
?geom_bar()
d+geom_bar()
d+geom_bar(fill=5, col=1)
d+geom_bar(fill=5, col=1, size=3)


#------------------------------
#One Continuous Variable &
#One Discrete Variable
#------------------------------

cd=ggplot(mpg, aes(factor(drv),hwy))

#1.Violinplot
#geom_violin()
?geom_violin()
cd+geom_violin()
cd+geom_violin(fill=5)

#2.Boxplot
#geom_boxplot()
?geom_boxplot()

c+geom_boxplot()
c+geom_boxplot(aes(y=class))
c+geom_boxplot(aes(y=class), fill=7)

cd+geom_boxplot()

cd+geom_boxplot(outlier.shape = 3,
                outlier.colour = 2,
                outlier.size = 4,
                outlier.stroke = 2)

#Violin Plot and Boxplot combined
cd+geom_violin(fill=5)
cd+geom_violin(fill=5)+geom_boxplot()


#------------------------------
#Two Continuous Variables
#------------------------------

#1.Scatterplot
#geom_point()
?geom_point()

cc=ggplot(mpg, aes(displ,hwy))
cc+geom_point()
  cc+geom_point(aes(col=class))

cc+geom_point(aes(col=class,
                  size=factor(cyl)))

cc+geom_point()+geom_smooth(span=0.5)

cc+geom_point()+
  geom_smooth(method = lm, se=T)

#2.Quantile plot
#geom_quantile()
?geom_quantile()
cc+geom_quantile()

#3.Jitter Plot
#geom_jitter()
?geom_jitter()
cc+geom_jitter()
cc+geom_jitter(aes(col=class,
                   size=factor(cyl)))

#4.Line plot
#geom_line()
?geom_line()
cc+geom_line()

#Time Series Analysis
#Datafiles: economics & economics_long
ggplot(economics, 
       aes(date, unemploy)) + 
  geom_line()

ggplot(economics, 
       aes(date, unemploy)) +
  geom_line(colour = "red")

ggplot(economics_long, 
       aes(date, value01, 
           colour = variable)) +
  geom_line()

#-------***---------***----------
#Facet wrap and grid


#Only axes provided
g=ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()
g

#Split charts for different classes
g+facet_wrap(~class)

g+facet_wrap(~class, nrow = 2)

summary(as.factor(drv))
summary(as.factor(cyl))

g+facet_grid(drv~cyl)

g+facet_grid(drv~cyl)+
  aes(col=class)


#==========Different geom===========
# 1. Univariate Analysis
# 2. Bivariate Analysis
 

#=======Univariate Analysis=========

g=ggplot(mpg, aes(x=displ))

#------------Histogram------------
g + geom_histogram()

g + geom_histogram(binwidth = .75,
                   fill="grey",
                   colour="black")

#------------Density------------
g + geom_density()
colours()
ggplot(mpg,aes(x=displ,
             fill=as.factor(cyl)))+
       geom_density()

ggplot(mpg,aes(x=displ,
               fill=as.factor(cyl)))+
       geom_density(alpha=.5)

#------------Boxplot------------
g + geom_boxplot(colour="red",
                 fill="green")

gg + geom_boxplot(colour="red",
                  fill="green")

#------------Dotplot------------
g + geom_dotplot()

g + geom_dotplot(binwidth = .2)
  

#========Bivariate Analysis=========

#-----------Scatter Plot------------
gg + geom_jitter(height = 0.5, 
                 width = .3)


#------------Violin Plot------------
ggplot(mpg, 
       aes(x=displ,y=hwy, 
           fill=as.factor(cyl)))+
       geom_violin(alpha=.5)+
       geom_boxplot()

gg + geom_point()
gg + geom_line()
gg + geom_smooth()
gg + geom_point() + geom_smooth() 
gg + geom_point() + geom_smooth(method = 'lm')

#------------Column plot------------
ggplot(mpg, aes(x=as.factor(cyl),y=displ, fill=cyl))+
     geom_col() 

ggplot(mpg, aes(x=as.factor(cyl),y=displ, fill=class))+
  geom_col() 

ggplot(mpg, aes(x=as.factor(cyl),y=displ, fill=class))+
  geom_col(position = "dodge") 


#Regression line through all point, color by cylinder
gg + geom_point(aes(col=2,size=4))+
  geom_smooth(method="lm",col=1, size=1)


#===Adding Title & Labels to Chart======

g1=ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point(col=6, size=3)+
  geom_smooth(method="lm",col=1, size=2)

g1

g1+labs(title="My Chart", 
        subtitle="Highway Mileage", 
        y="hwy", x="hwy", 
        caption="First chart")


#==============Annotation============
g1+annotate(geom="text", x=5, y=35,
            label="r2=0.58")

