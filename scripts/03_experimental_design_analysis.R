# ============================================================
# Title: Experimental Design Analysis in R
# Description: CRD, RCBD, LSD, BIBD, PBIBD, Resolvable and
#              Augmented Block Designs using ANOVA
# Author: Akashnil Kaibartta

# ============================================================

crd<- read.delim("crd.txt")
getwd()
attach(crd) 
names(crd) 

###################CRD with Unequal Replications################
trt=factor(trt) 
lm1=lm(yield~trt) 
anova(lm1) 



library(lsmeans)
lsm=lsmeans(lm1,"trt") 
lsm
pairs(lsm)

#pairs statement is an optional statement
#to provide letters for groups, need to install "multcompView OR multcomp"
library(multcomp)
cld(lsm,Letters="abcd") 
detach(crd)
###############################################

###########################RCBD###################################
rcbd<- read.delim("rcbd.txt")
attach(rcbd) 
names(rcbd) 
trt=factor(trt) 
blk=factor(blk) 
lm2=lm(yield~trt+blk) 
anova(lm2) 
library(lsmeans)
lsm=lsmeans(lm2,"trt") 
lsm
pairs(lsm)
#pairs state(multcomp)
cld(lsm, Letters="abcdefghij", decreasing = TRUE, reversed = TRUE) 
detach(rcbd)
###################################################ment is an optional statement
#to provide letters for groups, need to install multcompView library

########################LSD####################################
lsd<- read.delim("lsd1.txt")
attach(lsd) 
names(lsd) 
row=factor(lsd$row) 
col=factor(lsd$col) 
trt=factor(lsd$trt)
lm3=lm(yield~trt+row+col) 
anova(lm3)

library(lsmeans) 
lsm=lsmeans(lm3,"trt") 
lsm
pairs(lsm)
#pairs statement is an optional statement
#to provide letters for groups, need to install multcompView library(multcompView)
cld(lsm, Letters="abcdefghij",  reversed = TRUE) 
detach(lsd)

#####################################################

########################BIBD####################################
Ibd<- read.delim("BIBD.txt")
attach(Ibd) 
names(Ibd) 
blk=factor(Ibd$BLK) 
trt=factor(Ibd$TRT)
lm4=lm(CALORIES~blk+trt) 
#lm4=lm(CALORIES~trt+blk) 

anova(lm4)

library(lsmeans) 
lsm=lsmeans(lm4,"trt") 
lsm
pairs(lsm)
#pairs statement is an optional statement
#to provide letters for groups, need to install multcompView library(multcompView)
library(multcomp)
cld(lsm, Letters="abcdefghij",  reversed = TRUE) 
detach(Ibd)
#####################################################

########################PBIBD####################################
pbibd<- read.delim("PBIBD.txt")
attach(pbibd) 
names(pbibd) 
blk=factor(pbibd$blk) 
trt=factor(pbibd$trt)
lm6=lm(yield~blk+trt) 
#lm6=lm(yield~trt+blk) 

anova(lm6)

library(lsmeans) 
lsm=lsmeans(lm6,"trt") 
lsm
pairs(lsm)
#pairs statement is an optional statement
#to provide letters for groups, need to install multcompView library(multcompView)
library(multcomp)
cld(lsm, Letters="abcdefghij",  reversed = TRUE) 
detach(pbibd)
#####################################################

#################Contrast Analysis#################################
contrast(lsm, list(con1=c(6,-1,-1,-1,-1,-1,-1), con2=c(-1, 1,0,0,0,0,0)))
##########################################

########################Resolvable designs####################################
resolvable<- read.delim("resolvable.txt")
attach(resolvable) 
names(resolvable) 
trt=factor(resolvable$trt) 
rep=factor(resolvable$rep) 
blk=factor(resolvable$blk)
lm5=lm(syield~trt+blk+rep/blk-blk) 
anova(lm5)

library(car) 
Anova(lm5,type="III") ###for adjusted treatment sum of squares#########

library(lsmeans) 
lsm=lsmeans(lm5,"trt") 
lsm
pairs(lsm)

#pairs statement is an optional statement
#to provide letters for groups, need to install multcompView library(multcompView)
cld(lsm,Letters="abcdefghij",reversed = TRUE) 
detach(resolvable)
##############################

########################Resolvable designs####################################
lattice<- read.delim("lattice.txt")
attach(lattice) 
names(lattice) 
trt=factor(lattice$trt) 
rep=factor(lattice$rep) 
blk=factor(lattice$blk)
lm7=lm(yield~trt+blk+rep/blk-blk) 
anova(lm7)

library(car) 
Anova(lm7,type="III") ###for adjusted treatment sum of squares#########

library(lsmeans) 
lsm=lsmeans(lm5,"trt") 
lsm
pairs(lsm)

#pairs statement is an optional statement
#to provide letters for groups, need to install multcompView library(multcompView)
cld(lsm,Letters="abcdefghij",reversed = TRUE) 
detach(resolvable)
##############################

########################Augmented block designs####################################
augmented<- read.delim("augmented.txt")
names(augmented) 
trt1=factor(augmented$trt) 
blk1=factor(augmented$blk) 


lm8=lm(augmented$gw~blk1+trt1) 
anova(lm8)

library(agricolae)
block<-augmented$blk
trt<-augmented$trt
yield<-augmented$gw
#out<- DAU.test(block,trt,yield,method="tukey", group=TRUE)
#print(out$SE.difference)


blk1=factor(augmented$blk) 


lm8=lm(augmented$gw~blk1+trt1) 
anova(lm8)

#OR

library(augmentedRCBD)
out1 <- augmentedRCBD(factor(blk), factor(trt), yield, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE,checks = c("1", "2", "3", "4"))

################################################################




####CRD
library(agricolae)
getwd()
setwd("D:\\R_Workshop\\ICAR")
## IMPORT DATA

CRD_DATA<-read.csv("D:\\R_Workshop\\ICAR\\crd_data.csv",header=TRUE)

str(CRD_DATA)
# Convert Treatment column to factor 

CRD_DATA$Treatment<-as.factor(CRD_DATA$Treatment)

#now check again
str(CRD_DATA)


#Fit Linear Model and Perform ANOVA

crd_model<-lm(Yield~Treatment,data=CRD_DATA)

crd_anova<-anova(crd_model)
crd_anova

### Treatments Comparison (LSD Test)
# Load agricolae package
library(agricolae)
crd_lsd<-LSD.test(crd_model, "Treatment")
# Perform LSD test
LSD_result <- LSD.test(crd_model, "Treatment",alpha=0.01)



setwd("D:\\R_Workshop\\ICAR")
RBD_DATA<-read.csv(file.choose(),header = TRUE)

str(RBD_DATA)
RBD_DATA$Replication<-as.factor(RBD_DATA$Replication)
RBD_DATA$Treatment<-as.factor(RBD_DATA$Treatment)
rbd_model<-lm(Yield~ Replication+ Treatment,data = RBD_DATA)

rbd_anova<-anova(rbd_model)
rbd_anova
RBD

library(agricolae)
rbd_lsd <- LSD.test(rbd_model,"Treatment")


setwd("D:\\R_Workshop\\ICAR")
FACT_DATA <- read.csv(file.choose(), header=TRUE)
str(FACT_DATA)
FACT_DATA$Replication <- as.factor(FACT_DATA$Replication)
FACT_DATA$A <- as.factor(FACT_DATA$A)
FACT_DATA$B <- as.factor(FACT_DATA$B)
str(FACT_DATA)
fact_model <- lm(Yield~ Replication + A + B + A:B, data=FACT_DATA)
fact_anova <- anova(fact_model)


#### RBD

RBD_DATA<-read.csv(file.choose(),header = TRUE)
RBD_DATA
str(RBD_DATA)
# Convert Replication & Treatment column to factor 
RBD_DATA$Replication<-as.factor(RBD_DATA$Replication)
RBD_DATA$Treatment<-as.factor(RBD_DATA$Treatment)
str(RBD_DATA)

#Fit Linear Model and Perform ANOVA

rbd_model<-lm(Yield~Replication+Treatment,data=RBD_DATA)

rbd_anova<-anova(rbd_model)


# Load agricolae package
library(agricolae)
# Perform LSD test
LSD_result <- LSD.test(rbd_model, "Treatment")


#Two factor factorial

FACT_DATA<-read.csv(file.choose(),header = TRUE)

#convert Replication, A and B column to factor
FACT_DATA$Replication<-as.factor(FACT_DATA$Replication)
FACT_DATA$A<-as.factor(FACT_DATA$A)
FACT_DATA$B<-as.factor(FACT_DATA$B)


#Fit Linear Model and Perform ANOVA

fact_model<-lm(Yield ~ Replication +A+B+A:B, data = FACT_DATA)

fact_anova<-anova(fact_model)


### Treatments Comparison (LSD Test)
library(agricolae)
# LSD for main effects
LSD_A <- LSD.test(fact_model, "A")

LSD_B <- LSD.test(fact_model, "B")


LSD_AB <- LSD.test(fact_model, c("A","B"))


