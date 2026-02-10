# ============================================================
# Title: Soil Chemistry and Soil Physics Analysis in R
#
# Description:
# - Adsorption isotherms (Freundlich, Langmuir)
# - Nutrient release kinetics models
# - Soil testing calculations (SOC, N, P, K, micronutrients)
# - Standard curve estimation
# - Soil physics calculations (Proctor test)
# - Soil water retention (van Genuchten model)
# - Hydraulic conductivity and S-index
# ============================================================

`if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("bappa10085/soilchemistry")

library(openxlsx)
library(soilchemistry)

help(package = "soilchemistry")

#Read the data
df_ad <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "Adsorption")
 head(df_ad)
#Fit Freundlich isotherm to adsorption data
 Freundlich_A(W = 2, V = 20, Ci = df_ad$Initial_conc, 
 Cf = df_ad$Equilibrium_conc)
 #if we want the repitation of this df_ad$ use with function
with(data = df_ad, Freundlich_A(W = 2, V = 20, Ci = Initial_conc, 
                                Cf = Equilibrium_conc))

#Fit Langmuir isotherm fitted to adsorption data
with(data = df_ad, Langmuir(W = 2, V = 20, Ci = Initial_conc, Cf = Equilibrium_conc))

#Fit release kinetics
df_rel <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "Kinetics")

#zero-order equation
with(data = df_rel, rk_zero(Kt = Kt, Time = Time, ylab = "Kmax - Kt"))

#First-order equation
with(data = df_rel, rk_first(Kt = Kt, Time = Time, ylab = "ln(Kmax - Kt)", xlab = "Time"))

#second-order equation
with(data = df_rel, rk_second(Kt = Kt, Time = Time, ylab = "t/Kt"))

#simplified Elovich equation
with(data = df_rel, rk_Elovich(Kt = Kt, Time = Time))

#Power function equation
with(data = df_rel, rk_power(Kt = Kt, Time = Time))

#Parabolic diffusion
with(data = df_rel, rk_pd(Kt = Kt, Time = Time))

#Soil testing calculations
library(SoilTesting)
help(package = "SoilTesting")
?SoilTesting
#WBC
df_SOC <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "WBC")

with(data = df_SOC, WBC(W = Mass_Soil, Vk = Vol_Potassium_dichromate,
                        Vb = Vol_FAS_Blank, Vs = Vol_FAS_Soil, 
                        Nk = Normality_Potassium_dichromate))

#Total Nitrogen (N) using sulphuric acid for ammonia absorption
df_N <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "Total_N")

with(data = df_N_Sulphuric, N_Sulphuric(W = Mass_Soil, VS = Vol_NaOH_Soil,
                                        VB = Vol_NaOH_Blank, X = Normality_NaOH))

#Available phosphorus (P) in soil determined by ascorbic acid blue colour method
df_P <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "AvailableP")

with(data = df_P, Available_P(W = Mass_Soil, VE = Vol_Extractant,
                              VA = Vol_Aliquot, VC = Vol_Colour, 
                              R = P_mgL_Std_curve))

#Available potassium (K) in soil
df_K <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "AvailableK")

with(data = df_K, Available_K(W = Mass_Soil, VE = Vol_Extractant,
                              VA = Vol_Aliquot, VF = Vol_Final, 
                              FR =  Flame_Reading))

#Available micronutrient cations in soil
df_micro <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "DTPA")

with(data = df_micro, DTPA_Micro(W = Mass_Soil, VE = Vol_Extractant,
                                 VA = Vol_Aliquot, VF = Vol_Final, 
                                 S = AAS_Sample, B =  AAS_Blank))

#Elemental concentration in solution from absorbance or emission values
df_stdc <- read.xlsx("D:\\R_Workshop\\ICAR\\Release kinetics_F.xlsx", sheet = "Std_Curve")

with(df_stdc, Std_Curve(C_Std = C_Std, A_E_Std = A_E_Std,
                        A_E_Sample = A_E_Sample, xlab = "Emission", 
                        ylab = "Concentration"))

#Install soilphysics package
devtools::install_github("arsilva87/soilphysics")
install.packages("rpanel") #Required for van Genuchten’s equation fitting
library(soilphysics)

#Critical Moisture and Maximum Bulk Density (Proctor test)
mois <- c(0.083, 0.092, 0.108, 0.126, 0.135)
bulk <- c(1.86, 1.92, 1.95, 1.90, 1.87)
?criticalmoisture
# Usage
criticalmoisture(theta = mois, Bd = bulk)

# Interactive Estimation of van Genuchten’s (1980) Model Parameters
# Head in cm and moisture content in m3/m3
df_vG <- read.csv("D:\\R_Workshop\\ICAR\\Soil_moisture_retention.csv")

fitsoilwater(theta = df_vG$theta, x = df_vG$h_cm)

#Unsaturated Hydraulic Conductivity as a function of water content
Kr_theta(theta=0.45,thetaS=0.56531,thetaR=0.16761,
         n = 1.52926, Ks = 106, f=0.5)

#S-index
Sindex(theta_R=0.16761, theta_S= 0.56531, alpha=0.04748, 
       n=1.52926, xlim = c(0, 1000))
