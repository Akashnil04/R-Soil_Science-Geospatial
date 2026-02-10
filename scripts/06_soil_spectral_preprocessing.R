# ============================================================
# Title: Soil Spectral Data Preprocessing in R
#
# Description:
# - Reflectance spectra visualization
# - Continuum removal
# - Absorbance transformation
# - Savitzky–Golay smoothing and derivatives
# - SNV and MSC corrections
# - Moving average filtering
# - Spectral resampling (10 nm)
# ============================================================
setwd("D:\\R_Workshop\\ICAR")
library(pls)
library(prospectr)

par(family = "serif")
par(font = 2)   # main text bold
par(font.lab = 2)  # axis labels bold
par(font.axis = 2) # axis tick labels bold

#Read the data
data_1 <- read.csv("Spectral_data.csv")

#Subset the non-numeric column
data <- subset(data_1, select = -c(Samples))

#Create a sequence between 350 to 2500 at an interval of 1
wav <- seq(350, 2500, 1)

#Plot the spectra
matplot(wav, t(data[1:72,]))

#Line plot of the spectral data
matplot(wav, t(data[1:72,]), type = 'l')

#Change the plot axis titles
matplot(wav, t(data[1:72,]), type = 'l', xlab = 'Wavelength (nm)',
        ylab = 'Reflectance')

#Change the plot axis ticks (350-2500 nm)
matplot(wav, t(data[1:72,]), type='l', xlab='Wavelength (nm)',
        ylab='Reflectance', xaxt="n")
axis(1, at = seq(350, 2500, by = 430),labels=c(seq(350, 2500, by = 430)))
mtext('Reflectance spectra')

#Continuum Removal
# type = 'A' is used for absorbance spectra
cr <- continuumRemoval(data, wav, type = 'R')

#Plot the continued removed spectra
matplot(wav, t(cr[1:72,]), type = 'l', xlab = 'Wavelength (nm)',
        ylab = 'Reflectance', xaxt = "n")
axis(1, at = seq(350, 2500, by = 430), labels = c(seq(350, 2500, by = 430)))
mtext('Continuum Removal')

#Write the transformed data
write.csv(cr, "Continuum Removal.csv")

# conversion from reflectance to absorbance A = log10(1/R)
absorbance <- log(1/data)

# Plot the absorbance data
matplot(x = wav, y = t(absorbance),
        xlab = "Wavelength (nm)",
        ylab = "Absorbance",
        type = "l",
        lty = 1)
mtext('Absorbance spectra')

#Write the absorbance data
write.csv(absorbance, "Absorbance.csv")

##Detrend transformation
det <- detrend(data, wav)

#Plot the detrended data
matplot(wav, t(det[1:72,]), type= 'l', xlab = 'Wavelength (nm)', 
        ylab = 'Reflectance', xaxt="n")
axis(1, at = seq(350, 2500, by = 430),labels=c(seq(350, 2500, by = 430)))
mtext('Detrended spectra')

#Write the transformed data
write.csv(det,"Detrend Spectra.csv")

#Savitzky-Golay smoothing
SG_0_9 <- savitzkyGolay(X = data, m = 0, p = 0, w = 9)
write.csv(SG_0_9, "SG_0_9.csv")

#m differentiation order
#p polynomial order
#w window size (must be odd)
#delta.wav optional sampling interval

#Savitzky-Golay 1st derivative using a 1st-order polynomial
SGF_1_1_9 <- savitzkyGolay(X = data, m = 1, p = 1, w = 9)
write.csv(SGF_1_1_9,"SGF_1_1_9.csv")

#Savitzky-Golay 1st derivative using a 2nd-order polynomial
SGF_1_2_9 <- savitzkyGolay(X = data, m = 1, p = 2, w = 9)
write.csv(SGF_1_2_9,"SGF_1_2_9.csv")

#Savitzky-Golay 2nd derivative using a 2nd-order polynomial
SGS_2_2_9 <- savitzkyGolay(X = data, m = 2, p = 2, w = 9)
write.csv(SGS_2_2_9, "SGS_2_2_9.csv")

#Savitzky-Golay smoothing plotting
matplot(sort(as.numeric(as.factor(colnames(SG_0_9)))), t(SG_0_9[1:72,]), 
        type = 'l', xlab = 'Wavelength (nm)', ylab = 'Reflectance', 
        xaxt = "n")
axis(1, at = seq(1, 2151, by = 430),labels=c(seq(350, 2500, by = 430)))
mtext('Savitzky-Golay smoothing')

#Savitzky-Golay 1st derivative using a 1st-order polynomial
matplot(sort(as.numeric(as.factor(colnames(SGF_1_1_9)))),t(SGF_1_1_9[1:72,]), 
        type = 'l', xlab = 'Wavelength (nm)', ylab = '1st derivative', 
        xaxt = "n")
axis(1, at = seq(1, 2151, by = 430),labels = c(seq(350, 2500, by = 430)))
mtext('First order derivative')

#Savitzky-Golay 2nd derivative using a 2nd-order polynomial
matplot(sort(as.numeric(as.factor(colnames(SGS_2_2_9)))),t(SGS_2_2_9[1:72,]), 
        type = 'l', xlab = 'Wavelength (nm)', ylab = '2nd derivative',
        xaxt = "n")
axis(1, at = seq(1, 2151, by = 430), labels = c(seq(350, 2500, by = 430)))
mtext('Second order derivative')

#Standard Normal Variate (SNV) transformation
snv <- standardNormalVariate(data)

#Plot the SNV transformed data
matplot(wav, t(snv[1:72,]), type = 'l',xlab = 'Wavelength (nm)', 
        ylab = 'Standard Normal Variate (SNV)', 
        xaxt = "n")
axis(1, at = seq(350, 2500, by = 430), labels = c(seq(350, 2500, by = 430)))
mtext('Standard Normal Variate (SNV)')

#Write the Standard normal variate transformed data
write.csv(snv,"Standard normal variate transformation.csv")

#Multiplicative Scatter Correction
msc <- msc(as.matrix(data))

#Effect of MSC on raw spectra
plot(wav, data[1,], type = "l", xlab = "Wavelength (nm)", ylab = "Absorbance", 
     lwd = 1.5)
lines(wav, msc[1,], lwd = 1.5, col = "red")
axis(4, col = "red")
grid()
legend("topleft", legend = c("raw", "MSC signal"), 
       lty = c(1, 1), col = c("black", "red"))

#Plot the MSC transformed data
matplot(wav, t(msc[1:72,]), type = 'l', xlab = 'Wavelength (nm)', 
        ylab = 'Multiplicative Scatter Correction', 
        xaxt = "n")
axis(1, at = seq(350, 2500, by = 430), labels = c(seq(350, 2500, by = 430)))
mtext('Multiplicative Scatter Correction (MSC)')

#Write the Multiplicative Scatter Correction transformed data
write.csv(msc, "Multiplicative Scatter Correction.csv")

# Moving average filter
# add some noise
noisy <- data + rnorm(length(data), 0, 0.001) 

# Plot the first spectrum
plot(x = wav,
     y = noisy[1, ],
     type = "l",
     lwd = 1.5,
     xlab = "Wavelength", 
     ylab = "Reflectance") 

X <- movav(X = noisy, w = 11) # window size of 11 bands

# Note that the 5 first and last bands are lost in the process
lines(x = as.numeric(gsub("WV", "", colnames(X))), y = X[1,], 
      lwd = 1.5, col = "red")
grid()
legend("topleft", 
       legend = c("Raw", "Moving average"), 
       lty = c(1, 1), col = c("black", "red"))

# plot the noisy spectrum
plot(as.numeric(gsub("WV", "", colnames(noisy))), noisy[1,],
     type = "l",
     ylab = "Reflectance", xlab = "Wavelength (nm)",
     col = "red",
     xlim = c(1250, 1750), ylim = c(0.1, 0.35))

# add the smoothed spectrum
lines(as.numeric(gsub("WV", "", colnames(X))), X[1, ],
      col = "black")

# add a legend
legend("topright",
       legend = c("Raw", "Moving average"),
       lty = c(1, 1),
       col = 2:1)

#Resampling at 10nm interval
resampled <- resample(data, wav, seq(350, 2500, 10), interpol="spline") 
write.csv(resampled,"Resampled_spectra_10nm.csv", row.names = data_1$Samples)

#Plot the resampled spectra
matplot(seq(350, 2500, 10), t(resampled[1:72,]), type = 'l', xlab = 'Wavelength (nm)',
        ylab = 'Reflectance', xaxt = "n")
axis(1, at = seq(350, 2500, by = 430),labels=c(seq(350, 2500, by = 430)))
mtext('Resampling at 10nm')

#Dimension of spectra library
dim(data); dim(resampled)
