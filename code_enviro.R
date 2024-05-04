library(readr)
library(ggplot2)
library(psych)
library(vegan)
library(cowplot)

setwd("~/Uni/Year3/Semester2/SOES3035_Research_Training/Assessments/Assignment2_Group_Project/Code")

env <- read.csv("data/env_richness.csv")

# remove ID, closest lat, ecoregion (categorical)
environmental <- env[, c(-1, -3, -5)]

# check correlations and distributions
pairs.panels(environmental[, -c(1, 15)],
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             cex.axis = 2,
             cex.labels = 1.5,
             cex.cor = 2, #sets default font size of correlation values
             density = TRUE, # show density plots
             ellipses = FALSE, #don't show correlation ellipses
             main = "A"
             )

environmental_T <- environmental

# spread
environmental_T$Calcite <- log(environmental_T$Calcite)
environmental_T$Chlorophyll_a <- log(environmental_T$Chlorophyll_a)
environmental_T$Silicate <- log(environmental_T$Silicate)
environmental_T$SST <- log(environmental_T$SST)


colnames(environmental_T)[colnames(environmental_T) 
                                   == "Calcite"] <- "logCalcite"
colnames(environmental_T)[colnames(environmental_T) 
                                   == "Chlorophyll_a"] <- "logChl-a"
colnames(environmental_T)[colnames(environmental_T)
                          == "SST"] <- "logSST"
colnames(environmental_T)[colnames(environmental_T) 
                          == "Silicate"] <- "logSilicate"


# outliers
environmental_T$Average_tidal_amplitude <- log(environmental_T$Average_tidal_amplitude)
colnames(environmental_T)[colnames(environmental_T) 
                          == "Average_tidal_amplitude"] <- "logAmplitude"


# replot draftsmans plot:
pairs.panels(environmental_T[, -c(1, 15)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             cex.axis = 2,
             cex.labels = 1.5,
             cex.cor = 2, #sets default font size of correlation values             density = TRUE, # show density plots
             ellipses = FALSE, #don't show correlation ellipses
             main = "B"
             )


#####
# standardise the environmental data
# not normalisation as data is now transformed
Envn_Norm <- scale(environmental_T[,-1])
Bin <- (environmental_T$LAT)
row.names(Envn_Norm) <- Bin

#produce the distance Matrix: 
Envn_Resem <- vegdist(Envn_Norm, method = "euclidean")
sink("distance_mat.txt")
Envn_Resem
sink()
