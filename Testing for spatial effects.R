#INSTALL PACKAGES
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("RColorBrewer")
# install.packages("classInt")
# install.packages("varhandle")
# install.packages("spatialEco")
# install.packages("plotKML")

#WORKING DIRECTORY


setwd("C:/Users/adam139/Spatial-econometrics")


#Re-run the code from the previous class

rm(list = ls())
cat("\014") 


library(rgdal)
library(spdep)
library(maptools)
library(RColorBrewer)
library(classInt)
library(spatialEco)
library(plotKML)

dane <- read.csv("nama_10r_3gdp_1_Data.csv",header = TRUE, sep = ",", dec = ",")
dane2 <- read.csv("demo_r_find3_1_Data.csv",header = TRUE, sep = ",", dec = ",")

dane$Value <- gsub(",",".",dane$Value)

dane$GEO <- as.character(dane$GEO)
dane2$GEO <- as.character(dane2$GEO)

mapa <- readOGR(".", "NUTS_RG_01M_2013")
mapa <- spTransform(mapa, "+proj=longlat")
mapa@data$GEO <- as.character(mapa@data$NUTS_ID)
mapa@data$country <- substr(mapa@data$GEO, 1, 2) 
mapa <- mapa[mapa@data$country == "IT",]

colnames(dane)[which(colnames(dane) %in% c("Value") )] <- c("PKB")
colnames(dane2)[which(colnames(dane2) %in% c("Value") )] <- c("Mean")


dane <- dane[, !(colnames(dane) %in% c("TIME","Flag.and.Footnotes","UNIT")),drop=FALSE]
dane2 <- dane2[, !(colnames(dane2) %in% c("TIME","Flag.and.Footnotes","UNIT","INDIC_DE")),drop=FALSE]

spatial_data <- merge(y = dane, x = dane2, by.y = "GEO", by.x = "GEO")
spatial_data <- merge(y = spatial_data, x = mapa, by.y = "GEO", by.x = "GEO")

spatial_data$Mean <- as.numeric(as.character(spatial_data$Mean))
spatial_data$PKB <- as.numeric(as.character(spatial_data$PKB))

rm(mapa)
rm(dane)
rm(dane2)

sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)




#GRAPHICAL EVALUATION
colors <- brewer.pal(6, "BuGn")
brks <- classIntervals(spatial_data$Mean, n = 6, style = "quantile")
brks <- brks$brks
windows(12,7)
plot(spatial_data, col = colors[findInterval(spatial_data$Mean, brks, all.inside = TRUE)], axes = F)
title(paste ("Fertility indicators [%]"),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)


model.liniowy <- lm(spatial_data$Mean ~ spatial_data$PKB)
res <- model.liniowy$residuals
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W", zero.policy = TRUE)


#Globalny Test morana
moran.test(spatial_data$Mean, listw = W1_list,alternative = "greater")
moran.plot(spatial_data$Mean, listw = W1_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#Lokalny test morana
localmoran(spatial_data$Mean, W1_list, p.adjust.method = "bonferroni")

#Geary's C test
geary.test(spatial_data$Mean, W1_list)


#Joitcount
moran.plot(res, W1_list, ylab="OpóŸnienie przestrzenne reszt", xlab="Reszty", pch = 20, main = "Wykres Morana", col = sgh_green)
lm.morantest(model.liniowy, W1_list, alternative = "greater")
 
#joincount.test(as.factor(spatial_data$Mean > 31.45), listw = W1_list)
joincount.test(as.factor(spatial_data$Mean > 31.5), listw = W1_list)
#joincount.test(as.factor(spatial_data$Mean > 31.55), listw = W1_list)

# Morana i Gearyego wskazuje na istnienie dodatniej korelacji zmiennej
# Wystêpuje dodatnia autokorelacja reszt, zatem wykorzystanie modeli przestrzennych jest zasadne.
# Wybra³em 31,5 czyli mediane ¿eby podzieliæ z podobna liczebnoœcia


