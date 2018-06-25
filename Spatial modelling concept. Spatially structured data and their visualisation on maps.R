# Zbudowanie przestrzennego modelu objaœniaj¹cego wspó³czynnik dzietnoœci do wykorzystania modelu pos³u¿ono siê jedn¹ zmienna PKB per capita
# Po przeanalizowaniu mozliwosci NUTS 3 niestety wiekszoœæ danych by³a szcz¹tkowa lub niepe³na dlatego nie da³o siê swotrzyæ pe³nego modelu np loading/unloading , 
# iloœæ przestepstw do PKB i ilosci np. me¿czyzn - Pokazane w drugim programie
# £atwo mo¿na by³o to zauwazyæ przy moran.plot gdzie lenght siê nie zgadza³o.
# Dane bra³em ze strony // http://ec.europa.eu/eurostat/web/rural-development/data //

setwd("C:/Users/adam139/Spatial-econometrics")

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