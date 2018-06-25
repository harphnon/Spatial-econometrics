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

cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W", zero.policy = TRUE)

SDM <- lagsarlm(spatial_data$Mean ~ spatial_data$PKB , listw = W1_list, type = "Durbin", control = list(fdHess = TRUE))
impacts.SDM <- impacts(SDM, listw = W1_list, zstats = TRUE, R = 200)

HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "direct")

HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "indirect")

HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "total")

summary(impacts.SDM)

# Na poziomie istotnoœci 0.05 > efekt bezpoœredni
