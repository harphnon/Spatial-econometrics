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

model.liniowy <- lm(spatial_data$Mean ~ spatial_data$PKB)
res <- model.liniowy$residuals
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W", zero.policy = TRUE)

#Model SARAR

model_sarar <- sacsarlm(spatial_data$Mean ~ spatial_data$PKB, listw = W1_list)
summary(model_sarar)

res_sarar <- model_sarar$residuals
moran.test(res_sarar, listw = W1_list)

#SDM

model_sdm <- lagsarlm(spatial_data$Mean ~ spatial_data$PKB, listw = W1_list, type = "Durbin")
summary(model_sdm)

res_sdm <- model_sdm$residuals
moran.test(res_sdm, listw = W1_list)


#SDEM
model_sdem <- errorsarlm(spatial_data$Mean ~ spatial_data$PKB, listw = W1_list, etype = "emixed")
summary(model_sdem)

res_sdem <- model_sdem$residuals
moran.test(res_sdem, listw = W1_list)

#SAR VS SARAR

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sarar)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SAR VS SDM

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SAR vs SDEM

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SLX vs SARAR

lL0 <- logLik(model_slx)
lL1 <- logLik(model_sarar)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SLX vs SDM

lL0 <- logLik(model_slx)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SLX vs SDEM

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

# SEM vs SARAR

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sarar)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SEM vs SDM

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SEM vs SDEM

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

# Z dwoma Ÿród³ami 

#SARAR vs SDM

lL0 <- logLik(model_sarar)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SARAR vs SDEM

lL0 <- logLik(model_sarar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#SDM vs SDEM

lL0 <- logLik(model_sdm)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#Najlepszy model SDM - wykazuje test LR

summary(model_sdm)

#SDM - przeciwne znaki mozliwe ze efekty sie znosz¹(np. brak dodatkowych zmienych), Rho - istotne statystycznie
#Niestety nie uda³o mi siê zrobiæ modelu o powiekszonej jednostce np. PKB. lecz poprawa sytuacji w jednym regionie moze miec pozytywny skutek na drugi . Powiêkszanie o jednostke wykrzaczalo mi macierz.