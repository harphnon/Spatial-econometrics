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
plot(spatial_data, col = colors[findInterval(spatial_data$Mean, brks, all.inside = TRUE)], axes = F)
title(paste ("Fertility indicators [%]"),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)


model.liniowy <- lm(spatial_data$Mean ~ spatial_data$PKB)
res <- model.liniowy$residuals
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W", zero.policy = TRUE)

#Model czystej autoregresjii przestrzennej
model_pure_sar <- spautolm(spatial_data$Mean ~ 1, listw = W1_list)
summary(model_pure_sar)

res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W1_list)

#SAR
model_sar <- lagsarlm(spatial_data$Mean ~ spatial_data$PKB, listw = W1_list)
summary(model_sar)

res_sar <- model_sar$residuals
moran.test(res_sar, listw = W1_list)

colors <- brewer.pal(6, "BuGn")
brks <- classIntervals(res_sar, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sar, brks, all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu SAR"),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)

#SLX

W <- listw2mat(W1_list)
X <- cbind(spatial_data$PKB)
WX <- W %*% X
lag.PKB <- WX [, 1]


model_slx <- lm(spatial_data$Mean ~ spatial_data$PKB + lag.PKB)
summary(model_slx)

print(paste("AIC:",as.character(AIC(model_slx))))
res_slx <- model_slx$residuals
lm.morantest(model_slx, listw = W1_list)

colors <- brewer.pal(6, "BuGn")
brks <- classIntervals(res_slx, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_slx, brks, all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu SLX"),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)



#SEM

model_sem <- errorsarlm(spatial_data$Mean ~ spatial_data$PKB, listw = W1_list)
summary(model_sem)

res_sem <- model_sem$residuals
moran.test(res_sem, listw = W1_list)

colors <- brewer.pal(6, "BuGn")
brks <- classIntervals(res_slx, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_slx, brks, all.inside = TRUE)], axes = F)
title(paste ("Reszty z modelu SEM "),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)



#Testy LR
#1
lL0 <- logLik(model_sar)
lL1 <- logLik(model_slx)
LRa <- 2 * (L1 - L0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))
#2
lL0 <- logLik(model_sar)
lL1 <- logLik(model_sem)
LRa <- 2 * (L1 - L0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))
#3
lL0 <- logLik(model_sem)
lL1 <- logLik(model_slx)
LRa <- 2 * (L1 - L0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))


# Lambda jest dodatnia w pure SAR czyli dodatnio skorelowane
# SLX pokazalo autokorelacje reszt
# Wybieram model SEM z powodu test LR, najmniej skorelowane reszty
# AIC kryterium we wszystkich przypadkach by³o podobne lecz model SEM mia³ najni¿sza wartoœæ
