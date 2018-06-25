#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
# install.packages("spdep")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("RColorBrewer")
# install.packages("classInt")
install.packages("geospacom")
install.packages("clusterSim")

#WORKING DIRECTORY
setwd("C:/Users/adam139/Spatial-econometrics")


#Re-running code from previous class


rm(list = ls())
cat("\014") 
library(rgdal)
library(spdep)
library(geospacom)
dane <- read.csv("nama_10r_3gdp_1_Data.csv",header = TRUE, sep = ",", dec = ",")
mapa <- readOGR(".", "NUTS_RG_01M_2013")
mapa <- spTransform(mapa, "+proj=longlat")
mapa@data$GEO <- as.character(mapa@data$NUTS_ID)
mapa@data$country <- substr(mapa@data$GEO, 1, 2) 
mapa <- mapa[mapa@data$country == "DE", ]
dane$GEO <- as.character(dane$GEO)
spatial_data <- merge(y = dane, x = mapa, by.y = "GEO", by.x = "GEO")
rm(mapa)
rm(dane)

#Will also be useful...
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
N <- nrow(spatial_data)
centroids <- coordinates(spatial_data)
plot(spatial_data)
points(centroids, pch = 16, col = sgh_green)

#Way 1: Kryterium normalizacji - najwy¿sza wartoœæ w³asna
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "S")
W1 <- listw2mat(W1_list)
plot.nb(cont1, centroids, col = sgh_green, pch = 16)
W1_list$weights

#Way 2: Nie wiêkszy dystans niz 200km
distance <- DistanceMatrix(spatial_data, "GEO", unit = 1000)
gamma <- 1
W2 <- 1 / (distance ^ gamma)
threshold <- 1 / (200 ^ gamma)     # (próg, powy¿ej którego bêd¹ wartoœci niezerowe)
W2[W2 < threshold] <- 0 # (zerujemy to co jest poni¿ej)
diag(W2) <- 0
W2 <- W2 / as.matrix(rowSums(W2)) %*% matrix(1, nrow = 1, ncol = N)
W2_list <- mat2listw(W2, style="W")
summary(rowSums(W2))

# Way 3 : 

#PKB <- read.csv("nama_10r_3gdp_1_Data.csv",header = TRUE, sep = ",", dec = ",")
#Loading <- read.csv("road_go_na_rl3g_1_Data.csv",header = TRUE, sep = ",", dec = ",")
#unloading <- read.csv("road_go_na_ru3g_1_Data.csv",header = TRUE, sep = ",", dec = ",")

#PKB$Value <- gsub(",","",PKB$Value)
#Loading$Value <- gsub(",","",Loading$Value)
#unloading$Value <- gsub(",","",unloading$Value)

#PKB$GEO <- as.character(PKB$GEO)
#Loading$GEO <- as.character(Loading$GEO)
#unloading$GEO <- as.character(unloading$GEO)

#mapa <- readOGR(".", "NUTS_RG_01M_2013")
#mapa <- spTransform(mapa, "+proj=longlat")
#mapa@data$GEO <- as.character(mapa@data$NUTS_ID)
#mapa@data$country <- substr(mapa@data$GEO, 1, 2) 
#mapa <- mapa[mapa@data$country == "DE", ]

#colnames(PKB)[which(colnames(PKB) %in% c("Value") )] <- c("pkb")
#colnames(Loading)[which(colnames(Loading) %in% c("Value") )] <- c("loading")
#colnames(unloading)[which(colnames(unloading) %in% c("Value") )] <- c("unloading")

#PKB <- PKB[, !(colnames(PKB) %in% c("TIME","TIME_LABEL","Flag.and.Footnotes","UNIT")),drop=FALSE]
#Loading <- Loading[, !(colnames(Loading) %in% c("TIME","IPC","Flag.and.Footnotes","UNIT","NST07",'NST07_LABEL')),drop=FALSE]
#unloading <- unloading[, !(colnames(unloading) %in% c("TIME","IPC","Flag.and.Footnotes","UNIT","NST07","NST07_LABEL")),drop=FALSE]

#spatial_data <- merge(y = uloading, x = Loading, by.y = "GEO", by.x = "GEO")
#spatial_data <- merge(y = spatial_data, x = PKB, by.y = "GEO", by.x = "GEO")
#spatial_data <- merge(y = spatial_data, x = mapa, by.y = "GEO", by.x = "GEO")

#spatial_data$loading <- as.numeric(as.character(spatial_data$loading))
#spatial_data$unloading <- as.numeric(as.character(spatial_data$unloading))
#spatial_data$pkb <- as.numeric(as.character(spatial_data$pkb))

#rm(Loading)
#rm(mapa)
#rm(PKB)
#rm(unloading)

distance_1 <- DistanceMatrix(spatial_data, "GEO", unit = 1000) # macierz z nag³ówkami

for (i in 1:nrow(distance_1)) {  
  for (j in 1:ncol(distance_1)) {
    distance_1[i,j] <- 1/sqrt((spatial_data$PKB[i,1] - spatial_data$PKB[j,1])^2 + (spatial_data$loading[i,2] - spatial_data$loading[j,2])^2 + (spatial_data$unloading[i,3] - spatial_data$unloading[j,3])^2)
    if (distance_1[i,j] == Inf) {
      distance_1[i,j] <- 0
    }
  }
}

#Jako ¿e mieliœmy tylko pokazaæ model, wzia³em dane z testów które przeprowadza³em wczeœniej
# Loading - iloœæ wywiezionych towarów w T
# Unloading iloœæ przywiezionych towarów w T
# PKB w mln E
