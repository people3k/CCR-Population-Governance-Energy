rm(list = ls())
#change to working directory
setwd("")

#Utility Libraries
library(dplyr)

#space-time and analysis libraries
library(sp)
library(IDE)
library(FRK)
library(gstat)
library(spacetime)
library(INLA)
library(margins)
library(ape)

#map and plot libraries
library(ggplot2)
library (MAP)
library(RColorBrewer)


#LOAD DATA
#load dataset - Dataset are already dealing with missing values in Kinship intensity
#that is, the number of observation is reduced based on observations with missing values.
modern <- read.csv("modern.csv")
#polit0 <- read.csv("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/polit_0.csv")
polit1 <- read.csv("polit_1.csv")
polit2 <- read.csv("polit_2.csv")
polit3 <- read.csv("polit_3.csv")

#PREPARE DATA FOR ANALYSIS
#Modern
modmod <- subset(modern, select = c("js_code",  "Year", "nu_lon", "nu_lat", "LnEnergy","LnPopulation","ECI", "KII"))
#rescale so that minimum is 0 and minimum = closed societies
modmod$KII = modmod$KII * - 1 + abs(min(modmod$KII))
modmod$oryear = modmod$Year
modmod$Year = modmod$Year - 1972 #here we start at 1 as the beginning year

#Polity1 - minimally imputed (only 1 missing is imputed)
#Select and redefine variables to avoid too many changes in the analysis
modpol1 <- subset(polit1, select = c("BasePolity", "Period_start", "Longitude", "Latitude", "Temperature", "Polity_territory", "Polity_population", "CC_Infra", "Kinship1"))
modpol1$oryear = modpol1$Period_start
#negative dates can be an issue, so for this purpose we will create other columns to match the names in modern times
modpol1$js_code = modpol1$BasePolity
modpol1$Year = (modpol1$Period_start / 100 +1) + abs(min(modpol1$Period_start /100)) #here we start at 1 as the beginning year
modpol1$LnPopulation = log(modpol1$Polity_population)
modpol1$nu_lon = modpol1$Longitude
modpol1$nu_lat = modpol1$Latitude
modpol1$LnTerritory = log(modpol1$Polity_territory)
modpol1$ECI = modpol1$CC_Infra
#rescale so that minimum is 0 and minimum is closed society
modpol1$KII = (modpol1$Kinship1) + abs(min(modpol1$Kinship1))
modpol1 <- subset(modpol1, select = c("js_code", "Year", "oryear", "nu_lon", "nu_lat", "LnTerritory", "Temperature", "LnPopulation","ECI", "KII"))
#Elimnate observation dated 2100?
na = max(modpol1$Year)
modpol1 = filter(modpol1, Year < na)

#Polity2 - medium imputed (only 2 missing is imputed)
#Select and redefine variables to avoid too many changes in the analysis
modpol2 <- subset(polit2, select = c("BasePolity", "Period_start", "Longitude", "Latitude", "Temperature", "Polity_territory", "Polity_population", "CC_Infra", "Kinship1"))
modpol2$oryear = modpol2$Period_start
#negative dates can be an issue, so for this purpose we will create other columns to match the names in modern times
modpol2$js_code = modpol2$BasePolity
modpol2$Year = (modpol2$Period_start / 100 +1) + abs(min(modpol2$Period_start /100)) #here we start at 1 as the beginning year
modpol2$LnPopulation = log(modpol2$Polity_population)
modpol2$nu_lon = modpol2$Longitude
modpol2$nu_lat = modpol2$Latitude
modpol2$LnTerritory = log(modpol2$Polity_territory)
modpol2$ECI = modpol2$CC_Infra
#rescale so that minimum is 0 and minimum is closed society
modpol2$KII = (modpol2$Kinship1) + abs(min(modpol2$Kinship1))
modpol2 = subset(modpol2, select = c("js_code",  "Year","oryear", "nu_lon", "nu_lat", "LnTerritory", "Temperature","LnPopulation","ECI", "KII"))
#Elimnate observation dated 2100?
na = max(modpol2$Year)
modpol2 = filter(modpol2, Year < na)


#Polity3 - maximally imputed (all 3 missing are imputed)
#Select and redefine variables to avoid too many changes in the analysis
modpol3 <- subset(polit3, select = c("BasePolity", "Period_start", "Longitude", "Latitude", "Temperature", "Polity_territory", "Polity_population", "CC_Infra", "Kinship1"))
modpol3$oryear = modpol3$Period_start
#negative dates can be an issue, so for this purpose we will create other columns to match the names in modern times
modpol3$js_code = modpol3$BasePolity
modpol3$Year = (modpol3$Period_start / 100 +1) + abs(min(modpol3$Period_start /100)) #here we start at 1 as the beginning year
modpol3$LnPopulation = log(modpol3$Polity_population)
modpol3$nu_lon = modpol3$Longitude
modpol3$nu_lat = modpol3$Latitude
modpol3$LnTerritory = log(modpol3$Polity_territory)
modpol3$ECI = modpol3$CC_Infra
#rescale so that minimum is 0 and minimum is closed society
modpol3$KII = (modpol3$Kinship1) + abs(min(modpol3$Kinship1))
modpol3 <- subset(modpol3, select = c("js_code",  "Year","oryear", "nu_lon", "nu_lat", "LnTerritory", "Temperature","LnPopulation","ECI", "KII"))
#Elimnate observation dated 2100?
na = max(modpol3$Year)
modpol3 = filter(modpol3, Year < na)

#Plot Energy and Territory over the years
#set colors
col_scale <- function(palette = "Spectral", name = "", limits = NULL) {
  scale_colour_distiller(palette = palette,   # spectral colour scale
                         guide = "colourbar", # continuous colour bar
                         name = name,
                         limits = limits)
}

#Initial times: 
#Modern = 1973, 
#Polity1 = -1600, 
#Polity2 = -2900, 
#Polity3 = -13600
#Modern
energyplot = ggplot(modmod) + # plot points
  geom_point(aes(x = nu_lon,y = nu_lat, # lon and lat
                 colour = LnEnergy), # attribute color
             size = 1) + # make all points larger
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  col_scale (name = "LnEnergy")+
  facet_wrap(~oryear, ncol=6) + # facet by time
  coord_fixed(xlim = c(-115, 175),
              ylim = c(-45, 65)) 
theme_bw()
print(energyplot)
dev.copy(pdf,'Mod_Energy_History.pdf')
dev.off()

#Polity 1
energyplot = ggplot(modpol1) + # plot points
  geom_point(aes(x = nu_lon,y = nu_lat, # lon and lat
                 colour = LnTerritory), # attribute color
             size = 1) + # make all points larger
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  col_scale (name = "LnTerritory")+
  facet_wrap(~oryear, ncol=6) + # facet by time
  coord_fixed(xlim = c(-115, 175),
              ylim = c(-45, 65)) 
theme_bw()
print(energyplot)
dev.copy(pdf,'Pol1_Energy_History.pdf')
dev.off()

#Polity 2 need to subset or too many. Value chosen to have a 6*7 figure
submodpol2 = subset(modpol2, Year %in% c(round(seq (1, 50, by=1.190476))))
energyplot = ggplot(submodpol2) + # plot points
  geom_point(aes(x = nu_lon,y = nu_lat, # lon and lat
                 colour = LnTerritory), # attribute color
             size = 1) + # make all points larger
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  col_scale (name = "LnTerritory")+
  facet_wrap(~oryear, ncol=6) + # facet by time
  coord_fixed(xlim = c(-115, 175),
              ylim = c(-45, 65)) 
theme_bw()
print(energyplot)
dev.copy(pdf,'Pol2_Energy_History.pdf')
dev.off()

#Polity 3 - need to sub-select or too many. Value chosen to have a 6*7 figure with last and first year 
submodpol3 = subset(modpol3, Year %in% c(round(seq (1, 157, by= 3.8))))
energyplot = ggplot(submodpol3) + # plot points
  geom_point(aes(x = nu_lon,y = nu_lat, # lon and lat
                 colour = LnTerritory), # attribute color
             size = 1) + # make all points larger
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  col_scale (name = "LnTerritory")+
  facet_wrap(~oryear, ncol=6) + # facet by time
  coord_fixed(xlim = c(-115, 175),
              ylim = c(-45, 65)) 
theme_bw()
print(energyplot)
dev.copy(pdf,'Pol3_Energy_History.pdf')
dev.off()

#Hovmoller Plots
#Modern
limlat <- range(modmod$nu_lat) # latitude range
limlon <- range(modmod$nu_lon) # longitude range
limt <- range(modmod$Year) # time range
lataxis <- seq(limlat[1], # latitude axis
               limlat[2],
               length=25)
lonaxis <- seq(limlon[1], # latitude axis
               limlon[2],
               length=25)
taxis <- seq(limt[1], # time axis
             limt[2],
             length=40)
maingridlat <- expand.grid(lat = lataxis,
                           t = taxis)
maingridlon <- expand.grid(lon = lonaxis,
                           t = taxis)
#now associate points to closest in the grid, both in space and time
modgrid <- modmod
distlat <- abs(outer(modmod$nu_lat, lataxis, "-"))
distlon <- abs(outer(modmod$nu_lon, lonaxis, "-"))
modgrid$lat <- lataxis[apply(distlat, 1, which.min)]
modgrid$lon <- lonaxis[apply(distlon, 1, which.min)]

#Summarizefor Lat or Lon Hovemoller plot
modlatHov <- modgrid %>% group_by(lat, oryear) %>% summarise_at(vars(LnEnergy), list(Ln_E = mean))
modlonHov <- modgrid %>% group_by(lon, oryear) %>% summarise_at(vars(LnEnergy), list(Ln_E = mean))

#Plot Latitude Hovemoller
hovlat <- ggplot(modlatHov) + # take data
  geom_tile(aes(x = lat, y = oryear, fill = Ln_E)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +  
  scale_y_reverse() + # rev y scale
  ylab("Year - 0 = 1973") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw()
print(hovlat)
dev.copy(pdf,'Modern_Hovmol_Latitude.pdf')
dev.off()

#Plot Longitude Hovemoller
hovlon <- ggplot(modlonHov) + # take data
  geom_tile(aes(x = lon, y = oryear, fill = Ln_E)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) + 
  scale_y_reverse() + # rev y scale
  ylab("Year - 0 = 1973") + # add y label
  xlab("Longitude (degrees)") + # add x label
  theme_bw()
print(hovlon)
dev.copy(pdf,'Modern_Hovmol_Longitude.pdf')
dev.off()

#Polity 1
limlat <- range(modpol1$nu_lat) # latitude range
limlon <- range(modpol1$nu_lon) # longitude range
limt <- range(modpol1$Year) # time range
lataxis <- seq(limlat[1], # latitude axis
               limlat[2],
               length=25)
lonaxis <- seq(limlon[1], # latitude axis
               limlon[2],
               length=25)
taxis <- seq(limt[1], # time axis
             limt[2],
             length=40)
maingridlat <- expand.grid(lat = lataxis,
                           t = taxis)
maingridlon <- expand.grid(lon = lonaxis,
                           t = taxis)
#now associate points to closest in the grid, both in space and time
modgrid <- modpol1
distlat <- abs(outer(modpol1$nu_lat, lataxis, "-"))
distlon <- abs(outer(modpol1$nu_lon, lonaxis, "-"))
modgrid$lat <- lataxis[apply(distlat, 1, which.min)]
modgrid$lon <- lonaxis[apply(distlon, 1, which.min)]

#Summarizefor Lat or Lon Hovemoller plot
modlatHov <- modgrid %>% group_by(lat, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
modlonHov <- modgrid %>% group_by(lon, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#Plot Latitude Hovemoller 
hovlat <- ggplot(modlatHov) + # take data
  geom_tile(aes(x = lat, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) +  
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -1600") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw()
print(hovlat)
dev.copy(pdf,'Pol1_Hovmol_Latitude.pdf')
dev.off()

#Plot Longitude Hovemoller
hovlon <- ggplot(modlonHov) + # take data
  geom_tile(aes(x = lon, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) + 
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -1600") + # add y label
  xlab("Longitude (degrees)") + # add x label
  theme_bw()
print(hovlon)
dev.copy(pdf,'Pol1_Hovmol_Longitude.pdf')
dev.off()

#Polity 2
limlat <- range(modpol2$nu_lat) # latitude range
limlon <- range(modpol2$nu_lon) # longitude range
limt <- range(modpol2$Year) # time range
lataxis <- seq(limlat[1], # latitude axis
               limlat[2],
               length=25)
lonaxis <- seq(limlon[1], # latitude axis
               limlon[2],
               length=25)
taxis <- seq(limt[1], # time axis
             limt[2],
             length=40)
maingridlat <- expand.grid(lat = lataxis,
                           t = taxis)
maingridlon <- expand.grid(lon = lonaxis,
                           t = taxis)
#now associate points to closest in the grid, both in space and time
modgrid <- modpol2
distlat <- abs(outer(modpol2$nu_lat, lataxis, "-"))
distlon <- abs(outer(modpol2$nu_lon, lonaxis, "-"))
modgrid$lat <- lataxis[apply(distlat, 1, which.min)]
modgrid$lon <- lonaxis[apply(distlon, 1, which.min)]

#Summarizefor Lat or Lon Hovemoller plot
modlatHov <- modgrid %>% group_by(lat, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
modlonHov <- modgrid %>% group_by(lon, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#Plot Latitude Hovemoller 
hovlat <- ggplot(modlatHov) + # take data
  geom_tile(aes(x = lat, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) +  
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -2900") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw()
print(hovlat)
dev.copy(pdf,'Pol2_Hovmol_Latitude.pdf')
dev.off()

#Plot Longitude Hovemoller
hovlon <- ggplot(modlonHov) + # take data
  geom_tile(aes(x = lon, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) + 
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -2900") + # add y label
  xlab("Longitude (degrees)") + # add x label
  theme_bw()
print(hovlon)
dev.copy(pdf,'Pol2_Hovmol_Longitude.pdf')
dev.off()

#Polity 3
limlat <- range(modpol3$nu_lat) # latitude range
limlon <- range(modpol3$nu_lon) # longitude range
limt <- range(modpol3$Year) # time range
lataxis <- seq(limlat[1], # latitude axis
               limlat[2],
               length=25)
lonaxis <- seq(limlon[1], # latitude axis
               limlon[2],
               length=25)
taxis <- seq(limt[1], # time axis
             limt[2],
             length=40)
maingridlat <- expand.grid(lat = lataxis,
                           t = taxis)
maingridlon <- expand.grid(lon = lonaxis,
                           t = taxis)
#now associate points to closest in the grid, both in space and time
modgrid <- modpol3
distlat <- abs(outer(modpol3$nu_lat, lataxis, "-"))
distlon <- abs(outer(modpol3$nu_lon, lonaxis, "-"))
modgrid$lat <- lataxis[apply(distlat, 1, which.min)]
modgrid$lon <- lonaxis[apply(distlon, 1, which.min)]

#Summarizefor Lat or Lon Hovemoller plot
modlatHov <- modgrid %>% group_by(lat, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
modlonHov <- modgrid %>% group_by(lon, oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#Plot Latitude Hovemoller 
hovlat <- ggplot(modlatHov) + # take data
  geom_tile(aes(x = lat, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) +  
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -13600") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw()
print(hovlat)
dev.copy(pdf,'Pol3_Hovmol_Latitude.pdf')
dev.off()

#Plot Longitude Hovemoller
hovlon <- ggplot(modlonHov) + # take data
  geom_tile(aes(x = lon, y = oryear, fill = Ln_T)) + # plot
  scale_fill_gradientn(colours = colorspace::diverge_hcl(11)) + 
  scale_y_reverse() + # rev y scale
  ylab("Century - 0 = -13600") + # add y label
  xlab("Longitude (degrees)") + # add x label
  theme_bw()
print(hovlon)
dev.copy(pdf,'Pol3_Hovmol_Longitude.pdf')
dev.off()


#Scatter and Trend Plots
#Modern
#Find empirical spatial and temporal mean 
spaceavg <- modmod %>% group_by(nu_lat, nu_lon) %>% summarise_at(vars(LnEnergy), list(Ln_E = mean))
timeavg <- modmod %>% group_by(oryear) %>% summarise_at(vars(LnEnergy), list(Ln_E = mean))
#plot energy over latitude
lat_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lat, Ln_E)) +
  xlab("Latitude (deg)") +
  ylab("Ln Energy") + theme_bw()
print(lat_means)
dev.copy(pdf,'Modern_Scatter_Ene_Lat.pdf')
dev.off()
#plot energy over longitude
lon_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lon, Ln_E)) +
  xlab("Longitude (deg)") +
  ylab("Ln Energy") + theme_bw()
print(lon_means)
dev.copy(pdf,'Modern_Scatter_Ene_Lon.pdf')
dev.off()
#plot energy over time 
time_means <-
  ggplot() +
  geom_line(data = modmod,aes(x = oryear, y = LnEnergy, group = js_code),
            colour = "blue", alpha = 0.1) +
  geom_line(data = timeavg, aes(x = oryear, y = Ln_E)) +
  xlab("Year") + ylab("Ln Energy") +
  theme_bw()
print(time_means)
dev.copy(pdf,'Modern_Scatter_Ene_Time.pdf')
dev.off()


#Polity 1
#Find empirical spatial and temporal mean 
spaceavg <- modpol1 %>% group_by(nu_lat, nu_lon) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
timeavg <- modpol1 %>% group_by(oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#plot energy over latitude
lat_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lat, Ln_T)) +
  xlab("Latitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lat_means)
dev.copy(pdf,'Pol1_Scatter_Ene_Lat.pdf')
dev.off()
#plot energy over longitude
lon_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lon, Ln_T)) +
  xlab("Longitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lon_means)
dev.copy(pdf,'Pol1_Scatter_Ene_Lon.pdf')
dev.off()
#plot energy over time 
time_means <-
  ggplot() +
  geom_line(data = modpol1,aes(x = oryear, y = LnTerritory, group = js_code),
            colour = "blue", alpha = 0.2) +
  geom_line(data = timeavg, aes(x = oryear, y = Ln_T)) +
  xlab("Century - 0 = -1600") + ylab("Ln Territory") +
  theme_bw()
print(time_means)
dev.copy(pdf,'Pol1_Scatter_Ene_Time.pdf')
dev.off()

#Polity 2
#Find empirical spatial and temporal mean 
spaceavg <- modpol2 %>% group_by(nu_lat, nu_lon) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
timeavg <- modpol2 %>% group_by(oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#plot energy over latitude
lat_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lat, Ln_T)) +
  xlab("Latitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lat_means)
dev.copy(pdf,'Pol2_Scatter_Ene_Lat.pdf')
dev.off()
#plot energy over longitude
lon_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lon, Ln_T)) +
  xlab("Longitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lon_means)
dev.copy(pdf,'Pol2_Scatter_Ene_Lon.pdf')
dev.off()
#plot energy over time 
time_means <-
  ggplot() +
  geom_line(data = modpol2,aes(x = oryear, y = LnTerritory, group = js_code),
            colour = "blue", alpha = 0.2) +
  geom_line(data = timeavg, aes(x = oryear, y = Ln_T)) +
  xlab("Century - 0 = -2900") + ylab("Ln Territory") +
  theme_bw()
print(time_means)
dev.copy(pdf,'Pol2_Scatter_Ene_Time.pdf')
dev.off()

#Polity 3
#Find empirical spatial and temporal mean 
spaceavg <- modpol3 %>% group_by(nu_lat, nu_lon) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
timeavg <- modpol3 %>% group_by(oryear) %>% summarise_at(vars(LnTerritory), list(Ln_T = mean))
#plot energy over latitude
lat_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lat, Ln_T)) +
  xlab("Latitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lat_means)
dev.copy(pdf,'Pol3_Scatter_Ene_Lat.pdf')
dev.off()
#plot energy over longitude
lon_means <- ggplot(spaceavg) +
  geom_point(aes(nu_lon, Ln_T)) +
  xlab("Longitude (deg)") +
  ylab("Ln Territory") + theme_bw()
print(lon_means)
dev.copy(pdf,'Pol3_Scatter_Ene_Lon.pdf')
dev.off()
#plot energy over time 
time_means <-
  ggplot() +
  geom_line(data = modpol3,aes(x = oryear, y = LnTerritory, group = js_code),
            colour = "blue", alpha = 0.2) +
  geom_line(data = timeavg, aes(x = oryear, y = Ln_T)) +
  xlab("Century - 0 = -13600") + ylab("Ln Territory") +
  theme_bw()
print(time_means)
dev.copy(pdf,'Pol3_Scatter_Ene_Time.pdf')
dev.off()


#Create Space-Time Objects #note that due to scale, we abstract time from years and centuries to days.
#Works easier for space-time objects
#Modern
space = SpatialPoints(coords = modmod[, c("nu_lon", "nu_lat")]) 
#now create a space-time object
stobjmod = spacetime::STIDF(sp=space, 
                         time = as.Date(modmod$Year, origin = "1-1-1000"),
                         data=dplyr::select(modmod, -Year, - nu_lon, -nu_lat))
proj4string(stobjmod) <- CRS("+proj=longlat +ellps=WGS84")

#Polity 1
space = SpatialPoints(coords = modpol1[, c("nu_lon", "nu_lat")]) 
#now create a space-time object
stobjpol1 = spacetime::STIDF(sp=space, 
                               time = as.Date(modpol1$Year, origin = "1-1-1900"),
                               data=dplyr::select(modpol1, -Year, - nu_lon, -nu_lat))
proj4string(stobjpol1) <- CRS("+proj=longlat +ellps=WGS84") #define coordinate type

#Polity 2
space = SpatialPoints(coords = modpol2[, c("nu_lon", "nu_lat")]) 
#now create a space-time object
stobjpol2 = spacetime::STIDF(sp=space, 
                               time = as.Date(modpol2$Year, origin = "1-1-1001"),
                               data=dplyr::select(modpol2, -Year, - nu_lon, -nu_lat))
proj4string(stobjpol2) <- CRS("+proj=longlat +ellps=WGS84") #define coordinate type

#Polity 3
space = SpatialPoints(coords = modpol3[, c("nu_lon", "nu_lat")]) 
#now create a space-time object
stobjpol3 = spacetime::STIDF(sp=space, 
                               time = as.Date(modpol3$Year, origin = "1-1-1000"),
                               data=dplyr::select(modpol3, -Year, - nu_lon, -nu_lat))
#define coordinate type
proj4string(stobjpol3) <- CRS("+proj=longlat +ellps=WGS84") #define coordinate type


#Calculate Variograms

#Time 10 time lags, corresponding to 10 years in modern and 1000 years in polity data
#We calculate the variograms one time and save them as it takes a bit of time to do.
# vvmod <- variogramST(formula = LnEnergy ~ 1 + (LnPopulation + KII)^2 + ECI ,
#                   data = stobjmod, tunit = "days",
#                   width = 1000, # spatial bin (1000 km)
#                   cutoff = 10000, # consider pts < 10000 km apart
#                   tlags = 0:10, # up to 10 time-lags to be considered
#                   cores = getOption("mc.cores", 5))
# saveRDS(vvmod, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvmod")
# 
# vvpol1 <- variogramST(formula = LnTerritory ~ 1 + (LnPopulation + KII)^2 + Temperature + ECI ,
#                      data = stobjpol1,
#                      width = 1000, tunit = "days", # spatial bin (1000 km)
#                      cutoff = 10000, # consider pts < 10000 km apart
#                      tlags = 0:10, # up to 10 time-lags to be considered
#                      cores = getOption("mc.cores", 5))
# saveRDS(vvpol1, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol1")
# 
# vvpol2 <- variogramST(formula = LnTerritory ~ 1 + (LnPopulation + KII)^2 + Temperature + ECI ,
#                      data = stobjpol2, tunit = "days",
#                      width = 1000, # spatial bin (1000 km)
#                      cutoff = 10000, # consider pts < 10000 km apart
#                      tlags = 0:10, # up to 10 time-lags to be considered
#                      cores = getOption("mc.cores", 5))
# saveRDS(vvpol2, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol2")
# 
# vvpol3 <- variogramST(formula = LnTerritory ~ 1 + (LnPopulation + KII)^2 + Temperature + ECI ,
#                       data = stobjpol3, tunit = "days",
#                       width = 1000, # spatial bin (1000 km)
#                       cutoff = 10000, # consider pts < 10000 km apart
#                       tlags = seq(0, 10, by=1), # up to 10 time-lags to be considered
#                       cores = getOption("mc.cores", 5))
#saveRDS(vvpol3, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol3")


#Load the variograms previously calculated 
vvmod  = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvmod")
vvpol1 = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol1")
vvpol2 = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol2")
vvpol3 = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol3")

#Plot variograms
plot(vvmod, ylab = "Time Lags (Years)", xlab = "Distance (Km)")
dev.copy(pdf,'Modern_Variogram.pdf')
dev.off()
plot(vvmod, map = FALSE, xlab = "Distance (Km)" )
dev.copy(pdf,'Modern_Line_Variogam.pdf')
dev.off()

plot(vvpol1, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol1t_Variogram.pdf')
dev.off()
plot(vvpol1, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol1t_Line_Variogam.pdf')
dev.off()

plot(vvpol2, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol2t_Variogram.pdf')
dev.off()
plot(vvpol2, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol2t_Line_Variogam.pdf')
dev.off()

plot(vvpol3, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol3t_Variogram.pdf')
dev.off()
plot(vvpol3, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol3t_Line_Variogam.pdf')
dev.off()


#Statistical analysis, using GLS to account for space-time error dependence
#First calculate and evaluate basis functions to add as covariates. They are basically
#fixed effects

#Modern
basisN <- auto_basis(data = modmod[,c("nu_lon","nu_lat")] %>% # Take Tmax
                       SpatialPoints(), # To sp obj
                     regular = 0,
                     nres = 1, # One resolution
                     type = "Gaussian") # multiple types
show_basis(basisN)
evg <- eval_basis(basis = basisN, # basis functions
                  s = modmod[,c("nu_lon","nu_lat")] %>% # spat locations
                    as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(evg) <- paste0("B", 1:ncol(evg)) # assign column names
amodmod <- cbind(modmod, evg) 

#Polity1
basisN <- auto_basis(data = modpol1[,c("nu_lon","nu_lat")] %>% # Take Tmax
                       SpatialPoints(), # To sp obj
                     regular = 0,
                     nres = 1, # One resolution
                     type = "Gaussian") # multiple types
show_basis(basisN)
evg <- eval_basis(basis = basisN, # basis functions
                  s = modpol1[,c("nu_lon","nu_lat")] %>% # spat locations
                    as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(evg) <- paste0("B", 1:ncol(evg)) # assign column names
amodpol1 <- cbind(modpol1, evg)

#Polity2 
basisN <- auto_basis(data = modpol2[,c("nu_lon","nu_lat")] %>% # Take Tmax
                       SpatialPoints(), # To sp obj
                     regular = 0,
                     nres = 1, # One resolution
                     type = "Gaussian") # multiple types
show_basis(basisN)
evg <- eval_basis(basis = basisN, # basis functions
                  s = modpol2[,c("nu_lon","nu_lat")] %>% # spat locations
                    as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(evg) <- paste0("B", 1:ncol(evg)) # assign column names
amodpol2 <- cbind(modpol2, evg) 

#Polity3
basisN <- auto_basis(data = modpol2[,c("nu_lon","nu_lat")] %>% # Take Tmax
                       SpatialPoints(), # To sp obj
                     regular = 0,
                     nres = 1, # One resolution
                     type = "Gaussian") # multiple types
show_basis(basisN)
evg <- eval_basis(basis = basisN, # basis functions
                  s = modpol3[,c("nu_lon","nu_lat")] %>% # spat locations
                    as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(evg) <- paste0("B", 1:ncol(evg)) # assign column names
amodpol3 <- cbind(modpol3, evg)

#Perform a simple linear model
modlm = lm(LnEnergy ~ (nu_lon + nu_lat + Year)^2 + ECI + (KII + LnPopulation)^2 + ., # model
           data = amodmod %>% select( -js_code, -oryear)) # omit id
pol1lm = lm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + ., # model
           data = amodpol1 %>% select( -js_code, -oryear)) # omit id
pol2lm = lm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + ., # model
           data = amodpol2 %>% select( -js_code, -oryear)) # omit id
pol3lm = lm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + ., # model
           data = amodpol3 %>% select( -js_code, -oryear)) # omit id

modlm %>% summary
pol1lm %>% summary
pol2lm %>% summary
pol3lm %>% summary


#Plot marginal effects
#Marginal Effects
cols <- brewer.pal(11, "Spectral")
image(modlm, "KII", "LnPopulation", col = cols, xlab = "Social Inclusion")
dev.copy(pdf,'Modern_MarginalEffect.pdf')
dev.off()
image(pol1lm, "KII", "LnPopulation", col = cols, xlab = "Social Inclusion")
dev.copy(pdf,'Pol1_MarginalEffect.pdf')
dev.off()
image(pol2lm, "KII", "LnPopulation", col = cols, xlab = "Social Inclusion")
dev.copy(pdf,'Pol2_MarginalEffect.pdf')
dev.off()
image(pol3lm, "KII", "LnPopulation", col = cols, xlab = "Social Inclusion")
dev.copy(pdf,'Pol3_MarginalEffect.pdf')
dev.off()

image(modlm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Modern_LatYear_MarginalEffect.pdf')
dev.off()
image(pol1lm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Pol1_LatYear_MarginalEffect.pdf')
dev.off()
image(pol2lm, "Year", "nu_lat", col = cols, ylab = "Latitude" )
dev.copy(pdf,'Pol2_LatYear_MarginalEffect.pdf')
dev.off()
image(pol3lm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Pol3_LatYear_MarginalEffect.pdf')
dev.off()


#get model residuals
resmod = modlm$residuals
respol1 = pol1lm$residuals
respol2 = pol2lm$residuals
respol3 = pol3lm$residuals

amodmod$residuals = modlm$residuals
amodpol1$residuals = pol1lm$residuals
amodpol2$residuals = pol2lm$residuals
amodpol3$residuals = pol3lm$residuals

#Moran Test for SpatioTemporal data, 
#std error if correlated, than estimates are ok, but std are not
#Modern
dist2 <- amodmod %>% # take the data
  select(nu_lon, nu_lat, Year) %>% # extract coordinates
  dist() %>% # compute distances
  as.matrix() # convert to matrix
distinv <- 1/dist2
diag(distinv) <- 0
#make sure we have no INV values
distinv[is.infinite(distinv)] <- 0
Moran.I(amodmod$residuals, distinv)$p.value

#Polity 1
dist2 <- amodpol1 %>% # take the data
  select(nu_lon, nu_lat, Year) %>% # extract coordinates
  dist() %>% # compute distances
  as.matrix() # convert to matrix
distinv <- 1/dist2
diag(distinv) <- 0
#make sure we have no INV values
distinv[is.infinite(distinv)] <- 0
Moran.I(amodpol1$residuals, distinv)$p.value

#Polity 2
dist2 <- amodpol2 %>% # take the data
  select(nu_lon, nu_lat, Year) %>% # extract coordinates
  dist() %>% # compute distances
  as.matrix() # convert to matrix
distinv <- 1/dist2
diag(distinv) <- 0
#make sure we have no INV values
distinv[is.infinite(distinv)] <- 0
Moran.I(amodpol2$residuals, distinv)$p.value

#Polity 3
dist2 <- amodpol3 %>% # take the data
  select(nu_lon, nu_lat, Year) %>% # extract coordinates
  dist() %>% # compute distances
  as.matrix() # convert to matrix
distinv <- 1/dist2
diag(distinv) <- 0
#make sure we have no INV values
distinv[is.infinite(distinv)] <- 0
Moran.I(amodpol3$residuals, distinv)$p.value

amodmod = select(amodmod,-residuals)
amodpol1 = select(amodpol1,-residuals)
amodpol2 = select(amodpol2,-residuals)
amodpol3 = select(amodpol3,-residuals)

#####Do GLM better for dependence of errors as Moran's I suggests residual correlation
modglm = glm(LnEnergy ~ (nu_lon + nu_lat + Year)^2 + ECI + (KII + LnPopulation)^2 + .,
        data = amodmod %>% select( -js_code, -oryear))
pol1glm = glm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + .,
    data = amodpol1 %>% select( -js_code, -oryear))
pol2glm = glm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + .,
    data = amodpol2 %>% select( -js_code, -oryear))
pol3glm = glm(LnTerritory ~ (nu_lon + nu_lat + Year)^2 + Temperature + ECI + (KII + LnPopulation)^2 + ., 
    data = amodpol3 %>% select( -js_code, -oryear))

#check results
modglm %>% summary
pol1glm %>% summary
pol2glm %>% summary
pol3glm %>% summary

#Plot marginal effects
#Marginal Effects
cols <- brewer.pal(11, "Spectral")
image(modglm, "KII", "LnPopulation", col = cols, xlab = "Social Order")
dev.copy(pdf,'Modern_GLMMarginalEffect.pdf')
dev.off()
image(pol1glm, "KII", "LnPopulation", col = cols, xlab = "Social Order")
dev.copy(pdf,'Pol1_GLMMarginalEffect.pdf')
dev.off()
image(pol2glm, "KII", "LnPopulation", col = cols, xlab = "Social Order")
dev.copy(pdf,'Pol2_GLMMarginalEffect.pdf')
dev.off()
image(pol3glm, "KII", "LnPopulation", col = cols, xlab = "Social Order")
dev.copy(pdf,'Pol3_GLMMarginalEffect.pdf')
dev.off()

image(modglm, "ECI", "LnPopulation", col = cols, xlab = "Complexity")
dev.copy(pdf,'Modern_GLMComplexityPop.pdf')
dev.off()
image(pol1glm, "ECI", "LnPopulation", col = cols, xlab = "Complexity")
dev.copy(pdf,'Pol1_GLMComplexityPop.pdf')
dev.off()
image(pol2glm, "ECI", "LnPopulation", col = cols,xlab = "Complexity" )
dev.copy(pdf,'Pol2_GLMComplexityPop.pdf')
dev.off()
image(pol3glm, "ECI", "LnPopulation", col = cols, xlab = "Complexity")
dev.copy(pdf,'Pol3_GLMComplexityPop.pdf')
dev.off()

image(modglm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Modern_GLMLatYear_MarginalEffect.pdf')
dev.off()
image(pol1glm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Pol1_GLMLatYear_MarginalEffect.pdf')
dev.off()
image(pol2glm, "Year", "nu_lat", col = cols,ylab = "Latitude" )
dev.copy(pdf,'Pol2_GLMLatYear_MarginalEffect.pdf')
dev.off()
image(pol3glm, "Year", "nu_lat", col = cols, ylab = "Latitude")
dev.copy(pdf,'Pol3_GLMLatYear_MarginalEffect.pdf')
dev.off()



gresmod = modlm$residuals
grespol1 = pol1glm$residuals
grespol2 = pol2glm$residuals
grespol3 = pol3glm$residuals

amodmod$gresiduals = modglm$residuals
amodpol1$gresiduals = pol1glm$residuals
amodpol2$gresiduals = pol2glm$residuals
amodpol3$gresiduals = pol3glm$residuals

#Calculate Variograms on residuals for the GLM
#Add residuals to the data of the st objects
stobjmod@data = left_join(stobjmod@data, amodmod)
stobjpol1@data = left_join(stobjpol1@data, amodpol1)
stobjpol2@data = left_join(stobjpol2@data, amodpol2)
stobjpol3@data = left_join(stobjpol3@data, amodpol3)

#Finally, calculate the variograms for the residuals of the GLM models  one time and save them as it takes a bit of time to do.
vvmodres <- variogramST(formula = gresiduals ~ 1,
                   data = stobjmod, tunit = "days",
                   width = 1000, # spatial bin (1000 km)
                   cutoff = 10000, # consider pts < 10000 km apart
                   tlags = 0:10, # up to 10 time-lags to be considered
                   cores = getOption("mc.cores", 5))
saveRDS(vvmodres, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvmodres")

vvpol1res <- variogramST(formula = gresiduals ~ 1,
                      data = stobjpol1,
                      width = 1000, tunit = "days", # spatial bin (1000 km)
                      cutoff = 10000, # consider pts < 10000 km apart
                      tlags = 0:10, # up to 10 time-lags to be considered
                      cores = getOption("mc.cores", 5))
saveRDS(vvpol1res, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol1res")

vvpol2res <- variogramST(formula = gresiduals ~ 1,
                      data = stobjpol2, tunit = "days",
                      width = 1000, # spatial bin (1000 km)
                      cutoff = 10000, # consider pts < 10000 km apart
                      tlags = 0:10, # up to 10 time-lags to be considered
                      cores = getOption("mc.cores", 5))
saveRDS(vvpol2res, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol2res")

vvpol3res <- variogramST(formula = gresiduals ~ 1,
                      data = stobjpol3, tunit = "days",
                      width = 1000, # spatial bin (1000 km)
                      cutoff = 10000, # consider pts < 10000 km apart
                      tlags = seq(0, 10, by=1), # up to 10 time-lags to be considered
                      cores = getOption("mc.cores", 5))
saveRDS(vvpol3res, file = "/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol3res")


#Load the variograms previously calculated 
vvmodres  = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvmodres")
vvpol1res = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol1res")
vvpol2res = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol2res")
vvpol3res = readRDS("/Users/jacapobaggio/Documents/A_STUDI/AAA_Work/A_Freeman/Infrastructure-Territory/Data/vvpol3res")

#Plot variograms
plot(vvmodres, ylab = "Time Lags (Years)", xlab = "Distance (Km)")
dev.copy(pdf,'Modern_Res_Variogram.pdf')
dev.off()
plot(vvmodres, map = FALSE, xlab = "Distance (Km)" )
dev.copy(pdf,'Modern_Res_Line_Variogam.pdf')
dev.off()

plot(vvpol1res, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol1t_Res_Variogram.pdf')
dev.off()
plot(vvpol1res, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol1t_Res_Line_Variogam.pdf')
dev.off()

plot(vvpol2res, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol2t_Res_Variogram.pdf')
dev.off()
plot(vvpol2res, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol2t_Res_Line_Variogam.pdf')
dev.off()

plot(vvpol3res, ylab = "Time Lags (Centuries)", xlab = "Distance (Km)")
dev.copy(pdf,'Pol3t_Res_Variogram.pdf')
dev.off()
plot(vvpol3res, map = FALSE, xlab = "Distance (Km)")
dev.copy(pdf,'Pol3t_Res_Line_Variogam.pdf')
dev.off()

library(jtools)
set_summ_defaults(digits = 5)

lmglm = export_summs(modlm,modglm, pol1lm, pol1glm, pol2lm, pol2glm, pol3lm, pol3glm, scale = TRUE,  error_format = "[{conf.low}, {conf.high}]")
export_summs(modlm,modglm, pol1lm, pol1glm, pol2lm, pol2glm, pol3lm, pol3glm, scale = FALSE, model.names = c("LM  Modern", "GLM Modern", "LM  Polity 1", "GLM Polity 1", "LM  Polity 2", "GLM Polity 2", "LM  Polity 3", "GLM Polity 3"))


#Using average values for Modern Countries for GLM Basis Functions Model
avgmodmod<-modmod %>% group_by(js_code) %>% mutate(avgene = mean(LnEnergy), avgpop = mean(LnPopulation), avgeci=mean(ECI))

avgmodmod = aggregate(x = modmod[c("nu_lat", "nu_lon", "LnEnergy","LnPopulation","ECI","KII")], by = modmod[c("js_code")], FUN = mean)

#Modern
basisN <- auto_basis(data = avgmodmod[,c("nu_lon","nu_lat")] %>% # Take Tmax
                       SpatialPoints(), # To sp obj
                     regular = 0,
                     nres = 1, # One resolution
                     type = "Gaussian", 
                     tunit = 'days') # multiple types
show_basis(basisN)
evg <- eval_basis(basis = basisN, # basis functions
                  s = avgmodmod[,c("nu_lon","nu_lat")] %>% # spat locations
                    as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(evg) <- paste0("B", 1:ncol(evg)) # assign column names
aavgmodmod <- cbind(avgmodmod, evg) 

avgmod = glm(LnEnergy ~ (nu_lon + nu_lat)^2 + ECI + (KII + LnPopulation)^2 + .,
             data = aavgmodmod %>% select( -js_code))

summary(avgmod)             
avgmodglm = export_summs(avgmod, scale = FALSE,  error_format = "[{conf.low}, {conf.high}]")

image(avgmod, "KII", "LnPopulation", col = cols, xlab = "Inclusiveness")
dev.copy(pdf,'Marginal_modernAvg.pdf')
dev.off()