# Extracting management history for plot locations from Natural Resources Database
# 3 April, 2019
# Contact: Christine Rollinson, crollinson@mortonarb.org

# Libraries
library(raster); library(rgdal); library(rgeos); library(maps)
library(lubridate)
library(ggplot2)

# File paths
path.ew <- "../EastWoods-MonitoringPlots/plot_selection/" # This is a different github repository
google.gis <- "/Volumes/GoogleDrive/My Drive/East Woods/GIS_files" 
dir.terrain <- file.path(path.ew, "../EastWoods_GIS")
path.gis <- "/Volumes/GIS/"
path.out <- "/Volumes/GoogleDrive/My Drive/OakEcosystems_Fire/data_raw/"

# The OER plot locations
site.dat <- read.csv("../data_raw/OER-Fire_SiteCoords.csv")
oer.tma <- site.dat[site.dat$Site=="MortonArb",]
oer.tma

# Converting to a spatial file
oer.tma.sp <- SpatialPointsDataFrame(coords=oer.tma[,c("Long", "Lat")], oer.tma[,c("Site", "Long", "Lat")], proj4string=CRS("+proj=longlat"))

# A quick & dirty plot to double check things 
#  (ggplot can make it a lot prettier but requires extra steps)
# plot(oer.tma.sp, pch=19, cex=0.5, col="blue")



dem <- raster(file.path(google.gis, "DEMs/ewoods/")) # Elevation looks like its in feet
oer.ll <- spTransform(oer.tma.sp, CRS("+proj=longlat"))
oer.utm16 <- spTransform(oer.tma.sp, projection(dem))
oer.df <- data.frame(oer.tma,
                     x.nad83=coordinates(oer.tma.sp)[,1], y.nad83=coordinates(oer.tma.sp)[,2],
                     x.utm16=coordinates(oer.utm16)[,1], y.utm16=coordinates(oer.utm16)[,2],
                     point.ID=1:nrow(oer.tma.sp))
oer.df <- droplevels(oer.df) # just getting rid of remnant tags
summary(oer.df)


# Topography
dem <- raster(file.path(google.gis, "DEMs/ewoods/")) # Elevation looks like its in feet
# pre-calculated additional terrain variables
slope <- raster(file.path(google.gis, "topography_CR/eastwoods_slope"))
aspect <- raster(file.path(google.gis, "topography_CR/eastwoods_aspect"))
tpi <- raster(file.path(google.gis, "topography_CR/eastwoods_tpi"))


oer.df$elev   <- extract(dem   , spTransform(oer.tma.sp, projection(dem)))
oer.df$slope  <- extract(slope , spTransform(oer.tma.sp, projection(dem)))
oer.df$aspect <- extract(aspect, spTransform(oer.tma.sp, projection(dem)))
oer.df$tpi    <- extract(tpi   , spTransform(oer.tma.sp, projection(dem)))
summary(oer.df)



# # Soils 
soil_type <- readOGR("/Volumes/GIS/Collections/soils/soil_types.shp")
plot(soil_type)
summary(soil_type)

oer.soils <- extract(soil_type, spTransform(oer.tma.sp, projection(soil_type)))
oer.soils <- merge(oer.soils, oer.df[,c("point.ID", "PlotID")])
summary(oer.soils)
dim(oer.soils)


# -------------------------------
# Getting Burn Info
# -------------------------------
path.burn <- "/Volumes/GIS/Collections/MarKGIS/Management Unit Plans/Controlled Burn Areas.gdb"
burn.layers <- ogrListLayers(path.burn)

burn <- readOGR(file.path(path.burn), "Completed_Burn_Areas")
burn$Burn_Date <- as.Date(burn$Burn_Date)
burn$Year <- lubridate::year(burn$Burn_Date)
burn$Month <- lubridate::month(burn$Burn_Date)
burn$season <- as.factor(ifelse(burn$Month<7, "spring", "fall"))
burn[grep("2004", burn$NOTES),"Year"] <- 2004
burn[grep("2005", burn$NOTES),"Year"] <- 2005
burn[grep("2010", burn$NOTES),"Year"] <- 2010
burn[grep("2011", burn$NOTES),"Year"] <- 2011
burn[grep("2013", burn$NOTES),"Year"] <- 2013
burn[grep("Pizzo Burn 2008", burn$NOTES),"Year"] <- 2008
burn[grep("Pizzo Burn 2006", burn$NOTES),"Year"] <- 2006
burn[grep("SPRING", toupper(burn$NOTES)),"season"] <- "spring"
burn[grep("FALL", toupper(burn$NOTES)),"season"] <- "fall"
burn$Year <- as.numeric(burn$Year)
burn[burn$NOTES==" ","NOTES"] <- NA
summary(burn)

# Checking things with no burn year 
plot(burn)
plot(burn[is.na(burn$Year),])
noburn <- data.frame(burn[is.na(burn$Year),"NOTES"])
paste(noburn[,1])

burn <- spTransform(burn, projection(oer.tma.sp))
summary(burn)
dim(burn)

data.frame(burn[burn$Year==2013 & !is.na(burn$Year) & is.na(burn$Burn_Date),])
plot(burn[burn$Year==2013 & !is.na(burn$Year) & is.na(burn$Burn_Date),])

oer.burn <- extract(burn[!is.na(burn$Year),], oer.tma.sp)
oer.burn <- merge(oer.burn, oer.df[,c("point.ID", "PlotID", "Long", "Lat", "PlotName", "Treatment", "burned", "thinned")], all=T)
summary(oer.burn[is.na(oer.burn$Year),])
# summary(oer.burn[oer.burn$Year==2013 & !is.na(oer.burn$Year),])
summary(oer.burn)
dim(oer.burn)

burn.df2 <- aggregate(oer.burn[!is.na(oer.burn$Burn_Date),c("Burn_Date")], by=oer.burn[!is.na(oer.burn$Burn_Date),c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned", "Burn_Date", "Year", "Month", "season")], FUN=length)
names(burn.df2)[which(names(burn.df2)=="x")] <- "n.entry"
burn.df2$Acres <- aggregate(oer.burn[!is.na(oer.burn$Burn_Date),c("Acres")], by=oer.burn[!is.na(oer.burn$Burn_Date),c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned", "Burn_Date", "Year", "Month", "season")], FUN=mean)[,"x"]
summary(burn.df2)
# summary(burn.df2[burn.df2$n.entry>1,])
burn.df2[burn.df2$Acres==0,]

burn.yr  <- aggregate(burn.df2[,c("Burn_Date")], by=burn.df2[,c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned", "Year", "season")], FUN=length)
summary(burn.yr)

burn.sum  <- aggregate(burn.df2[,c("Burn_Date")], by=burn.df2[,c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned")], FUN=length)
names(burn.sum)[which(names(burn.sum)=="x")] <- "burn.n"
burn.sum$burn.first  <- aggregate(burn.df2[,c("Year")], by=burn.df2[,c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned")], FUN=min)[,"x"]
burn.sum$burn.last  <- aggregate(burn.df2[,c("Year")], by=burn.df2[,c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned")], FUN=max)[,"x"]
burn.sum <- burn.sum[!is.na(burn.sum$PlotName),]
burn.sum <- merge(burn.sum, oer.df[,c("PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned")], all=T) # Getting back in plots with no burn
burn.sum[is.na(burn.sum$burn.n), "burn.n"] <- 0
burn.sum

pdf(file.path(path.out, "MortonArb_Burn_Summaries.pdf"))
ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=Long, y=Lat, color=burn.n)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()

ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=Long, y=Lat, color=burn.first)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()

ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=Long, y=Lat, color=burn.last)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()
dev.off()
# -------------------------------


# -------------------------------
# Getting thinning info
# -------------------------------
path.thin <- "/Volumes/GIS/Collections/Natural Resources Management/Winter Clearing/Winter Clearing.gdb"
thin.layers <- ogrListLayers(path.thin)

# Shape files with areas (yay!)
thin.nr <- readOGR(path.thin, "Winter_Clearing")
thin.nr$Year.Finish <- as.numeric(paste0(substr(thin.nr$Year,1,2), substr(thin.nr$Year,nchar(paste(thin.nr$Year))-1,nchar(paste(thin.nr$Year)))))
summary(thin.nr)

# Point-based havests (boo!) -- we don't know much about these sizes, so we'll have to see what we can do
thin.bob <- readOGR(path.thin, "Bob_Fahey_Research")
thin.res <- readOGR(path.thin, "Winter_Thinning_06_07_Subplots")
summary(thin.bob) # Note that some of bob's points might be marked wrong
summary(thin.res)

thin.nr <- spTransform(thin.nr, projection(oer.tma.sp))
thin.bob <- spTransform(thin.bob, projection(oer.tma.sp))
thin.res <- spTransform(thin.res, projection(oer.tma.sp))
summary(thin.nr)
dim(thin.nr)

oer.thin.nr <- extract(thin.nr[,], oer.tma.sp)
oer.thin.nr <- merge(oer.thin.nr, oer.df[,c("point.ID", "PlotID", "Lat", "Long", "PlotName", "Treatment", "burned", "thinned")], all=T)
summary(oer.thin.nr)

# Note: Got rid of Bob's thins

pdf(file.path(path.out, "MortonArb_Thin_Summaries_NatResources_Only.pdf"))
ggplot(data=oer.thin.nr) +
  ggtitle("Natural Resources Thin") +
  coord_equal() +
  geom_point(aes(x=Long, y=Lat, color=Year.Finish)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()
dev.off()


# -------------------------------

# summary(oer.df)

# summary(harvest)
# mgmt  <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Boundaries/New Management Units.shp")
# harvest <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Canopy Thinning/Canopy Thinning.shp")

# oer.mgmt <- extract(mgmt, oer.tma.sp)
# oer.mgmt$point.ID <- as.factor(oer.mgmt$point.ID)
# summary(oer.mgmt)

# oer.df$AreaName <- car::recode(oer.mgmt$East_West, "'East Side'='East Woods'; 'Hidden Lake Forest Preser'='Hidden Lake'")
# oer.df$AreaName2 <- oer.mgmt$CommonName
# oer.df$MgmtUnit <- oer.mgmt$UnitNumber
# oer.df$MgmtUnitArea <- oer.mgmt$Acres
# oer.df$ComClass <- oer.mgmt$ComClass
# summary(oer.df)

# # oer.harvest <- over(oer.tma.sp, harvest)
# oer.harvest <- over(oer.tma.sp, harvest)
# summary(oer.harvest)
# dim(oer.harvest)

# oer.df$CanopyHarvest <- oer.harvest$Year

# oer.burn <- extract(burn, oer.tma.sp)
# oer.burn <- merge(oer.burn, oer.df[,c("point.ID", "PlotID")])
# summary(oer.burn)
# 
# 
# dim(oer.burn)


# Save all the info except the burn history
# Save to Github
write.csv(oer.df, "../data_raw/OER_MortonArb_point_info_GIS.csv", row.names=F)
write.csv(oer.burn, "../data_raw/OER_MortonArb_point_info_GIS_burnhistory.csv", row.names=F)
write.csv(oer.thin.nr, "../data_raw/OER_MortonArb_point_info_GIS_thinning_natres.csv", row.names=F)

# Save to Google Drive
write.csv(oer.df, file.path(path.out, "OER_MortonArb_point_info_GIS.csv"), row.names=F)
write.csv(oer.burn, file.path(path.out, "OER_MortonArb_point_info_GIS_burnhistory.csv"), row.names=F)
write.csv(oer.thin.nr, file.path(path.out, "OER_MortonArb_point_info_GIS_thinning_natres.csv"), row.names=F)
