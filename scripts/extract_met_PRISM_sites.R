# ------------------------------------
# Extracting Monthly Met from PRISM for all of the OER sites that I have (unreliable, messy) data for
# C. Rollinson 15 Feb 2019
# ------------------------------------

# ------------------------------------
# Load libraries
# ------------------------------------
library(raster); library(maps)
# ------------------------------------

# ------------------------------------
# Load file with site names & locations & turn it into a spatial file
# ------------------------------------
plot.dat <- read.csv("../data_raw/OER-Fire_SiteCoords.csv")
summary(plot.dat)

# Extracting one climate per cluster; assume the control is representative enough
#  -- Note: this makes the assumption that there are no microclimatic differences among sites
site.dat <- plot.dat[plot.dat$Treatment=="control",]
summary(site.dat)


# Making site a spatial file & graphing to make sure everything looks okay
site.loc <- SpatialPointsDataFrame(coords=site.dat[,c("Long", "Lat")], site.dat[,c("Site", "Long", "Lat")], proj4string=CRS("+proj=longlat"))

# A quick & dirty plot to double check things 
#  (ggplot can make it a lot prettier but requires extra steps)
plot(site.loc, pch=19, cex=0.5, col="blue")
map("state", plot=T,lty="solid", col="gray30", lwd=1.5, add=T)
# ------------------------------------

# ------------------------------------
# Set up & extract the PRISM data
# Note: just fo simplicity, doing everything at once and throwing into 1 met 
#       file in "long" format
# Note: Extracting BATCH of sites from each file because that's faster.  
#       If need to add new sites, another approach may be better
# ------------------------------------
# Making our lives easier by only pulling stable data
# PRISM_tdmean_stable_4kmM1_1895_bil.bil
# Directory containing PRISM data & what variables we have
dir.prism <- "~/Desktop/SpatialData/PRISM/monthly"
var.prism <- c("ppt", "tmax", "tmin", "tmean", "vpdmin", "vpdmax")

mos <- stringr::str_pad(1:12, width=2, side="left", pad=0)
yrs.met <- 1985:2017 # Just going ahead and specifying the year range we want

met.all <- data.frame(site  = rep(site.loc$Site, each=length(yrs.met)*length(mos)),
                      lat   = rep(site.loc$Lat, each=length(yrs.met)*length(mos)),
                      lon   = rep(site.loc$Long, each=length(yrs.met)*length(mos)),
                      year  = rep(rep(yrs.met, each=length(mos)), nrow(site.loc)),
                      month = rep(mos, length.out=length(yrs.met)*nrow(site.loc)))
summary(met.all)
# tail(met.all)

pb <- txtProgressBar(min=0, max=length(var.prism)*length(yrs.met)*length(mos), style=3)
pb.ind=0
for(VAR in var.prism){
  met.all[,VAR] <- NA # Initialize with an empty vector
  # yrs.met <- dir(file.path(dir.prism, VAR)) Use t
  # 
  for(YR in yrs.met){
    for(MO in mos){
      setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
      
      ind.now <- which(met.all$year==YR & met.all$month==MO)
      ftry <- paste("PRISM", VAR, "stable_4kmM1", paste0(YR, MO), "bil.bil", sep="_")
      
      # See if we're using M1, M2, or M3
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))) ftry <- paste("PRISM", VAR, "stable_4kmM2", paste0(YR, MO), "bil.bil", sep="_")
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))) ftry <- paste("PRISM", VAR, "stable_4kmM3", paste0(YR, MO), "bil.bil", sep="_")
      
      # If we don't have a stable M2, then just skip
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))){ print(warning("No stable PRISM file for: ", paste(VAR, YR, MO))); next }
      
      fnow <- raster(file.path(dir.prism, VAR, YR, ftry))
      
      met.all[ind.now,VAR] <- extract(fnow, site.loc)
    }
  }
}

summary(met.all)
write.csv(met.all, "../data_raw/OER_meteorology_monthly_PRISM.csv", row.names=F)
# ------------------------------------

