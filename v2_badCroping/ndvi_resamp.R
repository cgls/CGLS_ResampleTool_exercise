##

# Resampling Copernicus Land Products (https://land.copernicus.eu/; CLP) at 333m to 1km resolution and assessment of 
# different methodologies.
# CLP are projected in a standard WGS84 projection (also known as the Plate Carrée projection) with the latitude and 
# longitude coordinates defined at the pixel centre (1km pixel is 1/112º). This implies that the pixel boundaries 
# extend ± 1/224º for both latitude and longitude at the pixel corners. 
# 300m-products pixels are 1/336º, therefore 1km-prdoducts are 3 x 3 300m-products. 
# However, users should note that due to the pixel coordinate definition (which applies to both 1km and 300m), no 
# proper aggregation of 300m to 1km can be performed at the minimum and maximum latitude and longitude, while such
# an aggregation can be done within these boundaries
# (http://proba-v.vgt.vito.be/sites/proba-v.vgt.vito.be/files/products_user_manual.pdf).


## Settings
#rm(list = ls())
#.rs.restartR()
library(ncdf4)
library(fields)
library(raster)
library(rgdal)
library(lattice)
library(sf)

if(Sys.info()[4] == "D01RI1700371"){
  path2data <- "E:/rotllxa/NDVI_resample/NDVI_data"
  path2save <- "E:/rotllxa/NDVI_resample/NDVI_resample"
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  path2data <- ""
  path2save <- ""
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  path2data <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_data"
  path2save <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_resample"
}else{
  stop("Define your machine before to run LPD")
}

nc_file1km <- "c_gls_NDVI_201908110000_GLOBE_PROBAV_V2.2.1.nc"
nc_file300m <- "c_gls_NDVI300_201908110000_GLOBE_PROBAV_V1.0.1.nc"

# Do the images need to be cropped??
subsetting <- "yes"  



## Reading in data 1km ####
nc <- nc_open(paste0(path2data, "/", nc_file1km))
nc
str(nc)

lon <- ncvar_get(nc, "lon")
head(lon)
summary(lon)
lat <- ncvar_get(nc, "lat")
head(lat)
summary(lat)

time <- ncvar_get(nc, "time")
range(time) 


ndvi1km <- ncvar_get(nc, "NDVI")
dim(ndvi1km)
str(ndvi1km)
summary(as.vector(ndvi1km))
ndvi1km_backup <- ndvi1km
ndvi1km <- ndvi1km_backup

#ndvi1km_rstr <- raster(t(ndvi1km[, seq(length(lat), 1, -1)]))
#extent(ndvi1km_rstr) <- c(range(lon),  range(lat))
#ndvi1km_rstr



## Subsetting
if(subsetting == "yes"){
  Xmin <- 0 # coords where to cut (in deg)
  Xmax <- 4
  Ymin <- 40
  Ymax <- 43
  
  x0 <-   floor((dim(ndvi1km)[1] / sum(abs(range(lon)))) * (abs(range(lon)[1]) + Xmin))
  x1 <- ceiling((dim(ndvi1km)[1] / sum(abs(range(lon)))) * (abs(range(lon)[1]) + Xmax))
  
  y0 <- dim(ndvi1km)[2] - floor((dim(ndvi1km)[2] / sum(abs(range(lat)))) * (abs(range(lat)[1]) + Ymin))
  y1 <- dim(ndvi1km)[2] - ceiling((dim(ndvi1km)[2] / sum(abs(range(lat)))) * (abs(range(lat)[1]) + Ymax))
  
  ndvi1km <- ndvi1km[x0:x1, y0:y1]
  dim(ndvi1km)
  str(ndvi1km)
  summary(as.vector(ndvi1km))

  lon <- lon[x0:x1]
  lat <- lat[y0:y1]
}


## Plotting a map
jpeg(paste0(path2save, "/ndvi1km.jpg"))
#image.plot(lon, rev(lat), ndvi1km[, seq(15680, 1, -1)])
#image.plot(lon, lat, ndvi1km[, seq(338, 1, -1)])
image.plot(lon, lat, ndvi1km[,])
#image.plot(lon, lat, ndvi1km)
dev.off()

## Saving as a raster file (tiff)
ndvi1km_rstr <- raster(t(ndvi1km[, seq(338, 1, -1)]))
extent(ndvi1km_rstr) <- c(range(lon),  range(lat))
ndvi1km_rstr
writeRaster(ndvi1km_rstr, paste0(path2save, "/ndvi1km_Cat.tif"), overwrite = TRUE)
ndvi1km_rstr <- raster(paste0(path2save, "/ndvi1km_Cat.tif"))


## Crop (Catalonia)

#cat_extnt <- extent(c(0.3, 3.4, 40.4, 43))
#crop(ndvi1km_rstr, cat_extnt, filename = paste0(path2save, "/ndvi1km_Cat.tif"))


## cuttoff for NAs and flagged values on the NDVI image ####

# 0.9360001 / 255  # 0.003670589
# scale factor 0.00400000018998981
# add_offset: -0.0799999982118607
valid_range <- 250
# flag_values: 251
# flag_values: 252
# flag_values: 253
# flag_values: 254
flag_values <-  255    # missing_value

# cuttoff for NAs and flagged values on the NDVI image
cuttoff_NA_err <- (max(as.vector(ndvi1km)) / flag_values) * valid_range  # everything >= cuttoff_NA_err, must be removed for the calculations


jpeg(paste0(path2save, "/ndvi1km_kk.jpg"))
plot(ndvi1km_rstr, breaks = c(minValue(ndvi1km_rstr), cuttoff_NA_err, maxValue(ndvi1km_rstr)), col = c("blue", "red"))
dev.off()


nc_close(nc)




## Reading in data 300m ####
nc <- nc_open(paste0(path2data, "/", nc_file300m))
nc
str(nc)

# Getting a subset containing the eastern-southern cell
#lon <- ncvar_get(nc, "lon", start = c(1), count = c(960))
#lat <- ncvar_get(nc, "lat", start = c(1), count = c(40))
#min(lon, 7)
#max(lat, 7)
#

lon <- ncvar_get(nc, "lon", start = c(55000), count = c(15000))
head(lon)
summary(lon)
lat <- ncvar_get(nc, "lat", start = c(10000), count = c(7000))
head(lat)
summary(lat)

#time <- ncvar_get(nc, "time")
#range(time) 


ndvi300m <- ncvar_get(nc, "NDVI", start = c(55000, 10000), count = c(15000, 7000))
dim(ndvi300m)
str(ndvi300m)
summary(as.vector(ndvi300m))
ndvi300m_backup <- ndvi300m
ndvi300m <- ndvi300m_backup


## Subsetting (noticed that this 300m will NOT fit exactly with the 1km just subset, it needs to be cropped before resampling)
if(subsetting == "yes"){
  Xmin <- 0 # coords where to cut (in deg)
  Xmax <- 4
  Ymin <- 40
  Ymax <- 43
  
  x0 <-   floor((dim(ndvi300m)[1] / sum(abs(range(lon)))) * (abs(range(lon)[1]) + Xmin))
  x1 <- ceiling((dim(ndvi300m)[1] / sum(abs(range(lon)))) * (abs(range(lon)[1]) + Xmax))
  
  y0 <-  dim(ndvi300m)[2] -  floor((dim(ndvi300m)[2] / (max(lat) - min(lat))) * (Ymin - abs(range(lat)[1])))
  y1 <-  dim(ndvi300m)[2] - ceiling((dim(ndvi300m)[2] / (max(lat) - min(lat))) * (Ymax - abs(range(lat)[1])))
  
  ndvi300m <- ndvi300m[x0:x1, y0:y1]
  dim(ndvi300m)
  str(ndvi300m)
  summary(as.vector(ndvi300m))
  
  lon <- lon[x0:x1]
  lat <- lat[y0:y1]
}


## Plotting a map
jpeg(paste0(path2save, "/ndvi300m.jpg"))
image.plot(lon, lat, ndvi300m[,])
dev.off()

## Saving as a raster file (tiff)
ndvi300m_rstr <- raster(t(ndvi300m[, seq(1010, 1, -1)]))
extent(ndvi300m_rstr) <- c(range(lon),  range(lat))
ndvi300m_rstr
writeRaster(ndvi300m_rstr, paste0(path2save, "/ndvi300m_Cat.tif"), overwrite = TRUE)
ndvi300m_rstr <- raster(paste0(path2save, "/ndvi300m_Cat.tif"))

## Crop (Catalonia)

#cat_extnt <- extent(c(0.3, 3.4, 40.4, 43))
#crop(ndvi300m_rstr, cat_extnt, filename = paste0(path2save, "/ndvi300m_Cat.tif"))


# Plotting NAs and flagged values map
cuttoff_NA_err_300 <- (max(as.vector(ndvi300m)) / flag_values) * valid_range  # everything >= cuttoff_NA_err, must be removed for the calculations

if(cuttoff_NA_err != cuttoff_NA_err_300) stop("check why the cutoff is different for each resolution!!!")

jpeg(paste0(path2save, "/ndvi300m_kk.jpg"))
plot(ndvi300m_rstr, breaks = c(minValue(ndvi300m_rstr), cuttoff_NA_err, maxValue(ndvi300m_rstr)), col = c("blue", "red"))
dev.off()


nc_close(nc)




## Dealing with "flagged values" ####
# "flagged values" are those corresponding to water bodies, NAs, etc. 
# They have NDVI values > cuttoff_NA_err (0.9176471), or assigned values in the NetCDF between 251 and 255.
# We might want to "remove" them from the average calculations as they are highly influencing such averages,
# driving to wrong predictions.
#
# 
summary(ndvi1km_rstr)
sum(is.na(as.data.frame(ndvi1km_rstr)))
sum(as.data.frame(ndvi1km_rstr) > cuttoff_NA_err)

ndvi1km_rstr[ndvi1km_rstr > cuttoff_NA_err] <- NA
sum(is.na(as.data.frame(ndvi1km_rstr)))
sum(as.data.frame(ndvi1km_rstr) > cuttoff_NA_err, na.rm = T)
summary(ndvi1km_rstr)


summary(ndvi300m_rstr)
ndvi300m_rstr[ndvi300m_rstr > cuttoff_NA_err] <- NA
sum(is.na(as.data.frame(ndvi300m_rstr)))
nrow(as.data.frame(ndvi300m_rstr))






## Resampling using the Bilinear approach ####

summary(ndvi1km_rstr)
summary(ndvi300m_rstr)



ndvi300m_rsampled1km <- resample(ndvi300m_rstr, ndvi1km_rstr, 
                                 #na.rm = TRUE,
                                 method = "bilinear", 
                                 filename = paste0(path2save, "/ndvi300m_rsampled1km.tif"),
                                 overwrite = TRUE)
#ndvi300m_rsampled1km <- raster(paste0(path2save, "/ndvi300m_rsampled1km.tif"))
ndvi300m_rsampled1km_noNAs <- ndvi300m_rsampled1km
ndvi300m_rsampled1km
summary(ndvi300m_rsampled1km)
summary(ndvi300m_rsampled1km_noNAs)
nrow(as.data.frame(ndvi300m_rsampled1km))

jpeg(paste0(path2save, "/ndvi300m_rsampled1km.jpg"))
plot(ndvi300m_rsampled1km)
dev.off()



rsmpl_df <- data.frame(as.vector(ndvi1km_rstr), as.vector(ndvi300m_rsampled1km))
head(rsmpl_df)
str(rsmpl_df)
nrow(rsmpl_df)
summary(rsmpl_df$as.vector.ndvi300m_rsampled1km.)

length(as.vector(ndvi1km_rstr))
sum(is.na(as.vector(ndvi1km_rstr)))
length(as.vector(ndvi300m_rsampled1km))
sum(is.na(as.vector(ndvi300m_rsampled1km)))

head(rsmpl_df)
sum(!complete.cases(rsmpl_df))
sum(!complete.cases(rsmpl_df[, 2]))

#rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), ]
nrow(rsmpl_df)
head(rsmpl_df)

cuttoff_NA_err #this is the last good
sum(rsmpl_df$as.vector.ndvi1km_rstr. > cuttoff_NA_err)
sum(rsmpl_df$as.vector.ndvi300m_rsampled1km. > cuttoff_NA_err, na.rm = TRUE)
rsmpl_df$Err1km <- ifelse(as.vector(ndvi1km_rstr) > (cuttoff_NA_err + 0.0001), "red", "blue")
rsmpl_df$Err1kmResampled <- ifelse(rsmpl_df$as.vector.ndvi300m_rsampled1km. > (cuttoff_NA_err + 0.0001), "green", "blue")
rsmpl_df$badResamplingLow <- ifelse(rsmpl_df$as.vector.ndvi1km_rstr. > (cuttoff_NA_err + 0.0001) &
                                      rsmpl_df$as.vector.ndvi300m_rsampled1km. <= cuttoff_NA_err,
                                    "red", "green")
badResamplingLowProp <- round(((table(rsmpl_df$badResamplingLow)[2] / nrow(rsmpl_df)) * 100), 2)
rsmpl_df$badResamplingHigh <- ifelse(rsmpl_df$as.vector.ndvi300m_rsampled1km. > (cuttoff_NA_err + 0.0001) &
                                      rsmpl_df$as.vector.ndvi1km_rstr. <= cuttoff_NA_err,
                                    "red", "green")
badResamplingHighProp <- round(((table(rsmpl_df$badResamplingHigh)[2] / nrow(rsmpl_df)) * 100), 2)


head(rsmpl_df)
range(rsmpl_df[rsmpl_df$Err1km == "red", 1])
#rsmpl_df <- rsmpl_df[rsmpl_df$as.vector.ndvi300m_rsampled1km. < 0.935, ]


rsmpl_df_pearson <- cor(rsmpl_df[complete.cases(rsmpl_df), 1:2], method = "pearson")[2, 1]

sum(is.na(rsmpl_df$as.vector.ndvi1km_rstr.) & !is.na(rsmpl_df$as.vector.ndvi300m_rsampled1km.)) +
  sum(!is.na(rsmpl_df$as.vector.ndvi1km_rstr.) & is.na(rsmpl_df$as.vector.ndvi300m_rsampled1km.))
sum(!complete.cases(rsmpl_df[, 1:2]))
sum(complete.cases(rsmpl_df[, 1:2]))
nrow(rsmpl_df)


jpeg(paste0(path2save, "/resample_correlation.jpg"))
xyplot(rsmpl_df$as.vector.ndvi300m_rsampled1km. ~ rsmpl_df$as.vector.ndvi1km_rstr., type = c("p"),
       #col = rsmpl_df$Err1km,
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))))
dev.off()



jpeg(paste0(path2save, "/ndvi1km_300m_Cat.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(ndvi1km_rstr, main = "NDVI 1km")
plot(ndvi300m_rstr, main = "NDVI 333") 
dev.off()


jpeg(paste0(path2save, "/ndvi1km_Err.jpg"))
rstr2plot <- setValues(ndvi1km_rstr, as.factor(rsmpl_df$Err1km))
plot(rstr2plot, main = paste0("NDVI values > ", cuttoff_NA_err, " (NA, errors, etc)"), 
     legend = FALSE, 
     col = rev(c("blue", "white"))
     )
dev.off()



jpeg(paste0(path2save, "/ndvi1km_1kmResampled.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(ndvi1km_rstr, main = "NDVI 1km")
plot(ndvi300m_rsampled1km, main = "NDVI 1km (resampled)") 
dev.off()




# Mapping underpredictions 
#map_countr <- st_read("/Users/xavi_rp/Documents/Reference_Maps/ref-nuts-2016-60m.shp/NUTS_BN_60M_2016_4326_LEVL_0.shp/NUTS_BN_60M_2016_4326_LEVL_0.shp")
jpeg(paste0(path2save, "/ndvi300m_rsampled1km_badResamplingLow.jpg"))
rstr2plot <- setValues(ndvi1km_rstr, as.factor(rsmpl_df$badResamplingLow))
plot(rstr2plot, main = paste0("NDVI observed 1km > ", round(cuttoff_NA_err, 4), "\n and predicted <= ", round(cuttoff_NA_err, 4)), 
     legend = FALSE, col = rev(c("green", "red")), cex.main = 1.3)
#plot(map_countr, add =TRUE)
text(x = 2, y = 39.5, 
     label = paste0("Proportion of underpredicted pixels: ", badResamplingLowProp, "%"), 
     cex = 1.2, col = "red", xpd = TRUE)
dev.off()


# Mapping overpredictions
jpeg(paste0(path2save, "/ndvi300m_rsampled1km_badResamplingHigh.jpg"))
rstr2plot <- setValues(ndvi1km_rstr, as.factor(rsmpl_df$badResamplingHigh))
plot(rstr2plot, main = paste0("NDVI observed 1km <= ", round(cuttoff_NA_err, 4), "\n and predicted > ", round(cuttoff_NA_err, 4)), 
     legend = FALSE, col = c("green", "red"), cex.main = 1.3)
text(x = 2, y = 39.5, 
     label = paste0("Proportion of overpredicted pixels: ", badResamplingHighProp, "%"), 
     cex = 1.2, col = "red", xpd = TRUE)
dev.off()



jpeg(paste0(path2save, "/ndvi300m_rsampled1km_compar.jpg"))
plot(ndvi300m_rsampled1km, main = "ndvi300m_rsampled1km")
dev.off()


stuff2save <- c("cuttoff_NA_err", "rsmpl_df_pearson", "badResamplingHighProp", "badResamplingLowProp")
save(list = stuff2save, file = paste0(path2save, "/ResampleResults4Report.RData"))





## Resampling using the aggregation approach ####

summary(ndvi1km_rstr)
summary(ndvi300m_rstr)

ndvi300m_rsampled1km_Aggr <- aggregate(ndvi300m_rstr,
                                      fact = 3, # from 333m to 1km  
                                      fun = mean, 
                                      expand = TRUE, 
                                      na.rm = TRUE, 
                                      filename = paste0(path2save, "/ndvi300m_rsampled1km_Aggr.tif"),
                                      overwrite = TRUE)

#ndvi300m_rsampled1km_Aggr <- raster(paste0(path2save, "/ndvi300m_rsampled1km_Aggr.tif"))
ndvi300m_rsampled1km_Aggr
summary(ndvi300m_rsampled1km_Aggr)
nrow(as.data.frame(ndvi300m_rsampled1km_Aggr))   # 787 pixels have been lost!!!

# Expanding the resulting map
ndvi300m_rsampled1km_Aggr <- extend(ndvi300m_rsampled1km_Aggr, ndvi1km_rstr, value = NA, 
                                    filename = paste0(path2save, "/ndvi300m_rsampled1km_Aggr.tif"),
                                    overwrite = TRUE)


jpeg(paste0(path2save, "/ndvi300m_rsampled1km_Aggr.jpg"))
plot(ndvi300m_rsampled1km_Aggr)
dev.off()



rsmpl_df_Aggr <- data.frame(as.vector(ndvi1km_rstr), as.vector(ndvi300m_rsampled1km_Aggr))
str(rsmpl_df_Aggr)
nrow(rsmpl_df_Aggr)
summary(rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr.)

head(rsmpl_df_Aggr)
sum(!complete.cases(rsmpl_df_Aggr))
sum(!complete.cases(rsmpl_df_Aggr[, 2]))

#rsmpl_df_Aggr <- rsmpl_df_Aggr[complete.cases(rsmpl_df_Aggr), ]
nrow(rsmpl_df_Aggr)
head(rsmpl_df_Aggr)


cuttoff_NA_err #this is the last good
sum(rsmpl_df_Aggr$as.vector.ndvi1km_rstr. > cuttoff_NA_err)
sum(rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr. > cuttoff_NA_err, na.rm = TRUE)
rsmpl_df_Aggr$Err1km <- ifelse(as.vector(ndvi1km_rstr) > (cuttoff_NA_err + 0.0001), "red", "blue")
rsmpl_df_Aggr$Err1kmResampled <- ifelse(rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr. > (cuttoff_NA_err + 0.0001), "green", "blue")

rsmpl_df_Aggr$badResamplingLow <- ifelse(rsmpl_df_Aggr$as.vector.ndvi1km_rstr. > (cuttoff_NA_err + 0.0001) &
                                      rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr. <= cuttoff_NA_err,
                                    "red", "green")
badResamplingLowProp_Aggr <- round(((table(rsmpl_df_Aggr$badResamplingLow)[2] / nrow(rsmpl_df_Aggr)) * 100), 2)

rsmpl_df_Aggr$badResamplingHigh <- ifelse(rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr. > (cuttoff_NA_err + 0.0001) &
                                       rsmpl_df_Aggr$as.vector.ndvi1km_rstr. <= cuttoff_NA_err,
                                     "red", "green")
badResamplingHighProp_Aggr <- round(((table(rsmpl_df_Aggr$badResamplingHigh)[2] / nrow(rsmpl_df_Aggr)) * 100), 2)


head(rsmpl_df_Aggr)
range(rsmpl_df_Aggr[rsmpl_df_Aggr$Err1km == "red", 1])
#rsmpl_df_Aggr <- rsmpl_df_Aggr[rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km. < 0.935, ]


rsmpl_df_Aggr_pearson <- cor(rsmpl_df_Aggr[complete.cases(rsmpl_df_Aggr), 1:2], method = "pearson")[2, 1]


jpeg(paste0(path2save, "/resample_correlation_Aggr.jpg"))
xyplot(rsmpl_df_Aggr$as.vector.ndvi300m_rsampled1km_Aggr. ~ rsmpl_df_Aggr$as.vector.ndvi1km_rstr., type = c("p"),
       #col = rsmpl_df_Aggr$Err1km,
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_Aggr_pearson, 4))))
dev.off()


# Mapping underpredictions 
jpeg(paste0(path2save, "/ndvi300m_rsampled1km_badResamplingLow_Aggr.jpg"))
rstr2plot <- setValues(ndvi1km_rstr, as.factor(rsmpl_df_Aggr$badResamplingLow))
plot(rstr2plot, main = paste0("NDVI observed 1km > ", round(cuttoff_NA_err, 4), "\n and predicted <= ", round(cuttoff_NA_err, 4)), 
     legend = FALSE, col = c("green", "red"), cex.main = 1.3)
text(x = 2, y = 39.5, 
     label = paste0("Proportion of underpredicted pixels: ", badResamplingLowProp_Aggr, "%"), 
     cex = 1.2, col = "red", xpd = TRUE)
dev.off()


# Mapping overpredictions
jpeg(paste0(path2save, "/ndvi300m_rsampled1km_badResamplingHigh_Aggr.jpg"))
rstr2plot <- setValues(ndvi1km_rstr, as.factor(rsmpl_df_Aggr$badResamplingHigh))
plot(rstr2plot, main = paste0("NDVI observed 1km <= ", round(cuttoff_NA_err, 4), "\n and predicted > ", round(cuttoff_NA_err, 4)), 
     legend = FALSE, col = c("green", "red"), cex.main = 1.3)
text(x = 2, y = 39.5, 
     label = paste0("Proportion of overpredicted pixels: ", badResamplingHighProp_Aggr, "%"), 
     cex = 1.2, col = "red", xpd = TRUE)
dev.off()



# Saving results 4 report

stuff2save <- c(stuff2save,  "rsmpl_df_Aggr_pearson", "badResamplingHighProp_Aggr", "badResamplingLowProp_Aggr")
save(list = stuff2save, file = paste0(path2save, "/ResampleResults4Report.RData"))



