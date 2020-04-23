## Settings
library(ncdf4)
library(fields)
library(raster)
library(rgdal)
library(lattice)

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


## Subsetting
subsetting <- "yes"  # I think it does not work
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

nc_close(nc)


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




## Reading in data 300m ####
nc <- nc_open(paste0(path2data, "/", nc_file300m))
nc
str(nc)

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


## Subsetting
subsetting <- "yes"  # I think it does not work
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

nc_close(nc)




## Resampling ####

summary(ndvi1km_rstr)
summary(ndvi300m_rstr)

ndvi300m_rsampled1km <- resample(ndvi300m_rstr, ndvi1km_rstr, 
                                 method = "bilinear", 
                                 filename = paste0(path2save, "/ndvi300m_rsampled1km.tif"))

ndvi300m_rsampled1km
summary(ndvi300m_rsampled1km)

jpeg(paste0(path2save, "/ndvi300m_rsampled1km.jpg"))
plot(ndvi300m_rsampled1km)
dev.off()



rsmpl_df <- data.frame(as.vector(ndvi1km_rstr), as.vector(ndvi300m_rsampled1km))
str(rsmpl_df)
summary(rsmpl_df$as.vector.ndvi300m_rsampled1km.)

length(as.vector(ndvi1km_rstr))
sum(is.na(as.vector(ndvi1km_rstr)))
length(as.vector(ndvi300m_rsampled1km))
sum(is.na(as.vector(ndvi300m_rsampled1km)))

head(rsmpl_df)

rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), ]
nrow(rsmpl_df)


#rsmpl_df <- rsmpl_df[rsmpl_df$as.vector.ndvi1km_rstr. < 0.935, ]
#rsmpl_df <- rsmpl_df[rsmpl_df$as.vector.ndvi300m_rsampled1km. < 0.935, ]

rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")
rsmpl_df_pearson[2, 1]


jpeg(paste0(path2save, "/resample_correlation.jpg"))
xyplot(rsmpl_df$as.vector.ndvi300m_rsampled1km. ~ rsmpl_df$as.vector.ndvi1km_rstr., type = c("p"),
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson[2, 1], 4))))
dev.off()



jpeg(paste0(path2save, "/ndvi1km_300m_Cat.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(ndvi1km_rstr, main = "NDVI 1km")
plot(ndvi300m_rstr, main = "NDVI 333") 
dev.off()



jpeg(paste0(path2save, "/ndvi300m_rsampled1km_compar.jpg"))
plot(ndvi300m_rsampled1km, main = "ndvi300m_rsampled1km")
dev.off()






