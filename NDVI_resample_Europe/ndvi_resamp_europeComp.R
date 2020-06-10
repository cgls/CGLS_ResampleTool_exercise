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
  path2save <- "E:/rotllxa/NDVI_resample/NDVI_resample_Europe"
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  path2data <- ""
  path2save <- ""
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  path2data <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_data"
  path2save <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_resample/NDVI_resample_Europe"
}else{
  stop("Define your machine before to run LPD")
}

setwd(path2save)
  
nc_file300m <- paste0(path2data, "c_gls_NDVI300_PROD-DESC_202005010000_GLOBE_PROBAV_V1.0.1.nc")
qgis_resamp_europe_avrge <- paste0(path2data, "europa1000_aver.tif")
ndvi_1km_orig <- paste0(path2data, "/ndvi_v2_1km_c_gls_NDVI_202005010000_GLOBE_PROBAV_V2.2.1.nc")



## Reading in data QGIS resampled average ####

qgis_resamp_europe_avrge <- raster(qgis_resamp_europe_avrge)

qgis_resamp_europe_avrge <- projectRaster(from = qgis_resamp_europe_avrge, 
                                          res = (1/112),
                                          crs = CRS('+init=EPSG:4326'), 
                                          method="bilinear", 
                                          alignOnly=FALSE, over=FALSE, 
                                          filename="") 
qgis_resamp_europe_avrge

my_extent <- extent(qgis_resamp_europe_avrge)

# Checking correspondence with 1km PROBA-V products
# The following vectors contain Long and Lat coordinates, respectively, of the 1km grid (cell boundaries):
x_ext <- seq((-180 - ((1 / 112) / 2)), 180, (1/112))
y_ext <- seq((80 + ((1 / 112) / 2)), - 60, - (1/112))

if(!all(round(my_extent[1], 7) %in% round(x_ext, 7) &
        round(my_extent[2], 7) %in% round(x_ext, 7) &
        round(my_extent[3], 7) %in% round(y_ext, 7) &
        round(my_extent[4], 7) %in% round(y_ext, 7))){
  # The given extent from raster or coordinate vector does not fit into the 1km PROBA-V grid, so we are going to adjust it
  for(crd in 1:length(as.vector(my_extent))){
    if(crd <= 2){
      my_extent[crd] <- x_ext[order(abs(x_ext - my_extent[crd]))][1]
    }else{
      my_extent[crd] <- y_ext[order(abs(y_ext - my_extent[crd]))][1]
    }
  }
}
as.vector(my_extent)


## Reading in data 1km global ####

ndvi_1km_orig <- raster(ndvi_1km_orig)
ndvi_1km_orig_extnt <- extent(ndvi_1km_orig)

if(all(round(ndvi_1km_orig_extnt[1], 7) %in% round(x_ext, 7) &
       round(ndvi_1km_orig_extnt[2], 7) %in% round(x_ext, 7) &
       round(ndvi_1km_orig_extnt[3], 7) %in% round(y_ext, 7) &
       round(ndvi_1km_orig_extnt[4], 7) %in% round(y_ext, 7))){
  print("ndvi_1km_orig extent matches PROBA-V products")
}else{
  stop("ndvi_1km_orig extent does NOT match PROBA-V products!!!")
}   
  
#cropping to Europe
ndvi_1km_orig_Eur <- crop(ndvi_1km_orig, my_extent)
as.vector(extent(my_extent))
as.vector(extent(ndvi_1km_orig_Eur))
summary(getValues(ndvi_1km_orig_Eur))

jpeg(paste0(path2save, "/ndvi_1km_orig_Eur.jpg"))
plot(ndvi_1km_orig_Eur)
dev.off()

ndvi1km_rstr <- ndvi_1km_orig_Eur


## Reading in data 300m ####
ndvi_300m_orig <- raster(nc_file300m)
ndvi_300m_orig_extnt <- extent(ndvi_300m_orig)

#cropping to Europe
ndvi_300m_orig_Eur <- crop(ndvi_300m_orig, my_extent)
as.vector(extent(my_extent))
as.vector(extent(ndvi_300m_orig_Eur))
summary(getValues(ndvi_300m_orig_Eur))

jpeg(paste0(path2save, "/ndvi_300m_orig_Eur.jpg"))
plot(ndvi_300m_orig_Eur)
dev.off()


if(all(round(extent(ndvi_300m_orig_Eur)[1], 7) %in% round(x_ext, 7) &
       round(extent(ndvi_300m_orig_Eur)[2], 7) %in% round(x_ext, 7) &
       round(extent(ndvi_300m_orig_Eur)[3], 7) %in% round(y_ext, 7) &
       round(extent(ndvi_300m_orig_Eur)[4], 7) %in% round(y_ext, 7))){
  print("ndvi_300m_orig_extnt extent matches PROBA-V products")
}else{
  stop("ndvi_300m_orig_extnt extent does NOT match PROBA-V products!!!")
}   



# Plotting NAs and flagged values map
ndvi300m_rstr <- ndvi_300m_orig_Eur

cuttoff_NA_err <- 0.92  # everything >= cuttoff_NA_err, must be removed for the calculations


jpeg(paste0(path2save, "/ndvi300m_kk.jpg"))
plot(ndvi300m_rstr, breaks = c(minValue(ndvi300m_rstr), cuttoff_NA_err, maxValue(ndvi300m_rstr)), col = c("blue", "red"))
dev.off()


## Dealing with "flagged values" ####
# "flagged values" are those corresponding to water bodies, NAs, etc. 
# They have NDVI values > cuttoff_NA_err (0.92), or assigned values in the NetCDF between 251 and 255.
# We might want to "remove" them from the average calculations as they are highly influencing such averages,
# driving to wrong predictions.


# 300m product
sum(is.na(as.data.frame(ndvi300m_rstr)))
sum(as.data.frame(ndvi300m_rstr) > cuttoff_NA_err, na.rm = TRUE)

ndvi300m_rstr[ndvi300m_rstr > cuttoff_NA_err] <- NA  # setting to NA
sum(is.na(as.data.frame(ndvi300m_rstr)))

# 1km product
ndvi1km_rstr[ndvi1km_rstr > cuttoff_NA_err] <- NA   # setting to NA
sum(is.na(as.data.frame(ndvi1km_rstr)))



## Resampling using aggregate() ####

aggr_method <- "mean"
t0 <- Sys.time()
r300m_resampled1km_Aggr <- aggregate(ndvi300m_rstr,
                                     fact = 3, # from 333m to 1km  
                                     fun = aggr_method, 
                                     na.rm = TRUE, 
                                     filename = 'r300m_resampled1km_Aggr.tif',
                                     overwrite = TRUE)
Sys.time() - t0
r300m_resampled1km_Aggr

# plotting resampled map
jpeg(paste0(path2save, "/r300m_resampled1km_Aggr.jpg"))
plot(r300m_resampled1km_Aggr, main = "r300m_resampled1km_Aggr")
dev.off()


# plotting original 1km and 300m
jpeg(paste0(path2save, "/ndvi1km_300m_Eur.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(ndvi1km_rstr, main = "NDVI 1km")
plot(ndvi300m_rstr, main = "NDVI 333") 
dev.off()


# plotting original-1km + resampled-1km
jpeg(paste0(path2save, "/ndvi1km_1kmResampled_RAggr.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(ndvi1km_rstr, main = "NDVI 1km (original)")
plot(r300m_resampled1km_Aggr, main = "NDVI 1km (resampled)") 
dev.off()




## Resampling using resample() ####

r300m_resampled1km_Bilinear <- resample(ndvi300m_rstr, ndvi1km_rstr, 
                                        method = "bilinear", 
                                        filename = paste0(path2save, "/r300m_resampled1km_Bilinear.tif"),
                                        overwrite = TRUE)




## Comparison 'original-1km' with '300m-resampled-1km-R_Aggr' ####
comp_results <- as.data.frame(matrix(ncol = 4))  #to store results
names(comp_results) <- c("objects", 
                         "Pearson's r", "Root Mean Square Error", "Mean Absolute Error")
comp_results[1, 1] <- "orig-1km__resampl-1km-R-Aggreg"

rsmpl_df <- data.frame(getValues(ndvi1km_rstr), getValues(r300m_resampled1km_Aggr))

sum(complete.cases(rsmpl_df))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
rsmpl_df_pearson
rsmpl_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)
comp_results[1, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
perc_subsample <- 1   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_RAggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.r300m_resampled1km_Aggr. ~ rsmpl_df_subsample$getValues.ndvi1km_rstr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km original NDVI product",
       ylab = "1km resampled NDVI image (R)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
       )
dev.off()


# Calculating differences (errors)
head(rsmpl_df)
rsmpl_df$diff <- abs(rsmpl_df$getValues.ndvi1km_rstr. - rsmpl_df$getValues.r300m_resampled1km_Aggr.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.ndvi1km_rstr., 1) - round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 1))
rsmpl_df$diff3 <- abs(round(rsmpl_df$getValues.ndvi1km_rstr., 3) - round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 3))

summary(rsmpl_df$diff)
summary(rsmpl_df$diff1)
quantile(rsmpl_df$diff1, seq(0, 1, 0.1))
summary(rsmpl_df$diff3) # not substantial differences with 'rsmpl_df$diff'

1/250 # 0.004 is the amount of physical or real value (for NDVI, -0,08:0.92) 
      # for each digital number (0:250), so at least 3 decimals should be included
#

# Root Mean Square Error (RMSE; the lower, the better)
# In GIS, the RMSD is one measure used to assess the accuracy of spatial analysis and remote sensing.
rmse <- sqrt(mean((rsmpl_df$diff)^2)) 
comp_results[1, 3] <- rmse

# Mean Absolute Error (MAE; the lower, the better)
mae <- mean(rsmpl_df$diff)
comp_results[1, 4] <- mae


# Bivariate Linear Regression
lm_obj <- lm(rsmpl_df$getValues.ndvi1km_rstr. ~ rsmpl_df$getValues.r300m_resampled1km_Aggr.)
summary(lm_obj)
lm_obj_summary <- summary(lm_obj)
round(lm_obj_summary$r.squared, 10) == round(rsmpl_df_pearson^2, 10)


# Saving stuff for the report
stuff2save <- c("comp_results")
save(list = stuff2save, file = paste0(path2save, "/ResampleResults4Report.RData"))




## Comparison 'original-1km' with '300m-resampled-1km-QGIS_Aggr' ####
comp_results[2, 1] <- "orig-1km__resampl-1km-QGIS-Aggreg"

rsmpl_df <- data.frame(getValues(ndvi1km_rstr), getValues(qgis_resamp_europe_avrge))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
comp_results[2, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
#perc_subsample <- 1   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_QGISAggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.qgis_resamp_europe_avrge. ~ rsmpl_df_subsample$getValues.ndvi1km_rstr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km original NDVI product",
       ylab = "1km resampled NDVI image (QGIS)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()


# Calculating differences (errors)
head(rsmpl_df)
rsmpl_df$diff <- abs(rsmpl_df$getValues.ndvi1km_rstr. - rsmpl_df$getValues.qgis_resamp_europe_avrge.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.ndvi1km_rstr., 1) - round(rsmpl_df$getValues.qgis_resamp_europe_avrge., 1))

summary(rsmpl_df$diff)
summary(rsmpl_df$diff1)
quantile(rsmpl_df$diff1, seq(0, 1, 0.1))

1/250 # 0.004 is the amount of physical or real value (for NDVI, -0,08:0.92) 
# for each digital number (0:250), so at least 3 decimals should be included
#

# Root Mean Square Error (RMSE; the lower, the better)
# In GIS, the RMSD is one measure used to assess the accuracy of spatial analysis and remote sensing.
rmse <- sqrt(mean((rsmpl_df$diff)^2)) 
comp_results[2, 3] <- rmse

# Mean Absolute Error (MAE; the lower, the better)
mae <- mean(rsmpl_df$diff)
comp_results[2, 4] <- mae




## Comparison '300m-resampled-1km-R_Aggr' with '300m-resampled-1km-QGIS_Aggr' ####
comp_results[3, 1] <- "resampl-1km-R-Aggreg__resampl-1km-QGIS-Aggreg"

rsmpl_df <- data.frame(getValues(r300m_resampled1km_Aggr), getValues(qgis_resamp_europe_avrge))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
comp_results[2, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
#perc_subsample <- 1   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_R_QGIS_Aggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.qgis_resamp_europe_avrge. ~ rsmpl_df_subsample$getValues.r300m_resampled1km_Aggr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km resampled NDVI image (R)",
       ylab = "1km resampled NDVI image (QGIS)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()


# Calculating differences (errors)
head(rsmpl_df)
rsmpl_df$diff <- abs(rsmpl_df$getValues.r300m_resampled1km_Aggr. - rsmpl_df$getValues.qgis_resamp_europe_avrge.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 1) - round(rsmpl_df$getValues.qgis_resamp_europe_avrge., 1))

summary(rsmpl_df$diff)
summary(rsmpl_df$diff1)
quantile(rsmpl_df$diff1, seq(0, 1, 0.1))

1/250 # 0.004 is the amount of physical or real value (for NDVI, -0,08:0.92) 
# for each digital number (0:250), so at least 3 decimals should be included
#

# Root Mean Square Error (RMSE; the lower, the better)
# In GIS, the RMSD is one measure used to assess the accuracy of spatial analysis and remote sensing.
rmse <- sqrt(mean((rsmpl_df$diff)^2)) 
comp_results[3, 3] <- rmse

# Mean Absolute Error (MAE; the lower, the better)
mae <- mean(rsmpl_df$diff)
comp_results[3, 4] <- mae





# Saving stuff for the report
stuff2save <- c("comp_results")
save(list = stuff2save, file = paste0(path2save, "/ResampleResults4Report.RData"))


