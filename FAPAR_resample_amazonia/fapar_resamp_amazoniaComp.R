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
  path2save <- "E:/rotllxa/NDVI_resample/"
}else if(Sys.info()[4] == "h05-wad.ies.jrc.it"){
  path2data <- ""
  path2save <- ""
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  path2data <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_data"
  path2save <- "/Users/xavi_rp/Documents/D6_LPD/NDVI_resample/FAPAR_resample_amazonia"
}else{
  stop("Define your machine before to run LPD")
}

setwd(path2save)

date <- "august18"
date <- "may19"

if(date == "may19"){
  nc_file300m <- paste0(path2data, "/fapar300_v1_333m/fapar300_v1_333m_c_gls_FAPAR300_201905100000_GLOBE_PROBAV_V1.0.1.nc")
  fapar_1km_orig <- paste0(path2data, "/fapar_v2_1km/fapar_v2_1km_c_gls_FAPAR-RT6_201905100000_GLOBE_PROBAV_V2.0.1.nc")
}else if(date == "august18"){
  nc_file300m <- paste0(path2data, "/fapar300_v1_333m/fapar300_v1_333m_c_gls_FAPAR300_201808100000_GLOBE_PROBAV_V1.0.1.nc")
  fapar_1km_orig <- paste0(path2data, "/fapar_v2_1km/fapar_v2_1km_c_gls_FAPAR-RT6_201808100000_GLOBE_PROBAV_V2.0.1.nc")
}




## Amazonia working extent ####

my_extent <- extent(-70, -63, -5.5, -0.2)


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
  print("'my_extent' coordinates have been adjusted")
}
as.vector(my_extent)


## Reading in data 1km global ####

#nc <- nc_open(fapar_1km_orig)
#str(nc)
#nc$var$FAPAR$missval   # 255
#nc$var$FAPAR$scaleFact  # 0.004
#nc$var$FAPAR$addOffset  # 0  
#
##The physical or real value is computed as digital number * scale + offset.
##But this applies only for valid pixels.
#nc$var$FAPAR$missval * nc$var$FAPAR$scaleFact + nc$var$FAPAR$addOffset    # 1.02

#


fapar_1km_orig <- raster(fapar_1km_orig)
img_date <- fapar_1km_orig@z[[1]]
fapar_1km_orig_extnt <- extent(fapar_1km_orig)

if(all(round(fapar_1km_orig_extnt[1], 7) %in% round(x_ext, 7) &
       round(fapar_1km_orig_extnt[2], 7) %in% round(x_ext, 7) &
       round(fapar_1km_orig_extnt[3], 7) %in% round(y_ext, 7) &
       round(fapar_1km_orig_extnt[4], 7) %in% round(y_ext, 7))){
  print("fapar_1km_orig extent matches PROBA-V products")
}else{
  stop("fapar_1km_orig extent does NOT match PROBA-V products!!!")
}   

#cropping to Amazonia
fapar_1km_orig_Ama <- crop(fapar_1km_orig, my_extent)
as.vector(extent(my_extent))
as.vector(extent(fapar_1km_orig_Ama))
summary(getValues(fapar_1km_orig_Ama))

jpeg(paste0(path2save, "/fapar_1km_orig_Ama.jpg"))
plot(fapar_1km_orig_Ama)
dev.off()

fapar1km_rstr <- fapar_1km_orig_Ama


## Reading in data 300m ####
fapar_300m_orig <- raster(nc_file300m)
fapar_300m_orig_extnt <- extent(fapar_300m_orig)

#cropping to Amazonia
fapar_300m_orig_Ama <- crop(fapar_300m_orig, my_extent)
as.vector(extent(my_extent))
as.vector(extent(fapar_300m_orig_Ama))
summary(getValues(fapar_300m_orig_Ama))

jpeg(paste0(path2save, "/fapar_300m_orig_Ama.jpg"))
plot(fapar_300m_orig_Ama)
dev.off()


if(all(round(extent(fapar_300m_orig_Ama)[1], 7) %in% round(x_ext, 7) &
       round(extent(fapar_300m_orig_Ama)[2], 7) %in% round(x_ext, 7) &
       round(extent(fapar_300m_orig_Ama)[3], 7) %in% round(y_ext, 7) &
       round(extent(fapar_300m_orig_Ama)[4], 7) %in% round(y_ext, 7))){
  print("fapar_300m_orig_extnt extent matches PROBA-V products")
}else{
  stop("fapar_300m_orig_extnt extent does NOT match PROBA-V products!!!")
}   


## Dealing with "flagged values" ####
# "flagged values" are those corresponding to water bodies, NAs, etc. 
# They have FAPAR values > cuttoff_NA_err (0.94), or assigned values in the NetCDF between 251 and 255.
# They have FAPAR values < cuttoff_NA_err_min (0.00), or assigned values in the NetCDF between 251 and 255.
# We might want to "remove" them from the average calculations as they are highly influencing such averages,
# driving to wrong predictions.

# Converting flagged values to NAs
fapar300m_rstr <- fapar_300m_orig_Ama

cuttoff_NA_err <- 0.9400000001  # everything >= cuttoff_NA_err, must be removed for the calculations
cuttoff_NA_err <- 0.94  # everything >= cuttoff_NA_err, must be removed for the calculations
cuttoff_NA_err <- 0.9400001  # everything >= cuttoff_NA_err, must be removed for the calculations
cuttoff_NA_err_min <- -0.00000001  # everything <= cuttoff_NA_err_min, must be removed for the calculations

jpeg(paste0(path2save, "/fapar300m_NA.jpg"))
plot(fapar1km_rstr, breaks = c(minValue(fapar1km_rstr), cuttoff_NA_err), col = c("blue"))
dev.off()


# 300m product
sum(is.na(as.data.frame(fapar300m_rstr)))
sum(as.data.frame(fapar300m_rstr) > cuttoff_NA_err, na.rm = TRUE)
sum(as.data.frame(fapar300m_rstr) < cuttoff_NA_err_min, na.rm = TRUE)

fapar300m_rstr[fapar300m_rstr > cuttoff_NA_err] <- NA  # setting to NA
fapar300m_rstr[fapar300m_rstr < cuttoff_NA_err_min] <- NA  # setting to NA
sum(is.na(as.data.frame(fapar300m_rstr)))

# 1km product
fapar1km_rstr[fapar1km_rstr > cuttoff_NA_err] <- NA   # setting to NA
fapar1km_rstr[fapar1km_rstr < cuttoff_NA_err_min] <- NA   # setting to NA
sum(is.na(as.data.frame(fapar1km_rstr)))



## Resampling using aggregate() ####
mean_w.cond <- function(x, ...){ # mean including condition 'minimum 5 valid pixels'
  n_valid <- sum(!is.na(x)) # number of cells with valid value
  if(n_valid > 4){
    dts <- list(...)
    if(is.null(dts$na_rm)) dts$na_rm <- TRUE
    x_mean <- mean(x, na.rm = dts$na_rm)
    return(x_mean)
  }else{
    x_mean <- NA
    return(x_mean)
  }
}

aggr_method <- "mean_w.cond"
t0 <- Sys.time()
r300m_resampled1km_Aggr <- aggregate(fapar300m_rstr,
                                     fact = 3, # from 333m to 1km  
                                     fun = aggr_method, 
                                     na.rm = TRUE, 
                                     filename = 'r300m_resampled1km_Aggr.tif',
                                     overwrite = TRUE)
Sys.time() - t0
#r300m_resampled1km_Aggr <- raster('r300m_resampled1km_Aggr.tif')
r300m_resampled1km_Aggr

# plotting resampled map
jpeg(paste0(path2save, "/r300m_resampled1km_Aggr.jpg"))
plot(r300m_resampled1km_Aggr, main = "r300m_resampled1km_Aggr")
dev.off()


# plotting original 1km and 300m
jpeg(paste0(path2save, "/fapar1km_300m_Ama.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(fapar1km_rstr, main = "FAPAR 1km")
plot(fapar300m_rstr, main = "FAPAR 333m") 
dev.off()


# plotting original-1km + resampled-1km
jpeg(paste0(path2save, "/fapar1km_1kmResampled_RAggr.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(fapar1km_rstr, main = "FAPAR 1km (original)")
plot(r300m_resampled1km_Aggr, main = "FAPAR 1km (resampled)") 
dev.off()




## Resampling using resample() ####

#r300m_resampled1km_Bilinear <- resample(fapar300m_rstr, fapar1km_rstr, 
#                                        method = "bilinear", 
#                                        filename = paste0(path2save, "/r300m_resampled1km_Bilinear.tif"),
#                                        overwrite = TRUE)




## Comparison 'original-1km' with '300m-resampled-1km-R_Aggr' ####
comp_results <- as.data.frame(matrix(ncol = 4))  #to store results
names(comp_results) <- c("objects", 
                         "Pearson's r", "Root Mean Square Error", "Mean Absolute Error")
comp_results[1, 1] <- "orig-1km__resampl-1km-R-Aggreg"

rsmpl_df <- data.frame(getValues(fapar1km_rstr), getValues(r300m_resampled1km_Aggr))

sum(complete.cases(rsmpl_df))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
rsmpl_df_pearson
rsmpl_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)
comp_results[1, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
perc_subsample <- 10   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_RAggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.r300m_resampled1km_Aggr. ~ rsmpl_df_subsample$getValues.fapar1km_rstr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km original FAPAR product",
       ylab = "1km resampled FAPAR image (R)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()


# Calculating differences (errors)
head(rsmpl_df)
rsmpl_df$diff <- abs(rsmpl_df$getValues.fapar1km_rstr. - rsmpl_df$getValues.r300m_resampled1km_Aggr.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.fapar1km_rstr., 1) - round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 1))
rsmpl_df$diff3 <- abs(round(rsmpl_df$getValues.fapar1km_rstr., 3) - round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 3))

summary(rsmpl_df$diff)
summary(rsmpl_df$diff1)
quantile(rsmpl_df$diff1, seq(0, 1, 0.1))
summary(rsmpl_df$diff3) # not substantial differences with 'rsmpl_df$diff'

1/250 # 0.004 is the amount of physical or real value (for FAPAR, 0.00:0.94) 
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
#lm_obj <- lm(rsmpl_df$getValues.fapar1km_rstr. ~ rsmpl_df$getValues.r300m_resampled1km_Aggr.)
#summary(lm_obj)
#lm_obj_summary <- summary(lm_obj)
#round(lm_obj_summary$r.squared, 10) == round(rsmpl_df_pearson^2, 10)


# Saving stuff for the report
stuff2save <- c("comp_results", "my_extent", "img_date")

if(date == "may19"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report_May19.RData"))
  #load(paste0(path2save, "/ResampleResults_fapar_amazonia_4Report_May19.RData"), verbose = TRUE)
}else if(date == "august18"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report.RData"))
}



## QFLAG ####
nc_file300m <- paste0(path2data, "/fapar300_v1_333m/fapar300_v1_333m_c_gls_FAPAR300_201905100000_GLOBE_PROBAV_V1.0.1.nc")
#nc_open(nc_file300m)
qflag <- raster(nc_file300m, varname = "QFLAG")
#fapar_1km_orig <- paste0(path2data, "/fapar_v2_1km/fapar_v2_1km_c_gls_FAPAR-RT6_201905100000_GLOBE_PROBAV_V2.0.1.nc")
#qflag <- raster(fapar_1km_orig, varname = "QFLAG")

qflag <- crop(qflag, my_extent)
table(getValues(qflag))


# FAPAR_QFLAG 300m results
#       1       3       9      19      33      65      73     131     147 
#  549881 1989835    2239  896931     530   54316     560  429601  265019 

#  1     0000 0001
#  3     0000 0011
#  9     0000 1001
#  19    0001 0011
#  33    0010 0001
#  65    0100 0001
#  73    0100 1001
#  131   1000 0011
#  147   1001 0011

tots <- sum(table(getValues(qflag)))
((429601 + 265019)*100)/tots


#jpeg(paste0(path2save, "/fapar_QFLAG_300m_Land.jpg"))
#plot(qflag, breaks = c(130, maxValue(qflag)), col = c("blue"))
#dev.off()


# FAPAR_QFLAG 1km results
#     0   1024   2048   4100   4132   5124   5156   6148   6180   9220 
#  2108   7028    341  26557   3569 391070  25888   4772   3578      1 



qflag
## Resampling using aggregate() ####
modal_w.cond <- function(x, ...){ # modal including condition 'minimum 5 valid pixels'
  n_valid <- sum(!is.na(x)) # number of cells with valid value
  if(n_valid > 4){
    dts <- list(...)
    if(is.null(dts$na_rm)) dts$na_rm <- TRUE
    x_modal <- modal(x, na.rm = dts$na_rm)
    return(x_modal)
  }else{
    x_modal <- NA
    return(x_modal)
  }
}

aggr_method <- "modal_w.cond"
aggr_method <- "modal"
t0 <- Sys.time()
r300m_resampled1km_Aggr <- aggregate(qflag,
                                     fact = 3, # from 333m to 1km  
                                     fun = aggr_method, 
                                     na.rm = TRUE, 
                                     #filename = 'r300m_resampled1km_Aggr.tif',
                                     overwrite = TRUE)
Sys.time() - t0
r300m_resampled1km_Aggr

# plotting resampled map
jpeg(paste0(path2save, "/r300m_resampled1km_Aggr_QFLAG.jpg"))
categs <- unique(getValues(r300m_resampled1km_Aggr))
colrs <- length(categs)
colrs <- rev(terrain.colors(colrs))
plot(r300m_resampled1km_Aggr, 
     main = "FAPAR-QFLAG 1km (resampled)", 
     legend = FALSE, col = colrs)
par(xpd = TRUE)
legend(x = 'right', 
       legend = c(categs), fill = colrs,
       inset=c(- 0.2))
dev.off()



nModal_w.cond <- function(x, ...){ # modal including condition 'minimum 5 valid pixels'
  n_valid <- sum(!is.na(x)) # number of cells with valid value
  if(n_valid > 0){
    dts <- list(...)
    if(is.null(dts$na_rm)) dts$na_rm <- TRUE
    x_modal <- modal(x, na.rm = dts$na_rm)
    x_nModal <- sum(x == x_modal)
    return(x_nModal)
  }else{
    x_nModal <- NA
    return(x_nModal)
  }
}

aggr_method <- "nModal_w.cond"
t0 <- Sys.time()
r300m_resampled1km_Aggr_1 <- aggregate(qflag,
                                       fact = 3, # from 333m to 1km  
                                       fun = aggr_method, 
                                       na.rm = TRUE, 
                                       #filename = 'r300m_resampled1km_Aggr.tif',
                                       overwrite = TRUE)
Sys.time() - t0
r300m_resampled1km_Aggr_1

freq_df <- as.data.frame(table(getValues(r300m_resampled1km_Aggr_1)))
names(freq_df)[1] <- c("num_cells_equal2modal")
freq_df
round((sum(freq_df[freq_df$num_cells_equal2modal %in% c(5:9), 2]) * 100) / sum(freq_df$Freq), 1)
write.csv(freq_df, "freq_df.csv", row.names = FALSE)


summ_df <- as.data.frame(as.matrix(summary(getValues(r300m_resampled1km_Aggr_1))))
names(summ_df)[1] <- c("num_cells_equal2modal")
summ_df <- round(summ_df, 1)
write.csv(summ_df, "summ_df.csv", row.names = TRUE)


# Saving stuff for the report
stuff2save <- c("comp_results", "my_extent", "img_date", "freq_df")

if(date == "may19"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report_May19.RData"))
}else if(date == "august18"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report.RData"))
}





## Comparison 'original-1km' with '300m-resampled-1km-QGIS_Aggr' ####
qgis_resamp_amazonia_avrge <- paste0(path2data, "/QGIS_CGLT/fapar.tif")

qgis_resamp_amazonia_avrge <- raster(qgis_resamp_amazonia_avrge)

qgis_extent <- extent(qgis_resamp_amazonia_avrge)

# Checking correspondence with 1km PROBA-V products
# The following vectors contain Long and Lat coordinates, respectively, of the 1km grid (cell boundaries):
x_ext <- seq((-180 - ((1 / 112) / 2)), 180, (1/112))
y_ext <- seq((80 + ((1 / 112) / 2)), - 60, - (1/112))

if(all(round(qgis_extent[1], 7) %in% round(x_ext, 7) &
       round(qgis_extent[2], 7) %in% round(x_ext, 7) &
       round(qgis_extent[3], 7) %in% round(y_ext, 7) &
       round(qgis_extent[4], 7) %in% round(y_ext, 7))){
  print("qgis_resamp_amazonia_avrge extent matches PROBA-V products")
}else{
  stop("qgis_resamp_amazonia_avrge extent does NOT match PROBA-V products!!!")
}   

# Cropping 'qgis_resamp_amazonia_avrge'
qgis_resamp_amazonia_avrge <- crop(qgis_resamp_amazonia_avrge, my_extent)
qgis_resamp_amazonia_avrge

if(all(round(extent(qgis_resamp_amazonia_avrge)[1], 7) %in% round(x_ext, 7) &
       round(extent(qgis_resamp_amazonia_avrge)[2], 7) %in% round(x_ext, 7) &
       round(extent(qgis_resamp_amazonia_avrge)[3], 7) %in% round(y_ext, 7) &
       round(extent(qgis_resamp_amazonia_avrge)[4], 7) %in% round(y_ext, 7))){
  print("qgis_resamp_amazonia_avrge extent matches PROBA-V products")
}else{
  stop("qgis_resamp_amazonia_avrge extent does NOT match PROBA-V products!!!")
}   


comp_results[2, 1] <- "orig-1km__resampl-1km-QGIS-Aggreg"

rsmpl_df <- data.frame(getValues(fapar1km_rstr), getValues(qgis_resamp_amazonia_avrge))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
comp_results[2, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
#perc_subsample <- 1   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_QGISAggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.qgis_resamp_amazonia_avrge. ~ rsmpl_df_subsample$getValues.fapar1km_rstr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km original FAPAR product",
       ylab = "1km resampled FAPAR image (QGIS)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()


# Calculating differences (errors)
rsmpl_df$diff <- abs(rsmpl_df$getValues.fapar1km_rstr. - rsmpl_df$getValues.qgis_resamp_amazonia_avrge.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.fapar1km_rstr., 1) - round(rsmpl_df$getValues.qgis_resamp_amazonia_avrge., 1))

# Root Mean Square Error (RMSE; the lower, the better)
# In GIS, the RMSD is one measure used to assess the accuracy of spatial analysis and remote sensing.
rmse <- sqrt(mean((rsmpl_df$diff)^2)) 
comp_results[2, 3] <- rmse

# Mean Absolute Error (MAE; the lower, the better)
mae <- mean(rsmpl_df$diff)
comp_results[2, 4] <- mae


# plotting original-1km + resampled-1km
jpeg(paste0(path2save, "/fapar1km_1kmResampled_QGISAggr.jpg"),
     width = 22, height = 14, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 4, 5))
plot(fapar1km_rstr, main = "FAPAR 1km (original)")
plot(qgis_resamp_amazonia_avrge, main = "FAPAR 1km (resampled)") 
dev.off()




## Comparison '300m-resampled-1km-R_Aggr' with '300m-resampled-1km-QGIS_Aggr' ####
comp_results[3, 1] <- "resampl-1km-R-Aggreg__resampl-1km-QGIS-Aggreg"

rsmpl_df <- data.frame(getValues(r300m_resampled1km_Aggr), getValues(qgis_resamp_amazonia_avrge))
rsmpl_df <- rsmpl_df[complete.cases(rsmpl_df), 1:2]

# Pearson's correlation coefficient
rsmpl_df_pearson <- cor(rsmpl_df, method = "pearson")[2, 1]
comp_results[3, 2] <- rsmpl_df_pearson

# Plotting correlation (scatterplot)
#perc_subsample <- 1   # percentage of points for plotting
num_subsample <- round((nrow(rsmpl_df) * perc_subsample / 100), 0)
rsmpl_df_subsample <- rsmpl_df[sample(nrow(rsmpl_df), num_subsample), ]

jpeg(paste0(path2save, "/resample_correlation_R_QGIS_Aggr.jpg"))
xyplot(rsmpl_df_subsample$getValues.qgis_resamp_amazonia_avrge. ~ rsmpl_df_subsample$getValues.r300m_resampled1km_Aggr., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "1km resampled FAPAR image (R)",
       ylab = "1km resampled FAPAR image (QGIS)",
       main = paste0("Pearson's r = ", as.character(round(rsmpl_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()


# Calculating differences (errors)
rsmpl_df$diff <- abs(rsmpl_df$getValues.r300m_resampled1km_Aggr. - rsmpl_df$getValues.qgis_resamp_amazonia_avrge.)
rsmpl_df$diff1 <- abs(round(rsmpl_df$getValues.r300m_resampled1km_Aggr., 1) - round(rsmpl_df$getValues.qgis_resamp_amazonia_avrge., 1))

# Root Mean Square Error (RMSE; the lower, the better)
# In GIS, the RMSD is one measure used to assess the accuracy of spatial analysis and remote sensing.
rmse <- sqrt(mean((rsmpl_df$diff)^2)) 
comp_results[3, 3] <- rmse

# Mean Absolute Error (MAE; the lower, the better)
mae <- mean(rsmpl_df$diff)
comp_results[3, 4] <- mae




# Saving stuff for the report
stuff2save <- c("comp_results", "my_extent", "img_date", "freq_df")

if(date == "may19"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report_May19.RData"))
}else if(date == "august18"){
  save(list = stuff2save, file = paste0(path2save, "/ResampleResults_fapar_amazonia_4Report.RData"))
}

comp_results[, 2:4] <- round(comp_results[, 2:4], 5)
write.csv(comp_results, "comp_results.csv")




