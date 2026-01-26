require(raster)
require(ncdf4)
require(viridis)
setwd("~/Desktop/Watson et al., 2026 -- Deforestation MS")


##I'M USING PRISM 4km PRECIPITATION DATA.
prism_raster_april <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201704_bil.bil")
prism_raster_may <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201705_bil.bil")
prism_raster_june <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201706_bil.bil")
prism_raster_july <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201707_bil.bil")
prism_raster_august <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201707_bil.bil")
prism_raster_september <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201707_bil.bil")
prism_raster_october <- raster("./data/precip_validation_materials/PRISM_ppt_stable_4kmM3_2017_all_bil/PRISM_ppt_stable_4kmM3_201707_bil.bil")
beginning_of_month_nc <- nc_open("./data/netCDF_files/wrfout_d01_2017-04-01.nc")
end_of_month_nc <- nc_open("./data/netCDF_files/wrfout_d01_2017-11-01.nc")


prism_total <- prism_raster_april + prism_raster_may + prism_raster_june + prism_raster_july + prism_raster_august + prism_raster_september + prism_raster_october

rainnc <- ncvar_get(end_of_month_nc, "RAINNC")
rainnc_earlier <- ncvar_get(beginning_of_month_nc, "RAINNC")
rain_increment <- rainnc - rainnc_earlier

lat <- ncvar_get(beginning_of_month_nc, "XLAT")
lon <- ncvar_get(beginning_of_month_nc, "XLONG")

###NOW I GOTTA CREATE A WRF RASTER & APPEND THE PRECIP DATA TO IT.
wrf_raster <- raster(nrows = dim(lat)[1], ncols = dim(lat)[2], xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat),
                     crs = "+proj=longlat +datum=WGS84") ##OR WGS84 I'LL GOOGLE IT LATER. LOOKS BETTER AS NAD83, BUT I THINK WRF USES WGS84.

##WEIRD REFLECTION THINGIE HAPPENED, WHERE IT LOOKED LIKE IT WAS BEING REFLECTED OVER THE X-AXIS, COMPARED TO NCVIEW.
new_rain_increment <- rain_increment[, ncol(rain_increment):1]

##NOW I'M ATTACHING IT TO THE 
values(wrf_raster) <- as.vector(new_rain_increment)

###PRISM IS IN 4KM RESOLUTION, WRFOUT FILES ARE IN 3KM. I'M RESAMPLING THE PRISM DATA HERE:
prism_reprojected <- projectRaster(prism_total, crs = crs(wrf_raster))
prism_resampled <- resample(prism_reprojected, wrf_raster, method = "bilinear")
bias <- wrf_raster - prism_resampled

precip_limits <- range(c(values(wrf_raster), values(prism_resampled), values(bias)), na.rm = TRUE)

par(mfrow = c(2, 2)) 
plot(wrf_raster, main = "WRF Precipitation, April - October", col = turbo(100), zlim = precip_limits)
plot(prism_resampled, main = "PRISM Precipitation, April - October", col = turbo(100), zlim = precip_limits)
plot(bias, main = "Bias (WRF - PRISM), April - October", col = turbo(100), zlim = precip_limits)
par(mfrow = c(1, 1))

mean(values(bias), na.rm = TRUE) ##Straight-up average bias
sqrt(mean(values(bias)^2, na.rm = TRUE)) ##Fancy statistiques: RMSE




