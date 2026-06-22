library(terra)
#rotate and save as Tif (faster computation via RAM and not SSD)

temp <- rast("lecture4/data/ERA5_1980-2025_month_temp.nc")
prec <- rast("lecture4/data/ERA5_1980-2025_month_totprec.nc")

temp <- rotate(temp)
prec <- rotate(prec)

writeRaster(temp, "lecture4/data/temp_rot.tif", overwrite=TRUE)
writeRaster(prec, "lecture4/data/prec_rot.tif", overwrite=TRUE)