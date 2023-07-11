library(raster)

file_path <- "E:/DataFiles/Data/homestead_psscene_analytic_8b_sr_udm2/files/20220418_161614_08_241d_3B_AnalyticMS_SR_8b_harmonized_clip.tif"
homestead <- brick(file_path)
homestead
raster::plotRGB(homestead,r=6,b=2,g=4,scale = 10000,axes=T,stretch = "lin")
crs(homestead)
