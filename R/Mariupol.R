library(RStoolbox)
library(sf)
library(terra)
library(rjson)
library(tmap)
library(stars)
library(ggplot2)
library(mapview)
library(raster)
library(cluster)
library(ggplot2)
library(rasterVis)
# importing tif file and metadata obtained from planet.com, of a small area of Mariupol
file_path <- "E:/DataFiles/Data/citygardenareaofmariupol_psscene_analytic_sr_udm2/files/20220415_073634_64_2212_3B_AnalyticMS_SR_harmonized_clip.tif"
meta_data_path <- "E:/DataFiles/Data/citygardenareaofmariupol_psscene_analytic_sr_udm2/files/20220415_073634_64_2212_metadata.json"

metadata <- fromJSON(paste(readLines(meta_data_path),collapse = "")) # reading metadata into readable list object
city_garden_area <- brick(file_path) # reading tif file into raster brick object (multilayer raster) object

# Information regarding the brick object
summary(city_garden_area) # summary statistics of the individual layer bands
city_garden_area # more information
crs(city_garden_area) # return crs 
nbands(city_garden_area) # number of bands
ncell(city_garden_area) # number of cells in the raster
raster::res(city_garden_area) # each pixel represents 3 by 3 meters on the ground.
city_garden_extent <- extent(city_garden_area) # retrieving boundaries of the area
city_garden_extent
# Quick rgb plot of the area under consideration
raster::plotRGB(city_garden_area,r=3,g=2,b=1,scale=1000,stretch="lin",
                main = "Original Imagery", axes = T)

### Spatial resampling, we will perform both upsampling and downsampling ###

# Creating empty raster layer of spatial resolution 2 m, we'll define the dimensions, resolution
# and projection
city_garden_area_us2 <- raster(crs = crs(city_garden_area),resolution = 2,
                               xmn = city_garden_extent@xmin,
                               xmx = city_garden_extent@xmax,
                               ymn = city_garden_extent@ymin,
                               ymx = city_garden_extent@ymax)

## First we will use the nearest neighbor method. Here is the function to perform the resample. ##
city_garden_area_us2_ngb <- raster::resample(city_garden_area,city_garden_area_us2,method = "ngb")

# Let's check the number of cells and spatial resolution
res(city_garden_area_us2_ngb)
ncell(city_garden_area_us2_ngb)
# We have now upsampled the original imagery, instead of 3m by 3m per pixel, its 2m by 2m per pixel, and there 
# are 2.25 times as many cells in the upsampled imagery as there are in the original. 
raster::plotRGB(city_garden_area_us2_ngb,r=3,g=2,b=1,scale=1000,stretch="lin",
                main = "2 by 2 m Imagery using Nearest Neighbour", axes = T) # quick rbgplot 

## Method of Bilinear interpolation ##
city_garden_area_us2_bl <- raster::resample(city_garden_area,city_garden_area_us2,method = "bilinear")

# Checking cells and res
res(city_garden_area_us2_bl)
ncell(city_garden_area_us2_bl)
raster::plotRGB(city_garden_area_us2_bl,r=3,g=2,b=1,scale=1000,stretch="lin",
                main = "2 by 2 m Imagery using Bilinear interpolation", axes = T) # quick rbgplot 

## Upsampling to 4 by 4 m resolution ##

# Creating empty raster layer of spatial resolution 4m by 4m, we'll define the dimensions, resolution
# and projection
city_garden_area_ds4 <- raster(crs=crs(city_garden_area),resolution = 4, 
                               xmn = city_garden_extent@xmin,
                               xmx = city_garden_extent@xmax,
                               ymn = city_garden_extent@ymin,
                               ymx = city_garden_extent@ymax)

# Downsampling by nearest neighbor method to 4 m instead of 3 m 
city_garden_area_ds4_ngb <- raster::resample(city_garden_area,city_garden_area_ds4,method = "ngb")
# Checking res and number of cells
res(city_garden_area_ds4_ngb)
ncell(city_garden_area_ds4_ngb)
# There are approximately .56 amount of cells in the downsampled imagery, compared to the original
raster::plotRGB(city_garden_area_us2_bl,r=3,g=2,b=1,scale=1000,stretch="lin",
                main = "4 by 4 m Imagery using Nearest Neighbour", axes = T) # quick rbgplot 

## Upsampling to 9 by 9 m resolution ##
city_garden_area_ds9 <- raster(crs=crs(city_garden_area),resolution = 9,
                               xmn = city_garden_extent@xmin,
                               xmx = city_garden_extent@xmax,
                               ymn = city_garden_extent@ymin,
                               ymx = city_garden_extent@ymax)
# Downsampled by nearest neighbor method to 9 m instead of 3 m
city_garden_area_ds9_ngb <- raster::resample(city_garden_area,city_garden_area_ds9, method = "ngb")
ncell(city_garden_area_ds9_ngb)
res(city_garden_area_ds9_ngb)
raster::plotRGB(city_garden_area_ds9_ngb,r=3,g=2,b=1,scale=1000,stretch="lin",
                main = "9 by 9 m Imagery using Nearest Neighbour", axes = T) # quick rbgplot 
 ##########################################################################################
library(RColorBrewer)
library(RStoolbox)
library(sf)
library(terra)
library(rjson)
library(tmap)
library(stars)
library(ggplot2)
library(mapview)
library(raster)
library(cluster)
library(ggplot2)
library(rasterVis)
library(zoom)
library(tidyverse)

# importing tif file and metadata obtained from planet.com, of a small area of Mariupol
file_path <- "E:/DataFiles/Data/citygardenareaofmariupol_psscene_analytic_sr_udm2/files/20220415_073634_64_2212_3B_AnalyticMS_SR_harmonized_clip.tif"
meta_data_path <- "E:/DataFiles/Data/citygardenareaofmariupol_psscene_analytic_sr_udm2/files/20220415_073634_64_2212_metadata.json"

metadata <- fromJSON(paste(readLines(meta_data_path),collapse = "")) # reading metadata into readable list object
city_garden_area <- brick(file_path) # reading tif file into raster brick object (multilayer raster) object

# Information regarding the brick object
summary(city_garden_area) # summary statistics of the individual layer bands
city_garden_area # more information
crs(city_garden_area) # return crs 
nbands(city_garden_area) # number of bands
ncell(city_garden_area) # number of cells in the raster
raster::res(city_garden_area) # each pixel represents 3 by 3 meters on the ground.


ggRGB(city_garden_area, r=3,g=2,b=1,stretch="lin") +
  ggtitle("City Garden, Mariupol") + 
  labs(x='Longitude(m)',y='Latitude(m)') +
  theme(plot.title = element_text(hjust = .5, 
                                  size = 30), # size of axis title
        axis.title = element_text(size = 20)) # size of axis labels

pointfile <- readxl::read_xlsx("C:/Users/gacas/PythonProjects/BlogPosts/Data/MariupolCsv.xlsx")
class(pointfile)
coordinates(pointfile) = ~ Longitude + Latitude
class(pointfile)
pointfile <- st_as_sf(pointfile)
st_crs(pointfile) <- st_crs(city_garden_area)
st_crs(pointfile)

city_garden_area_stretched <- terra::stretch(city_garden_area, minv = 0, maxv = 255, minq = .1, maxq = .99)
tm_shape(city_garden_area_stretched) + 
  tm_rgb(r=3,g=2,b=1) + 
  tm_shape(pointfile) +
  tm_dots(col = "white")
####################################################################################

library(osmdata)
osm.features <- as.vector(available_features())
osm.tags <- as.vector(available_tags("highway"))
osm.tags

# Defining bounding box, which defines a geographical area by its bounding latitudes and longitudes. Use
# getbb() to retrieve the bouding box of a place using its name

mariupol_bb <- getbb("Mariupol")

# Create overpass query
mariupol_bb %>% opq()

# Retrieving the osmdata object

mariupol.roads <- mariupol_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  add_osm_feature(key = "highway", value = "secondary") %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  add_osm_feature(key = "highway", value = "tertiary") %>%
  add_osm_feature(key = "highway", value = "residential") %>%
  add_osm_feature(key = "highway", value = "motorway_link") %>%
  add_osm_feature(key = "highway", value = "trunk_link") %>%
  add_osm_feature(key = "highway", value = "primary_link") %>%
  add_osm_feature(key = "highway", value = "secondary_link") %>%
  add_osm_feature(key = "highway", value = "tertiary_link") %>%
  add_osm_feature(key = "highway", value = "living_street") %>%
  osmdata_sf()





mariupol.roads.sf <- mariupol.roads$osm_lines 
st_transform(mariupol.roads.sf,crs = st_crs(city_garden_area))
as.character(st_crs(mariupol.roads.sf)) == as.character(st_crs(city_garden_area))
View(mariupol.roads.sf)

####################################################################################

library(RColorBrewer)
library(RStoolbox)
library(sf)
library(terra)
library(rjson)
library(tmap)
library(stars)
library(ggplot2)
library(mapview)
library(raster)
library(cluster)
library(ggplot2)
library(rasterVis)
library(zoom)
library(tidyverse)



# importing tif file and metadata obtained from planet.com, of a small area of Mariupol
file_path <- "E:/DataFiles/Data/MariupolApril29_psscene_analytic_8b_sr_udm2/files/20220429_073052_09_2439_3B_AnalyticMS_SR_8b_harmonized_clip.tif"
meta_data_path <- "E:/DataFiles/Data/MariupolApril29_psscene_analytic_8b_sr_udm2/files/20220429_073052_09_2439_metadata.json"
metadata <- fromJSON(paste(readLines(meta_data_path),collapse = "")) # reading metadata into readable list object
city_garden_area <- raster::brick(file_path) # reading tif file into raster brick object (multilayer raster) object

mean_filter_list <- list()

for(i in 1:nbands(city_garden_area)){
  mean_filter_list[[i]] <- focal(city_garden_area[[1]], w=matrix(1,25,25), fun = mean)
}
# cga stands for city garden area
mean_filtered_cga <- brick(mean_filter_list[[1]],mean_filter_list[[2]],
                           mean_filter_list[[3]],mean_filter_list[[4]],
                           mean_filter_list[[5]],mean_filter_list[[6]],
                           mean_filter_list[[7]],mean_filter_list[[8]])
raster::plot(mean_filtered_cga)

####################################################################################


####################################################################################
library(randomForest)
library(caret)
library(e1071)
library(RColorBrewer)
library(RStoolbox)
library(sf)
library(terra)
library(rjson)
library(tmap)
library(stars)
library(ggplot2)
library(mapview)
library(raster)
library(cluster)
library(ggplot2)
library(rasterVis)
library(zoom)
library(tidyverse)
library(data.table)


# importing tif file and metadata obtained from planet.com, of a small area of Mariupol
file_path <- "E:/DataFiles/Data/MariupolApril29_psscene_analytic_8b_sr_udm2/files/20220429_073052_09_2439_3B_AnalyticMS_SR_8b_harmonized_clip.tif"
meta_data_path <- "E:/DataFiles/Data/MariupolApril29_psscene_analytic_8b_sr_udm2/files/20220429_073052_09_2439_metadata.json"
metadata <- fromJSON(paste(readLines(meta_data_path),collapse = "")) # reading metadata into readable list object
city_garden_area <- raster::brick(file_path) # reading tif file into raster brick object (multilayer raster) object


ndvi <- (city_garden_area$nir-city_garden_area$red)/(city_garden_area$nir+city_garden_area$red)

# Creating Vegetation Training Polygons 
raster::plotRGB(city_garden_area,r=6,g=4,b=2, stretch="lin")
vegetation_points <- raster::click(city_garden_area,n=16,show=T,xy=T)
temp <- vegetation_points 
temp <- as.matrix(temp)
temp <- temp[,1:2] 
veg_poly <- st_sfc(
  list(
    st_polygon(list(st_multipoint(rbind(temp[1,],temp[2,],temp[3,],temp[4,],temp[1,])))),
    st_polygon(list(st_multipoint(rbind(temp[5,],temp[6,],temp[7,],temp[8,],temp[5,])))),
    st_polygon(list(st_multipoint(rbind(temp[9,],temp[10,],temp[11,],temp[12,],temp[9,])))),
    st_polygon(list(st_multipoint(rbind(temp[13,],temp[14,],temp[15,],temp[16,],temp[13,]))))
  )
)
plot(veg_poly)
veg_poly <- st_sfc(veg_poly, crs = st_crs(city_garden_area)) 
crs(veg_poly)
crs(city_garden_area)
vegetation_training_polygons <- st_sf(data.frame(Class = "Vegetation"), geometry = veg_poly)


# Creating Apartments Training Polygons 
raster::plotRGB(city_garden_area,r=6,g=4,b=2, stretch="lin")
apartment_points <- raster::click(city_garden_area,n=20,show=T,xy=T)
temp <- apartment_points 
temp <- as.matrix(temp)
temp <- temp[,1:2] 
apartment_poly <- st_sfc(
  list(
    st_polygon(list(st_multipoint(rbind(temp[1,],temp[2,],temp[3,],temp[4,],temp[1,])))),
    st_polygon(list(st_multipoint(rbind(temp[5,],temp[6,],temp[7,],temp[8,],temp[5,])))),
    st_polygon(list(st_multipoint(rbind(temp[9,],temp[10,],temp[11,],temp[12,],temp[9,])))),
    st_polygon(list(st_multipoint(rbind(temp[13,],temp[14,],temp[15,],temp[16,],temp[13,])))),
    st_polygon(list(st_multipoint(rbind(temp[17,],temp[18,],temp[19,],temp[20,],temp[17,]))))
  )
)
plot(apartment_poly)
apartment_poly <- st_sfc(apartment_poly, crs = st_crs(city_garden_area)) 
crs(apartment_poly)
crs(city_garden_area)
apartment_training_polygons <- st_sf(data.frame(Class = "Apartments"), geometry = apartment_poly)


# Creating Housing Training Polygons 
raster::plotRGB(city_garden_area,r=6,g=4,b=2, stretch="lin")
house_points <- raster::click(city_garden_area,n=16,show=T,xy=T)
temp <- house_points 
temp <- as.matrix(temp)
temp <- temp[,1:2] 
house_poly <- st_sfc(
  list(
    st_polygon(list(st_multipoint(rbind(temp[1,],temp[2,],temp[3,],temp[4,],temp[1,])))),
    st_polygon(list(st_multipoint(rbind(temp[5,],temp[6,],temp[7,],temp[8,],temp[5,])))),
    st_polygon(list(st_multipoint(rbind(temp[9,],temp[10,],temp[11,],temp[12,],temp[9,])))),
    st_polygon(list(st_multipoint(rbind(temp[13,],temp[14,],temp[15,],temp[16,],temp[13,]))))
  )
)
plot(house_poly)
house_poly <- st_sfc(house_poly, crs = st_crs(city_garden_area)) 
crs(house_poly)
crs(city_garden_area)
house_training_polygons <- st_sf(data.frame(Class = "Housing"), geometry = house_poly)


# Creating Water Training Polygons 
raster::plotRGB(city_garden_area,r=6,g=4,b=2, stretch="lin")
water_points <- raster::click(city_garden_area,n=16,show=T,xy=T)
temp <- water_points 
temp <- as.matrix(temp)
temp <- temp[,1:2] 
water_poly <- st_sfc(
  list(
    st_polygon(list(st_multipoint(rbind(temp[1,],temp[2,],temp[3,],temp[4,],temp[1,])))),
    st_polygon(list(st_multipoint(rbind(temp[5,],temp[6,],temp[7,],temp[8,],temp[5,])))),
    st_polygon(list(st_multipoint(rbind(temp[9,],temp[10,],temp[11,],temp[12,],temp[9,])))),
    st_polygon(list(st_multipoint(rbind(temp[13,],temp[14,],temp[15,],temp[16,],temp[13,]))))
  )
)
plot(water_poly)
water_poly <- st_sfc(water_poly, crs = st_crs(city_garden_area)) 
crs(water_poly)
crs(city_garden_area)
water_training_polygons <- st_sf(data.frame(Class = "Water"), geometry = water_poly)

# Creating Roads Training Polygons 
raster::plotRGB(city_garden_area,r=6,g=4,b=2, stretch="lin")
road_points <- raster::click(city_garden_area,n=16,show=T,xy=T)
temp <- road_points 
temp <- as.matrix(temp)
temp <- temp[,1:2] 
road_poly <- st_sfc(
  list(
    st_polygon(list(st_multipoint(rbind(temp[1,],temp[2,],temp[3,],temp[4,],temp[1,])))),
    st_polygon(list(st_multipoint(rbind(temp[5,],temp[6,],temp[7,],temp[8,],temp[5,])))),
    st_polygon(list(st_multipoint(rbind(temp[9,],temp[10,],temp[11,],temp[12,],temp[9,])))),
    st_polygon(list(st_multipoint(rbind(temp[13,],temp[14,],temp[15,],temp[16,],temp[13,]))))
  )
)
plot(road_poly)
road_poly <- st_sfc(road_poly, crs = st_crs(city_garden_area)) 
crs(road_poly)
crs(city_garden_area)
roads_training_polygons <- st_sf(data.frame(Class = "Roads"), geometry = road_poly)
trainingData


# Combining the polygons, into one sf object, to be the training data
training_list <- list(vegetation_training_polygons,
                      apartment_training_polygons,
                      house_training_polygons,
                      water_training_polygons,
                      roads_training_polygons)
trainingData <- st_sf(rbindlist(training_list))

# Turning the classes into class "factor"
trainingData$Class <- as.factor(trainingData$Class)

# Lastly, changing the names of the columns, to be acceptable for the function superClass()
colnames(trainingData) <- c("class","geom")
st_geometry(trainingData) <- "geom"


city_garden_area_rf <- superClass(city_garden_area,trainData = trainingData, valData = NULL,
                                   responseCol = "class", model = 'mlc',
                                   mode = 'classification', trainPartition = .2, predType = "prob",
                                   kfold = 2, verbose = F)
city_garden_area

# Mapping reuslt
ggR(city_garden_area_mlc$map, geom_raster = T) +
  ggtitle("Maximum Likelihood Classification") +
  labs(x='Longitude (m)',y='Latitude (m)') +
  theme(plot.title = element_text(hjust=.5,size=30),
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  scale_fill_manual(values = c("brown","green","blue","red","yellow"),name = "Class")











