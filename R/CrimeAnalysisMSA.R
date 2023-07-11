
# Dataframe obtained via https://openjustice.doj.ca.gov/data
# Shapefile obtained via https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-counties-1/explore?location=39.492298%2C-106.612721%2C5.49
library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
CaliforniaCrimeData <- read.csv("E:/DataFiles/Data/PopulationData/OnlineArrestData1980-2021.csv")
CountiesShp <- sf::read_sf("E:/DataFiles/Data/PopulationData/California_Counties/California_Counties.shp")
CountiesShp
plot(CountiesShp)
colnames(CaliforniaCrimeData)
unique(CaliforniaCrimeData$COUNTY)
unique(CaliforniaCrimeData$AGE_GROUP)
unique(CaliforniaCrimeData$RACE)
unique(CaliforniaCrimeData$GENDER)
unique(CaliforniaCrimeData$YEAR)
summary(CaliforniaCrimeData)


 
CaliforniaCrimeData$GENDER <- as.factor(CaliforniaCrimeData$GENDER)
CaliforniaCrimeData$AGE_GROUP <- as.factor(CaliforniaCrimeData$AGE_GROUP)
CaliforniaCrimeData$COUNTY <- as.factor(CaliforniaCrimeData$COUNTY)
CaliforniaCrimeData$RACE <- as.factor(CaliforniaCrimeData$RACE)
View(CaliforniaCrimeData)

View(CountiesShp)

CaliforniaCrimeData$COUNTY <- gsub(pattern = "County", replacement = "", as.character(CaliforniaCrimeData$COUNTY))
View(CaliforniaCrimeData)
colnames(CountiesShp)
CountiesShp <- CountiesShp %>% group_by(COUNTY_NAM) %>% summarize(geometry = st_union(geometry))
View(CountiesShp)
unique(CountiesShp$COUNTY_NAM) == unique(CaliforniaCrimeData$COUNTY)

CaliforniaCrimeData$COUNTY <- substr(CaliforniaCrimeData$COUNTY, 1, nchar(CaliforniaCrimeData$COUNTY)-1)
unique(CountiesShp$COUNTY_NAM) == unique(CaliforniaCrimeData$COUNTY)


colnames(CountiesShp)
colnames(CaliforniaCrimeData)
# Ok, now we can merge the datasets, into one! 
joinedData <- left_join(CaliforniaCrimeData, CountiesShp, by = c("COUNTY" = "COUNTY_NAM"))

data <- st_sf(joinedData, crs = st_crs(CountiesShp), sfc_last = T )
View(data[1:100,])
## Note: Data has missing values that are important, for example, it does not have recorded data 
## for hispanic women over the age of 70 in the year 1980 committing any of the categories of crime
## because there were no people in that age group who committed any crimes. 

















