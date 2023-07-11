
# Dataframe obtained via https://openjustice.doj.ca.gov/data
# Shapefile obtained via https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-counties-1/explore?location=39.492298%2C-106.612721%2C5.49
library(readxl)
library(plyr)
library(tidyverse)
library(highcharter)
library(sf)
library(stringr)
library(reshape2)


# Import data
# ------------------------------

# Crime data 
arrests <- read.csv("E:/Crime/OpenJustice/CA/OnlineArrestData1980-2021.csv")
dispositions <- read.csv("E:/Crime/OpenJustice/CA/OnlineArrestDispoData1980-2021.csv")

# Geographic Data
CountiesShp <- sf::read_sf("E:/Shapefiles/US/CA/California_Counties/California_Counties.shp")

# Population data
pop1980 <- readxl::read_xlsx("E:/Population/CA/CitiesandCounties1981to1990.xlsx",
                             col_names = T, skip = 5, n_max = 70)
pop1990 <- readxl::read_xlsx("E:/Population/CA/CitiesandCounties1991to2000.xlsx",
                             col_names = T, skip = 2, n_max = 58)
pop2000 <- readxl::read_xlsx("E:/Population/CA/CitiesandCounties2001to2010.xlsx", 
                             col_names = T, skip = 3, n_max = 58, sheet = 2, col_types = NULL)
pop2010 <- readxl::read_xlsx("E:/Population/CA/CitiesandCounties2011to2020.xlsx", 
                             col_names = T, skip = 1, n_max = 58,sheet = 2)




# ------------------------------


# Cleaning Population Data
# ------------------------------
colnames(pop1980) <- c("County", "Year1981", "Year1982", "Year1983", "Year1984", "Year1985", "Year1986", "Year1987",
                       "Year1988", "Year1989", "Year1990")
colnames(pop1990) <- c("County", "Year1991", "Year1992", "Year1993", "Year1994", "Year1995", "Year1996", "Year1997",
                       "Year1998", "Year1999", "Year2000")
colnames(pop2000) <- c("County", "Year2001", "Year2002", "Year2003", "Year2004", "Year2005", "Year2006", "Year2007",
                       "Year2008", "Year2009", "Year2010")
colnames(pop2010) <- c("County", "Year2011", "Year2012", "Year2013", "Year2014", "Year2015", "Year2016", "Year2017",
                       "Year2018", "Year2019", "Year2020")

population <- right_join(pop1980, pop1990, by = c("County"))
population = right_join(population, pop2000, by = c("County")) 
population = right_join(population, pop2010, by = c("County"))

temp <- pivot_longer(population, cols = colnames(population)[-1]) 
temp1 <- pivot_wider(temp, names_from = "County", values_from = "value")

colnames(temp1) <- c("Year", colnames(temp1)[-1])

temp1$Year <- as.numeric(unlist(regmatches(temp1$Year, gregexpr("[[:digit:]]+", temp1$Year))))
population = temp1
rm(temp1); rm(temp); rm(pop1980); rm(pop1990); rm(pop2000); rm(pop2010)

population
colnames(population)
population.long <- melt(population, id.vars = "Year", measure.vars = colnames(population), variable.name = "County") %>%
  filter(County != "Year")
population.long  


# ------------------------------

# Cleaning and combining arrests and geographic data 
# ------------------------------

# In the arrests dataset, counties are labeled as, for example, "Yolo County", and in the shapefile dataset
# counties are labeled as "Yolo", so here I'm modifying the arrests dataset such that the strings match up 
arrests$COUNTY <- stringr::str_remove(arrests$COUNTY, " County")

# Removing duplicate counties
CountiesShp <- CountiesShp %>% group_by(COUNTY_NAM) %>% summarize(geometry = st_union(geometry))

# Checking if counties are the same in both arrests data and shapefile
unique(CountiesShp$COUNTY_NAM) == unique(arrests$COUNTY)

# Turning strings into factors in the arrests dataset
arrests$GENDER <- as.factor(arrests$GENDER)
arrests$AGE_GROUP <- as.factor(arrests$AGE_GROUP)
arrests$COUNTY <- as.factor(arrests$COUNTY)
arrests$RACE <- as.factor(arrests$RACE)

# Ok, now we can merge the datasets, into one! 
joinedData <- left_join(arrests, CountiesShp, by = c("COUNTY" = "COUNTY_NAM"))
arrests.sf <- st_sf(joinedData, crs = st_crs(CountiesShp), sfc_last = T );
rm(joinedData); rm(dispositions); rm(CountiesShp); 
arrests.sf = arrests.sf %>% select(-S_TOTAL)
arrests = arrests %>% select(-S_TOTAL)


# In order to have any meaningful analysis, we need to interpret the counts
# of the number of offenses in a county relative to the total population of the county
# So we need to merge the datasets, so the population at particular county and 
# year corresponds to the arrests dataset, so we can create rates

data <- inner_join(arrests, population.long, by = c("YEAR" = "Year", "COUNTY" = "County"))
colnames(data) <- replace(colnames(data), c(length(colnames(data))), "Population")
# ----------------------------

# Statistical Analysis 

# Loglinear modelling
library(car)








































































































