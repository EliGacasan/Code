



library(tidyverse)
library(astsa)
library(highcharter)
library(xts)
library(lubridate)
library(shiny)
library(zoo)
library(gridExtra)
library(scales)
library(purrr)
library(extrafont)
library(scales)
library(timevis)


pwt <- readxl::read_excel("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/GIS/pwt100.xlsx",sheet="Data")
sort(unique(pwt$country))
##### Russia ######

# Filtering: Russian Federation Data
russia.df <- pwt %>% filter(country == "Russian Federation", year %in% 1990:2019) 
russia.df
# Cleaning: Replacing variables with units 'millions of dollars'
#           with units 'billions of dollars'
#           
columns <- c("rgdpe", "rgdpo", "rgdpna",
             "ccon", "cda", "cgdpe", "cgdpo",
             "cn", "rconna", "rdana", "rnna")
billions.Columns <- modify(russia.df[,columns],
                           ~ .x / 1000)
russia.df <- russia.df %>% select(-columns) %>%
  mutate(billions.Columns)

# Creating time series objects
russia.ts <- ts(data = russia.df, start = 1990, end = 2019)
russia.xts <- as.xts(x = russia.ts)
russia.zoo <- zoo(x = russia.ts)

# Creating Event list # 
# Events: Wars
russia.events.war <- xts(c("First Chechen War: 1994-1996", "Second Chechen War: 1999-2009",
                           "Russo-Ukrainian War: 2014-Present"), 
                         as.Date(c("1994-12-11", "1999-08-7", "2014-02-20")), 
                         unique = T)
russia.events.war.hc <- hchart(russia.events.war)

# Events: Presidents 
russia.events.presidents <- xts(c("Boris Yeltsin: 1991-1999", "Vladimir Putin: 2000-2008",
                                  "Dmitry Medvedev: 2008-2012", "Vladimir Putin: 2012-Present"),
                                as.Date(c("1991-07-10","2000-08-7","2000-08-7","2012-08-7")), 
                                unique = T)
russia.events.presidents.hc <- hchart(russia.events.presidents)


# Refreshing ggplot2 skills # 
###################################################
# Measures of GDP plot #
ggplot(data = russia.df, aes(x = year), fill = "Measure of GDP") + 
  ylab("RGDP at chained PPPs (in bil. 2017US$)") + xlab("") +
  ggtitle("Russian Federation") +
  geom_line(aes(y = rgdpe, color = "cyan4")) +
  geom_line(aes(y = rgdpo, color = "goldenrod")) +
  geom_line(aes(y = cgdpe, color = "purple")) +
  geom_line(aes(y = cgdpo, color = "darkgreen")) + 
  geom_vline(xintercept = 1991, col = "grey") +
  geom_text(aes(x = 1990.7, y = 2800, label = "Yeltsin", angle = 90)) +
  geom_vline(xintercept = 2000.6, col = "grey") +
  geom_text(aes(x = 2000.25, y = 2800, label = "Putin", angle = 90)) +
  geom_vline(xintercept = 2008.6, col = "grey") +
  geom_text(aes(x = 2008.25, y = 2800, label = "Medvedev", angle = 90)) +
  geom_vline(xintercept = 2012.6, col = "grey") +
  geom_text(aes(x = 2012.25, y = 2800, label = "Putin", angle = 90)) +
  scale_color_manual(labels = c("Real GDP:expenditure side", 
                                   "Real GDP:output side", 
                                "Current Price GDP:expenditure side",
                                "Current Price GDP:output side"),
                     values = c("goldenrod", "cyan4", "purple", "darkgreen"),
                     aes(fill = "Measure of Real GDP")) +
  theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
                      panel.grid.major = element_line(color = "white"),
                      panel.grid.minor = element_line(color = "white"),
        plot.margin = margin(20, 17,18,20),
        plot.title = element_text(hjust = .5, family = "serif", size = 20),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.text.x = element_text(family = "serif", size = 12),
        axis.title.y = element_text(family = "serif", size = 15),
        legend.position = c(.12, .91),
        legend.box = "vertical",
        legend.text = element_text(size = 10)) 


# Plot wih President
ggplot(data = russia.df, aes(x = year), fill = "Measure of GDP") + 
  ylab("RGDP at chained PPPs (in bil. 2017US$)") + xlab("") +
  ggtitle("Russian Federation") +
  geom_line(aes(y = rgdpe, color = "cyan4")) +
  geom_line(aes(y = rgdpo, color = "goldenrod")) +
  geom_line(aes(y = cgdpe, color = "purple")) +
  geom_line(aes(y = cgdpo, color = "darkgreen")) + 
  geom_vline(xintercept = 1991, col = "grey") +
  geom_text(aes(x = 1990.7, y = 2800, label = "Yeltsin", angle = 90)) +
  geom_vline(xintercept = 2000.6, col = "grey") +
  geom_text(aes(x = 2000.25, y = 2800, label = "Putin", angle = 90)) +
  geom_vline(xintercept = 2008.6, col = "grey") +
  geom_text(aes(x = 2008.25, y = 2800, label = "Medvedev", angle = 90)) +
  geom_vline(xintercept = 2012.6, col = "grey") +
  geom_text(aes(x = 2012.25, y = 2800, label = "Putin", angle = 90)) +
  scale_color_manual(labels = c("Real GDP:expenditure side", 
                                "Real GDP:output side", 
                                "Current Price GDP:expenditure side",
                                "Current Price GDP:output side"),
                     values = c("goldenrod", "cyan4", "purple", "darkgreen"),
                     aes(fill = "Measure of Real GDP")) +
  theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.margin = margin(20, 17,18,20),
        plot.title = element_text(hjust = .5, family = "serif", size = 20),
        axis.text.y = element_text(family = "serif", size = 12),
        axis.text.x = element_text(family = "serif", size = 12),
        axis.title.y = element_text(family = "serif", size = 15),
        legend.position = c(.12, .91),
        legend.box = "vertical",
        legend.text = element_text(size = 10)) 

###################################################



























