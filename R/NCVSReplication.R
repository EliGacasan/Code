
### 2015 Replication ### 

setwd("D:/R-4.0.3/RStudio/RProjects/Projects/Projects")

library(plyr)
library(tidyverse)
library(ggplot2)
library(lobstr)
library(combinat)

# WGTPERCY: Weight used to determine the average total persons 12 years and older during the collection year.
# The weight factor will contain all zeros for noninterview persons. BJS publications CRIMINAL VICTIMIZATION, YYYY
# use this weight to calculate person totals

# WGTHHCY: Weight used to determine the average total households during the NCVS. This weight takes into consideration that 
# the total period from which interviewers were taken spans 18 months, to allow for the six month reference period. This
# weighting factor will contain all zeros for noninterview households. BJS publications except CRIMINAL
# VICTIMIZATION, YYYY will use this weight to calculate household totals. 

# SERIES_WEIGHT: Victimization weight adjusted for series crimes. 

# Load data files 
load("E:/NCVS/ICPSR_36456-V1/ICPSR_36456/DS0003/36456-0003-Data.rda")
load("E:/NCVS/ICPSR_36456-V1/ICPSR_36456/DS0002/36456-0002-Data.rda")
load("E:/NCVS/ICPSR_36456-V1/ICPSR_36456/DS0001/36456-0001-Data.rda")

incidents <- da36456.0003
rm(da36456.0003)
colnames(incidents)


incidents = subset(incidents, incidents$V4022 != "(1) Outside U.S.")
incidents$violent = ifelse(as.numeric(incidents$V4529) <= 20, 1, 0)
incidents$property = ifelse(as.numeric(incidents$V4529) >= 24, 1, 0)
incidents$robbery <- ifelse(as.numeric(incidents$V4529) %in% c(5,6,7,8,9,10), 1, 0)



# Getting data into yearly list
incidents.year.2015 <- incidents %>% filter(YEAR == 2015)

# Weighted total number of victimized persons (by property and violence victimizations)
victims.totals.2015 <- xtabs(SERIES_WEIGHT ~ violent + property, data = incidents.year.2015)
victims.totals.2015.robbery <- xtabs(SERIES_WEIGHT ~ robbery, incidents.year.2015)
victims.totals.2015.weapon <- xtabs(SERIES_WEIGHT ~ V4049 + violent, incidents.year.2015)

# Access the person-level data file.
persons.year.2015 <- da36456.0002 %>% filter(YEAR == 2015)


# Weighted total number of persons
persons.totals.2015 <- xtabs(WGTPERCY ~ V3001, data = persons.year.2015)
persons.totals.2015

# Access the household-level data file.

households.year.2015 <- da36456.0001 %>% filter(YEAR == 2015); rm(da36456.0001)

# Find weighted number of households 
households.totals.2015 <- xtabs(formula = WGTHHCY ~ V2001, data = households.year.2015)
households.totals.2015

# Violent victimization rate per 1,000 persons
VVP <- as.numeric(format((as.numeric(victims.totals.2015[2,1]) / as.numeric(persons.totals.2015)) * 1000, digits = 4))
VVPWW <- as.numeric(format(((977841.5 / as.numeric(persons.totals.2015)) * 1000), digits=4))
PVH <- as.numeric(format(((victims.totals.2015[1,2] / households.totals.2015) * 1000), digits=4))
RP <- as.numeric(format(((victims.totals.2015.robbery[2] / persons.totals.2015) * 1000), digits=4))



# Calculate the rates.
cat("The rate of violent victimization per 1,000 persons is:", format(((victims.totals.2015[2,1] / persons.totals.2015) * 1000), digits=10), "\n")
cat("The rate of serious violent crime involving weapons per 1,000 persons is:", format(((977841.5 / persons.totals.2015) * 1000), digits=10), "\n")
cat("The rate of property victimization per 1,000 households is:", format(((victims.totals.2015[1,2] / households.totals.2015) * 1000), digits=4), "\n")
cat("The rate of robberies per 1,000 persons is:", format(((victims.totals.2015.robbery[2] / persons.totals.2015) * 1000), digits=4), "\n")




# Calculation of standard errors: To be continued! #





































# A description of variables chosen

# IDHH # 
# The ID for households is unique within a six month interview period. Given the rotating panel design
# of NCVS, a value of IDHH may appear in a year of data one, two, or three times. The Variables 
# YEARQ and IDHH uniquely identify a household at a given interview within the NCVS
# IDPER # (ID for persons) Added variable to faciliate matching of data files. The ID for persons is 
# unique within a six month interview period. 
# V2001 # Household record type. Useless
# V2002 # Household identification number of ICPSR
# V2003 # Year and Quarter Identification Number
# V2004 # Sampling number: Two digit code used to identify the sequence and source of selection of sample
# housing units
# V2005 # Scrambled control number: Data users will not be able to match 1990 sample design cases
# on NCVS data released for quarter 1, 2006 and beyond to 1990 sample design cases on NCVS data files. 
# V2006 # Household number: The number identifies the sequence of households that occupy a particular
# address. For example, if a new family moves into a sample unit being interviewed for a second time, the 
# household number will be 2 indicated that this is a different household from that previously interviewed. 
# V2008 # The "tens" digit identifies the panel; the units digit identifies the rotation group
# V2009 # Person sequence number 
# V2010 # Unit status (unit in sample the previous enumation, unit in sample first time, residue, 
# out of universe)
# V2012 Useless
# V2015 # Tenure (Allocated) Useless 
# V2017 # Land use (ALLOCATED) type  for household 
# V2019 # Farm sales 
# V2021 # Type of living quarters
# V2022 # Location of phone 
# V2024 # Number of housing units in structure 
# V2026 # Household income 
# V2030 # Reason for noninterview  
# V2032  Principal Person Relation to ref person 
# V2033 Principal Person age 
# V2034 Principal person marital status 
# V2035 Principal person marital status  (of previous survey period)
# V2036 Principal person sex
# V2038 Principal person educational attainment 
# V2041 Principal person Hispanic origin
# V2042 Reference Person Age
# V2043 Reference Person Marital Status 
# V2044 Reference Person Marital Status  (of previous survey period)
# V2045 Reference Person Sex 
# V2047 Reference Person Education attainment
# V2050 Reference Person hispanic origin 
# V2071 # # of household members 12 years and older 
# V2072 # # of household members younger than 12 years
# V2073 # Type Z noninterviews 12 years and older (start 2005 Q1)
# This variable started in quarter 1, 2005 
# V2074 # # of crime incident reports (Total number of crime incident reports filled)
# V2076 # Has anyone broken in or attempted to break into your home by forcing a door/window
# pushing past someone, jimmying a lock, cutting a screen, or entering through an open door 
# or window? Has anyone illegally gotten in or tried to get into a garage, shed, or storage room?
# Or illegally gotten in or tried to get into a hotel or motel room or vacation home where you were
# staying? Did any incidents of this type happen to you? # Yes, no, refused, residue, out of universe 
# V2078 # # of motor vehicles owned. What was the total number of cars, vans, trucks, motorcycles,
# or other motor vehicles owned by you or any other member of this household during the last six months.
# Include those you no longer own. 
# V2109 Household record siize 
# V2110 UCF size; recoded value 
# V2111 # Number of person records 
# V2112 # Person record size 
# V2113 # Number of incident Records
# V2114 # Incident record size 
# V2115 # CATI Group number Useless 
# V2116 # Household weight # Weighting factor used to tabulate household data 
# WGTHHCY - Adjust household weight - data year
# V2122 # Family structure (male, wife, children relatives only, male wife, only, lone male etc.)
# V2123 Type interview code 
# V2124 # Incoming or continuing rotation
# V2129 # CBSA MSA status (Central city of an MSA, in MSA but not central city, not msa, residue
# out of unvierse)
# V2130 # Monthly allocated from panel/rot NO.
# V2131 Year Allocated from panel/rot NO. 
# V3001 # Person Record type # Useless?
# V3002 VICPSR HH id number
# V3003 Year and Quarter Identification 
# V3004 Sample number 
# V3005 Scrambled control number 
# V3006 Household number: Sequence of households that occupy a particular addresss
# V3008 Panel and rotation group
# V3009 Person Sequence Number 
# V3010 Person line number 
# V3011 Type of Interview (telephone, personal, proxy, noninterview, residue, out of universe etc)
# V3012 Relationship to reference person
# V3014 Age allocated 
# V3015 Marital status (current survey)
# V3016  Marital Status (Previous survey)
# V3018 Sex (Allocated)
# V3020 Educational attainment 
# V3024 Hispanic Origin 
# V3025 Month Interview completed
# V3026 Day Interview completed 
# V3027 Year Interview completed 
# V3034 Something stolen or attempt 
# Question ... was something belonging to YOU stolen, such as ... or did anyone attempt to steal
# V3040 Attack, threat, theft Location cues; Where were you attacked or threatened, or had something
# stolen from you? 
# V3042 Attack, threat, weapon cues
# Did any incidents of this type happen to you?
# V3044  Stolen, attack, threat; offender known? Did you know the attacker 
# V3046 Were you involved in an incident where you were forced or engaged in unwanted sexual activity?
# V3048 Did you call the police during the last 6 months to report something that happened to you which
# you thought was a crime:? 
# V3060  Who besides the respondent was present when screen questions were asked? (during screen questions )
# V3061 Telephone interview (same question as 3060)
# V3062 # Who besides the respondent was present when screen questions were asked?
# V3063 # Respondent spouse, same question 
# V3064 same question for not spouse 
# V3065 same question under 12 
# V3066 Same question for nonhousehold members
# V3067 Same question, someone present can't say who 
# V3068 # same question, didn't know
# V3069 # same question residue 
# V3077 Household respondent 
# V3080 Person weight, weighting factor used to tabulate person or victim data 
# VWGTPERCY  # Adjust person weight 
# V3081 # Number of crime incident reports 
# V4001 Incident record type 
# V4002 ICPSR HH ID number
# V4003 Year and quarter identification
# V4004 sample number
# V4005 Scrambled control number 
# V4006 Household number 
# V4008 Panel and rotation group
# V4009 Person sequence number 
# V4010 Line number of respondent
# V4011 Screen question number
# V4012 Incident Number 
# V2014  Monthly incident occurred. in what month did first/this incident happen?
# V4015 Year Incident Occurred
# V4016 How many times incident occurred last six months?
# V4017 How many incidents?
# V2022 In what city, town, village? (1 outside us, 2, not inside city/town/village/don't know/ etc)
# V2024 Where did this incident happen?
# V4043 How far from home did the incident happen?
# V4045 Respondent present, were you or any other member of the house present?
# V4288 Was something taken?
# V4387 Was anything damaged?
# V4399 Were the police informed or did they find out about the incident in any way?
# V4467 Did you or someone in the household receieve any help or advice other than the police
# that deals with victims of crime?
# V4478 What were you doing during this incident? (working, sleeping, ..etc)
# V4479 Were you employed at time of incident 
# V4527 Incident weight
# V4528 Type of crime code (completed rape, attempted rape, sexual attack, robbery)
# V4529 Type of crime (new, NCVS)
# WGTVICCY Adjusted victimization weight, collection year
# HELD Identifies incidents where the information was given during the previous interview, but
# the incident date indicates the incident occurred during the current reference period. 
# In this case we held the incident from the previous interview. 

# Only choosing columns with 100% availability of data, so removing about 800 columns. 

























































































