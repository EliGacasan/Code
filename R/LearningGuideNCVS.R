

# * !! Edit this change directory command to include the path to your saved data.
setwd("E:/NCVS/2015")

# Access the incident-level data file.
load("E:/NCVS/2015/incidents2015/ICPSR_36448/DS0004/36448-0004-Data.rda")



# Exclude crimes occurring outside the United States.
incidents <- subset(da36448.0004, da36448.0004$V4022 != "(1) Outside U.S.")


incidents$V4249

# Create variables that capture the total number of property and violent victimizations.
incidents$violent <- ifelse(as.numeric(incidents$V4529) <= 20, 1, 0)
incidents$property <- ifelse(as.numeric(incidents$V4529) >= 24, 1, 0)
incidents$simple <- ifelse(as.numeric(incidents$V4529) %in% c(7, 32, 56, 59, 54, 57, 55, 58, 8, 9), 1, 0)


# Find the weighted numbers of victimizations.
xtabs(SERIES_WEIGHT~violent+property, data=incidents)
(vict_viol <- xtabs(SERIES_WEIGHT~violent+property, data=incidents)[2,1])
(vict_simp <- xtabs(SERIES_WEIGHT ~ simple, data = incidents)[2])
vict_prop <- xtabs(SERIES_WEIGHT~violent+property, data=incidents)[1,2]
cat("Violent victimizations:  ", format(vict_viol, nsmall=1), "\n")
cat("Property victimizations:", format(vict_prop, nsmall=1), "\n")

# Access the person-level data file.
load("E:/NCVS/2015/persons2015/ICPSR_36448/DS0003/36448-0003-Data.rda")
xtabs(WGTPERCY~V3001, data=da36448.0003)

# Total number of estimated persons, a weighted calculation using weights and persons
xtabs(WGTPERCY~V3001, data=da36448.0003)

# Find the weighted number of persons.
persons <- as.numeric(xtabs(WGTPERCY~V3001, data=da36448.0003)[1])
cat("Persons:  ", format(persons, nsmall=1), "\n")


# Access the household-level data file.
load("E:/NCVS/2015/households2015/ICPSR_36448/DS0002/36448-0002-Data.rda")

# Find weighted number of households again
xtabs(WGTHHCY~V2001, data=da36448.0002)

# Find the weighted number of households.
households <- as.numeric(xtabs(WGTHHCY~V2001, data=da36448.0002)[1])
cat("Households:  ", format(households, nsmall=1), "\n")

# Calculate the rates.
cat("The rate of violent victimization per 1,000 persons is:", format(((vict_viol / persons) * 1000), digits=4), "\n")
cat("The rate of simple assaults per 1,000 persons is:", format(((vict_simp / persons) * 1000), digits = 4), "\n")
cat("The rate of property victimization per 1,000 households is:", format(((vict_prop / households) * 1000), digits=4), "\n")
cat("The rate of violent victimization per 1,000 households is:", format(((vict_viol / households) * 1000), digits=4), "\n")






