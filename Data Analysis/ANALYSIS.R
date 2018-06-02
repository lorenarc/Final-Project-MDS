
#### FINAL MASTER PROJECT: ANALYSIS OF INEQUALITY FACTORS BETWEEN COUNTRIES ####

#### BY LORENA RECIO @ KSCHOOL

# *****************************************************************************

####-------------------1. SET WORKING DIRECTORY AND INSTALL PACKAGES--------------------####


setwd("C:/Users/satellite/Final-Project-MDS/Data Analysis/")
# ATTENTION! Path of working directory change depends on the computer that you work.

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if (!require("gap")){
  install.packages("gap")
  library(gap)
}

# *****************************************************************************

####-------------------2. LOADING AND TRANSFORMING DATA--------------------####

who <- read.csv('../DATASETS/whocomplete.csv', dec=".",sep=";", header=T, stringsAsFactors = FALSE)

# ATTENTION!Path of the file must be modified by the one that applies for avoid operating errors.

# I change the name of "Gross national income per capita (PPP international $)" feature to "GNIPPP".

names(who)[5] <- "GNIPPP"


# *****************************************************************************

####-------------------3. PREVISUALIZATION--------------------####

# In all the analisys we will have as a dependent variable "GNIPPP".

hist(who1clean$GNIPPP, col="red", main="Histogram of GNIPPP")
# Here we can see that the GNIPPP variable don't have normal distribution.
# So I do logarithm of the GNIPPP to have a more symmetrical data distribution.
hist(log(who1clean$GNIPPP), col="blue", main="Histogram of LOG GNIPPP")


####-------------------4. LINEAR REGRESSION--------------------####

modelpopulation
modelenergy
modelinternet
modelmilitary
modelsugar

modelCO2 <- lm(GNIPPP ~ CO2_emissions, data=who1clean, na.action=na.exclude)
modelCO2log <- lm(log(GNIPPP) ~ CO2_emissions, data=who1clean, na.action=na.exclude)
summary(modelCO2)
summary(modelCO2log)

plot(who1clean[,"GNIPPP"], who1clean[,"CO2_emissions"], main="Relation GNIPPP_CO2", col="brown")

ggplot(who1clean, aes(x = CO2_emissions, y = GNIPPP)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(who1clean, aes(x = CO2_emissions, y = log(GNIPPP))) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)
