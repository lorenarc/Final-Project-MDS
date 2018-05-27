# First of all I set the working directory and install some useful packages.

setwd("C:/Users/satellite/Final-Project-MDS/Data Analysis/")

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if (!require("gap")){
  install.packages("gap")
  library(gap)
}

# In all the analisys we will have as a dependent variable "GNIPPP".

hist(who1clean$GNIPPP, col="red", main="Histogram of GNIPPP")
# Here we can see that the GNIPPP variable don't have normal distribution.
# So I do logarithm of the GNIPPP to have a more symmetrical data distribution.
hist(log(who1clean$GNIPPP), col="blue", main="Histogram of LOG GNIPPP")


##### Linear regression #####

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




