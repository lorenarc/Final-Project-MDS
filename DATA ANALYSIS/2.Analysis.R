
## FINAL MASTER PROJECT: ANALYSIS OF INEQUALITY FACTORS BETWEEN COUNTRIES ####

## BY LORENA RECIO @ KSCHOOL

# *****************************************************************************



####-------------------1. SET WORKING DIRECTORY AND INSTALL PACKAGES --------------------####


setwd("C:/Users/satellite/Final-Project-MDS/Data Analysis/")
# ATTENTION! Path of working directory change depends on the computer that you work.

if(!require("ggplot2")){
  install.packages("ggplot2", dependencies = T)
  library("ggplot2")
}

if (!require("gap")){
  install.packages("gap", dependencies = T)
  library(gap)
}

if (!require("dplyr")){
  install.packages("dplyr", dependencies = T)
  library(gap)
}

if (!require("data.table")){
  install.packages("data.table", dependencies = T)
  library(gap)
}


# *****************************************************************************


####-------------------2. LOADING AND TRANSFORMING DATA --------------------####

who <- read.csv('../DATASETS/whocomplete.csv', dec=".",sep=";", header=T, 
                stringsAsFactors = FALSE, row.names = "Country")
# ATTENTION!Path of the file must be modified by the one that applies for avoid operating errors.
# In all the analisys we will have as a dependent variable "GNIPPP".

# Now I'll execute a summary of the data to to verify that the type of data loaded is correct.
head(who)
summary(who)

# I change the name of "Gross national income per capita (PPP international $)" feature to "GNIPPP".
names(who)[5] <- "GNIPPP"


# *****************************************************************************


####-------------------3. PRINCIPAL COMPONENT ANALYSIS (PCA) --------------------####

# For this step I'll use all column except the first one "Continent" because it not contains numbers.
whoModified <- subset(who, select = - Continent)

# First of all I check the mean and the variance in order to see the different scale of the features.
meanWho <- as.data.frame(x=colMeans(x=whoModified)) # MEAN
variancewho <- as.data.frame(apply(X = whoModified, MARGIN = 2, FUN = var)) # VARIANCE

# Now I apply prcomp() function to implement PCA. 
# By default prcomp centers the features to get mean equal to zero, and if we indicate 
# "scala=TRUE" we also get standard desviation to be 1.

pca <- prcomp(whoModified, scale = TRUE)
names(pca) 
# CENTER contains the variable means (means that were substracted).
# SCALE contains the variable standard desviations (the scaling applied to each variable )
# ROTATION contains the matrix of variable loadings (columns are the eigenvectors)
# X contains the coordinates of the individuals (observations) on the principal components.

pca$rotation



####-------------------4. PREVISUALIZATION --------------------####



####-------------------5. LINEAR REGRESSION --------------------####

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
