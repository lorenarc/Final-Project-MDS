
## FINAL MASTER PROJECT: ANALYSIS OF INEQUALITY FACTORS BETWEEN COUNTRIES ####

## BY LORENA RECIO @ KSCHOOL

# *****************************************************************************

####-------------------1. SET WORKING DIRECTORY AND INSTALL PACKAGES --------------------####

# *****************************************************************************


setwd("C:/Users/satellite/Final-Project-MDS/DATA ANALYSIS/")
# ATTENTION! Path of working directory change depends on the computer that you work.

if(!require("ggplot2")){
  install.packages("ggplot2", dependencies = T)
  library("ggplot2")
}

if (!require("dplyr")){
  install.packages("dplyr", dependencies = T)
  library(gap)
}


# *****************************************************************************

####-------------------2. LOADING AND TRANSFORMING DATA --------------------####

# *****************************************************************************

who <- read.csv('../DATASETS/whodfR.csv', dec=".",sep=";", header=T, 
                stringsAsFactors = FALSE, row.names = "Country")

# ATTENTION!Path of the file must be modified by the one that applies for avoid operating errors.
# In all the analisys we will have as a dependent variable "GNIPPP".

# Now I'll execute a summary of the data to verify that the type of data loaded is correct.

head(who)
summary(who)


# *****************************************************************************

####-------------------3. PRINCIPAL COMPONENT ANALYSIS (PCA) --------------------####

# *****************************************************************************

# For this step I'll use all column except the first one "Continent" because it not contains numbers.

whoModified <- subset(who, select = - Continent)

# First of all I check the mean and the variance in order to see the different scale of the features.

meanWho <- as.data.frame(x=colMeans(x=whoModified)) # MEAN
variancewho <- as.data.frame(apply(X = whoModified, MARGIN = 2, FUN = var)) # VARIANCE

# As we can see the scale of the variables is different, so I'll use the correlation matrix to fix it.

# Now I apply prcomp() function to implement PCA. 
# By default prcomp centers the features to get mean equal to zero, and if we indicate 
# "scala=TRUE" we also get standard desviation to be 1 (Use the correlation matrix instead of
# covariance matrix).

pca.who <- prcomp(whoModified, scale = TRUE)
names(pca.who) # To see the names of the output elements of the PCA.

# CENTER contains the variable means (means that were substracted).
# SCALE contains the variable standard desviations (the scaling applied to each variable )
# ROTATION contains the matrix of variable loadings (columns are the eigenvectors)
# X contains the coordinates of the individuals (observations) on the principal components.

head(pca.who$x)
pca.who$rotation # Here we can se all the main components (PC) and their distributions.

# PC1 have more positive correlation with "GNIPPP", "Population median age",
# "Fixed line and mobile phone subscribers", "Internet users", "Cell phones per 100 people" and
# "Colon and rectum cancer in men and woman". So their positive values could be assimilated with 
# those countries that stand out for their numbers of users using tecnologies, a high GNIPPP and
# median age population, and their have many cases of Colon and rectum cancer.

# PC2 have more positive correlation with "Improved_sanitation_facilities_urban", "Medical doctors"
# and "Life expectancy at birth", so we can assimilated with those countries that have a good health
# system.

plot(pca.who) # I plot "pca.who" to see the variability distribution.

# By biplot function we can obtain a bidimensional representation for the first and second components.
# I put scale=0 so that the arrows have the same scale as the components.

biplot(x = pca.who, scale = 0, cex = 0.5, col = c("blue4", "brown3"))

# Once the main components have been calculated, we can know the variance explained by each of them and 
# the proportion with respect to the total.

pca.who$sdev^2  # We calculate the variance as the square of the standard desviation.

propVar <- pca.who$sdev^2 / sum(pca.who$sdev^2)
propVar # Proportion of variance respect to the total.

ggplot(data = data.frame(propVar, pc = 1:4),
       aes(x = pc, y = propVar)) +
  geom_col(width = 0.5) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Main component",
       y = "Explain variance")
# In this graphic we can see that the first component explain almost the 70% of the total variance.


# *****************************************************************************

####-------------------5. LINEAR MULTIPLE REGRESSION --------------------####

# *****************************************************************************

## --------- 5.1. OUTLIERS TREATMENT ---------- 

boxplot(whoModified,horizontal=F,axes=T) # Here we can see that we have outliers.
boxplot(whoModified,horizontal=F,axes=T, outline =F) 

# I don't remove this outliers because, as we can see, it's for the GNIPPP , Income, Electric power
# consumption and energy use high values in some countries, so I'll do one model without this features.

## --------- 5.2. MODEL WITH ALL FEATURES ---------- 

# How we have continuous quantitative variables, excepto "Continent", we are going to perform different linear multiple 
# regression models adding different variables (or all), to see which ones explain more the 
# changes in the "GNIPPP" (dependent variable)

modeltotal <- lm(GNIPPP ~ ., data=who) # Model with all features
summary(modeltotal)

# The adjusted R-squared value is too high (0.8486) indicating that the model seems to fit good.
# The fact that the model as a whole is significant (p-value: < 2.2e-16), but few factors are 
# significant at the individual level, is an indicator that collinearity may exist.

plot(modeltotal$residuals)
hist(modeltotal$residuals) # Here we can see the outliers residuals

# Plotting the residuals of modeltotal we can see that the linear adjustment would be adequate, 
# there wouldn't be problems, except outliers.

confint(modeltotal)

# With the confidence intervals we see that just the features that are not significant have a 
# confidence interval with a wide range.


## --------- 5.3. MODEL WITHOUT OUTLIERS AND CONTINENT ---------- 


model2 <- lm(GNIPPP ~ Health_expenditure_per_person + Population.proportion.under.15....
             + Improved_sanitation_facilities_urban + Sugar_per_person
             + Population.median.age..years. + Children_and_elderly
             + Personal_computers_per_100_people + Electric_power_consumption
             + Fixed_line_and_mobile_phone_subscribers + Internet_users
             + Broadband_subscribers_per_100_people + Agriculture_contribution_to_economy
             + CO2_emissions + Medical_Doctors + Cell_phones_per_100_people
             + Colon_and_Rectum_cancer_new_cases_per_100_000_women
             + Colon_and_Rectum_cancer_new_cases_per_100_000_men
             + Breast_cancer_new_cases_per_100_000_women + Prostate_cancer_new_cases_per_100_000_men
             + Infant_mortality_rate + Adolescent_fertility_rate + Under_five_mortality_rate
             + Life_expectancy_at_birth + Improved_water_source + Roads_paved, data=who)

summary(model2)
# Adjusted R-squared is good (0.8382) and the whole model is significant with p-value: <2.2e-16.

plot(model2$residuals)
hist(model2$residuals)
# How the model not improve too much, we choose the first one with all features.

## --------- 5.4 MODEL WITH ALL SIGNIFICANT FEATURES IN MODELTOTAL---------- 

# Other model that I'll do is with significant features in modeltotal.


modelSignificant <- lm(GNIPPP ~ Health_expenditure_per_person + Income_per_person
             + Population.median.age..years. + Children_and_elderly
             + Personal_computers_per_100_people + Electric_power_consumption
             + Broadband_subscribers_per_100_people + Agriculture_contribution_to_economy,
             data=who)

summary(modelSignificant)
# Adjusted R-squared is good (0.8484),the whole model is significant with p-value: <2.2e-16 and all
# the features are significant.

# HERE WE CAN CONCLUDE THAT IN THIS MODEL, WE HAVE THE FEATURES THAT MOST CONTRIBUTE TO THE INEQUALITY
# BETWEEN COUNTRIES BECAUSE THEY ARE THE MOST SIGNIFICANT IN AN ADEQUATE MODEL

## --------- 5.5. CONTINENT MODEL ---------- 

# I want to see if belonging to a continent explains changes in "GNIPPP".

modelContinent <- lm(GNIPPP ~ Continent, data=who)
summary(modelContinent)

hist(modelContinent$residuals) # Normal distribution

# We can see that R-squared is too small in this model, but p.value is also small and 
# the features are all significant, except belonging to East Asia.


## --------- 5.6. GRAPHICS ---------- 

ggplot(who, aes(x = Health_expenditure_per_person, y = GNIPPP)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(who, aes(x = Income_per_person, y = GNIPPP)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(who, aes(x = Electric_power_consumption, y = GNIPPP)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

# As we see in this 3 graphics the relation between Heatlh expenditure, Income and Electric power
# consumption with GNIPPP is directly proportional.

# Now let's move to Tableau to do some visualizations.



