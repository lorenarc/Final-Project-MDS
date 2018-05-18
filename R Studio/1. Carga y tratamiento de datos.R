# For all the project, firtsly I get the path and set the working directory.
getwd()
setwd("C:/Users/satellite/Final-Project-MDS/R Studio")

# Then I install and load some packages that it will be useful for cleaning and processing the data tables.
install.packages("dplyr", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)
install.packages("sqldf", dependencies = TRUE)
install.packages("mice", dependencies = TRUE)
install.packages("VIM", dependencies = TRUE)

library("dplyr","data.table","sqldf","mice")
library("VIM")

# Now I load the dataset, I can do it directly from the website as follow:
who1 <- read.csv('https://query.data.world/s/8Pb1O_ASzuDGfaFfGu6w6kLT6t6IwL', header=TRUE,stringsAsFactors = FALSE)

# Another way to do it's loading from the path where you saved the file.
who1 <- read.csv('WHO.csv', dec=".",sep=",", header=T, stringsAsFactors = FALSE)

# I visualized all the columns and summarize it to have a general idea of the data.
colnames(who1)
summary(who1)
summary(who1$Gross.national.income.per.capita..PPP.international...)
head(who1)

# I change row names to the Country column and then I remove this column.
# I also change the name of the "GNI per capita" variable and in "Continent" column I put the name
# of the continent instead of numbers.

row.names(who1) <- who1$Country
who1 <- subset(who1,select=-Country)

names(who1)[5] <- "GNIPPP"

who1$Continent[who1$Continent==1] <- "WestAsia"
who1$Continent[who1$Continent==2]  <- "Europe"
who1$Continent[who1$Continent==3] <- "Africa"
who1$Continent[who1$Continent==4] <- "AmericaN"
who1$Continent[who1$Continent==5] <- "AmericaS"
who1$Continent[who1$Continent==6] <- "SouthAsia-Oceania"
who1$Continent[who1$Continent==7] <- "EastAsia"


# Now I do a histogram with VIM package, to see what are the features that have more missing values.

plotmissing <- aggr(who1, col=c('blue','red'), 
                    numbers=TRUE, 
                    sortVars=TRUE,
                    labels=names(who1),
                    cex.axis=.8,
                    gap=3,
                    ylab=c("Histogram of missing values","Pattern"))

# In the next step I remove the columns which present more than 65% of missing values because they will
# not provide much information to the analysis.

who1 <- subset(who1,select=-("faltaponernombresdevariables"))

# For countries that present missing values in any features, I'll do a multiple imputation with mice 
# package.
 
# Now we are ready to start with the analysis.



  