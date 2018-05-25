# For all the project, firtsly I get the path and set the working directory.
getwd()
setwd("C:/Users/satellite/Final-Project-MDS/R Studio")

# Then I install and load some packages that it will be useful for cleaning and processing the data tables.
install.packages("dplyr", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)
install.packages("mice", dependencies = TRUE)
install.packages("VIM", dependencies = TRUE)

library("dplyr","data.table")
library("mice")
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

listtoclean <- c("Present_value_of_debt",
                    "Math_achievement_4th_grade",
                    "Environment.and.public.health.workers.density..per.10.000.population.",
                    "Aid_given",
                    "Children.aged.6.59.months.who.received.vitamin.A.supplementation....",
                    "Nuclear_consumption_per_person",
                    "Prevalence.of.condom.use.by.young.people..15.24.years..at.higher.risk.sex.....female",
                    "Prevalence.of.condom.use.by.young.people..15.24.years..at.higher.risk.sex.....male",
                    "Math_achievement_8th_grade",
                    "Nuclear_consumption",
                    "Community.and.traditional.health.workers.density..per.10.000.population.",
                    "Coal_production_per_person",
                    "Coal_production",
                    "Antiretroviral.therapy.coverage.among.HIV.infected.pregt.women.for.PMTCT....",
                    "Children.aged..lt.5.years.with.ARI.symptoms.taken.to.facility....",
                    "Children.aged..lt.5.years.with.diarrhoea.receiving.ORT....",
                    "Laboratory.health.workers.density..per.10.000.population.",
                    "Children.aged..lt.5.years.sleeping.under.insecticide.treated.nets....",
                    "Number.of.confirmed.poliomyelitis.cases",
                    "Malaria_prevention_insecticide_treated_bed_nets_usage",
                    "Malaria_treatment",
                    "Natural_gas_production",
                    "Natural_gas_production_per_person",
                    "Natural_gas_consumption_per_person",
                    "Oil_proved_reserves",
                    "Oil_proven_reserves_per_person",
                    "Natural_gas_consumption",
                    "Oil_production",
                    "Oil_production_per_person",
                    "Natural_gas_proved_reserves",
                    "Natural_gas_proven_reserves_per_person",
                    "Children.aged..lt.5.years.who.received.any.antimalarial.treatment.for.fever....",
                    "Number.of.community.and.traditional.health.workers",
                    "Imports_unit_value",
                    "Prevalence.of.adults...gt..15.years..who.are.obese.....male",
                    "Hydroelectricity_consumption_per_person",
                    "Hydroelectricity_consumption",
                    "Births.attended.by.skilled.health.personnel.....highest.wealth.quintile",
                    "Births.attended.by.skilled.health.personnel.....lowest.wealth.quintile",
                    "Births.attended.by.skilled.health.personnel.difference.highest.lowest.wealth.quintile",
                    "Births.attended.by.skilled.health.personnel.ratio.highest.lowest.wealth.quintile",
                    "Measles.immunization.coverage.among.one.year.olds.....highest.wealth.quintile",
                    "Measles.immunization.coverage.among.one.year.olds.....lowest.wealth.quintile",
                    "Measles.immunization.coverage.among.one.year.olds.difference.highest.lowest.wealth.quintile",
                    "Measles.immunization.coverage.among.one.year.olds.ratio.highest.lowest.wealth.quintile",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..difference.lowest.highest.wealth.quintile",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..highest.wealth.quintile",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..lowest.wealth.quintile",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..ratio.lowest.highest.wealth.quintile",
                    "Coal_consumption_per_person",
                    "Coal_consumption",
                    "Births.attended.by.skilled.health.personnel.....highest.educational.level.of.mother",
                    "Births.attended.by.skilled.health.personnel.....lowest.educational.level.of.mother",
                    "Births.attended.by.skilled.health.personnel.....rural",
                    "Births.attended.by.skilled.health.personnel.....urban",
                    "Births.attended.by.skilled.health.personnel.difference.highest.lowest.educational.level.of.mother",
                    "Births.attended.by.skilled.health.personnel.difference.urban.rural",
                    "Births.attended.by.skilled.health.personnel.ratio.highest.lowest.educational.level.of.mother",
                    "Births.attended.by.skilled.health.personnel.ratio.urban.rural",
                    "Measles.immunization.coverage.among.one.year.olds.....highest.educational.level.of.mother",
                    "Measles.immunization.coverage.among.one.year.olds.....lowest.educational.level.of.mother",
                    "Measles.immunization.coverage.among.one.year.olds.....rural",
                    "Measles.immunization.coverage.among.one.year.olds.....urban",
                    "Measles.immunization.coverage.among.one.year.olds.difference.highest.lowest.educational.level.of.mother",
                    "Measles.immunization.coverage.among.one.year.olds.difference.urban.rural",
                    "Measles.immunization.coverage.among.one.year.olds.ratio.highest.lowest.educational.level.of.mother",
                    "Measles.immunization.coverage.among.one.year.olds.ratio.urban.rural",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..difference.lowest.highest.educational.level.of.mother",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..difference.rural.urban",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..highest.educational.level.of.mother",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..lowest.educational.level.of.mother",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..ratio.lowest.highest.educational.level.of.mother",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..ratio.rural.urban",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..rural",
                    "Under.5.mortality.rate..Probability.of.dying.aged..lt..5.years.per.1.000.live.births..urban",
                    "Electricity_generation_per_person",
                    "Electricity_generation",
                    "Oil_consumption_per_person",
                    "Primary_energy_consumption_per_person",
                    "Oil_consumption",
                    "Primary_energy_consumption",
                    "Exports_unit_value",
                    "Other.health.service.providers.density..per.10.000.population.",
                    "Ratio.of.health.management.and.support.workers.to.health.service.providers",
                    "Population.living.below.the.poverty.line....living.on..lt..US.1.per.day.")

who1clean <- who1[, ! names(who1) %in% listtoclean, drop = F]

# For countries that present missing values in any features, I'll do the analysis without NA's.

# Now we are ready to start with the analysis.



  