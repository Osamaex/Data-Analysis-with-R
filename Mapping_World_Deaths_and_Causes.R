#Lets start installing the needed packages
install.packages('tidyverse')
install.packages('formattable')

#lets load the packages
library("tidyverse")
library("ggplot2")
library("dplyr")
library("readr")
library(formattable)

#Importing the datasets
DeathData <- read.csv("annual_deaths_by_causes.csv")
PopulationData <- read.csv("population_total_long.csv")

str(DeathData)
str(PopulationData)

#Renaming our columns
DeathData <- DeathData %>%
  rename(alzheimer_disease = "alzheimer.s_diesease") %>%
  rename(parkinsons_disease = "parkinson.s_disease") %>%
  rename(aids = "hiv.aids")

PopulationData <- PopulationData %>%
  rename(country = "Country.Name") %>%
  rename(year = "Year") %>%
  rename(population = "Count")

#Some country names are written differently in each dataset
#Example, Syria and Syrian Arab Republic
#Lets fix this before merging

PopulationData[PopulationData == 'Egypt, Arab Rep.'] <- 'Egypt'
PopulationData[PopulationData == 'Yemen, Rep.'] <- 'Yemen'
PopulationData[PopulationData == 'Syrian Arab Republic'] <- 'Syria'
PopulationData[PopulationData == 'Iran, Islamic Rep.'] <- 'Iran'
PopulationData[PopulationData == 'Venezuela, RB'] <- 'Venezuela'
PopulationData[PopulationData == 'Lao PDR'] <- 'Laos'
PopulationData[PopulationData == 'Kyrgyz Republic'] <- 'Kyrgyzstan'
PopulationData[PopulationData == 'Slovak Republic'] <- 'Slovakia'
PopulationData[PopulationData == 'Korea, Dem. People’s Rep.'] <- 'North Korea'
PopulationData[PopulationData == 'Korea, Rep.'] <- 'South Korea'
PopulationData[PopulationData == 'Congo, Dem. Rep.'] <- 'Democratic Republic of Congo'
PopulationData[PopulationData == 'Russian Federation'] <- 'Russia'
PopulationData[PopulationData == 'Congo, Rep.'] <- 'Congo'
PopulationData[PopulationData == 'Czech Republic'] <- 'Czechia'

#Merging data
MergedData <- left_join(DeathData, PopulationData, by=c("country", "year"))

#The data frame is challenging to work with, better to change it to a long format
#Converting the data to long format using this code chunk
DataNew <- data.frame(country = MergedData$country,
                       code = MergedData$code,
                       year = MergedData$year,
                       death = c(MergedData$meningitis,
                                 MergedData$alzheimer_disease,
                                 MergedData$parkinsons_disease,
                                 MergedData$nutritional_deficiency,
                                 MergedData$malaria,
                                 MergedData$drowning,
                                 MergedData$interpersonal_violence,
                                 MergedData$maternal_disorders,
                                 MergedData$aids,
                                 MergedData$drug_use_disorders,
                                 MergedData$tuberculosis,
                                 MergedData$cardiovascular_diseases,
                                 MergedData$lower_respiratory_infections,
                                 MergedData$neonatal_disorders,
                                 MergedData$alcohol_use_disorders,
                                 MergedData$self_harm,
                                 MergedData$exposure_to_forces_of_nature,
                                 MergedData$diarrheal_diseases,
                                 MergedData$environmental_heat_and_cold_exposure,
                                 MergedData$neoplasms,
                                 MergedData$conflict_and_terrorism,
                                 MergedData$diabetes_mellitus,
                                 MergedData$chronic_kidney_disease,
                                 MergedData$poisonings,
                                 MergedData$protein_energy_malnutrition,
                                 MergedData$terrorism,
                                 MergedData$road_injuries,
                                 MergedData$chronic_respiratory_diseases,
                                 MergedData$chronic_liver_diseases,
                                 MergedData$digestive_diseases,
                                 MergedData$fire_heat_hot_substance,
                                 MergedData$acute_hepatitis),
                       cause = c(rep("meningitis", nrow(MergedData)),
                                 rep("alzheimer_disease", nrow(MergedData)),
                                 rep("parkinsons_disease", nrow(MergedData)),
                                 rep("nutritional_deficiency", nrow(MergedData)),
                                 rep("malaria", nrow(MergedData)),
                                 rep("drowning", nrow(MergedData)),
                                 rep("interpersonal_violence", nrow(MergedData)),
                                 rep("maternal_disorders", nrow(MergedData)),
                                 rep("aids", nrow(MergedData)),
                                 rep("drug_use_disorders", nrow(MergedData)),
                                 rep("tuberculosis", nrow(MergedData)),
                                 rep("cardiovascular_diseases", nrow(MergedData)),
                                 rep("lower_respiratory_infections", nrow(MergedData)),
                                 rep("neonatal_disorders", nrow(MergedData)),
                                 rep("alcohol_use_disorders", nrow(MergedData)),
                                 rep("self_harm", nrow(MergedData)),
                                 rep("exposure_to_forces_of_nature", nrow(MergedData)),
                                 rep("diarrheal_diseases", nrow(MergedData)),
                                 rep("environmental_heat_and_cold_exposure", nrow(MergedData)),
                                 rep("neoplasms", nrow(MergedData)),
                                 rep("conflict_and_terrorism", nrow(MergedData)),
                                 rep("diabetes_mellitus", nrow(MergedData)),
                                 rep("chronic_kidney_disease", nrow(MergedData)),
                                 rep("poisonings", nrow(MergedData)),
                                 rep("protein_energy_malnutrition", nrow(MergedData)),
                                 rep("terrorism", nrow(MergedData)),
                                 rep("road_injuries", nrow(MergedData)),
                                 rep("chronic_respiratory_diseases", nrow(MergedData)),
                                 rep("chronic_liver_diseases", nrow(MergedData)),
                                 rep("digestive_diseases", nrow(MergedData)),
                                 rep("fire_heat_hot_substance", nrow(MergedData)),
                                 rep("acute_hepatitis", nrow(MergedData))),
                       population = MergedData$population)
View(DataNew)
#This new data frame is much more organized and easy to use for all sorts of calculations
#feel free to use the code chuck for your own notebook

#We have 32 cause of death
n_distinct(LongData$cause)

#Lets check the leading cause of death globally
DataWorldWide <- DataNew %>%
  filter(country == 'World') %>%
  group_by(cause) %>%
  summarise(totaldeath = sum(death)) %>%
  arrange(-totaldeath)
DataWorldWide

#It seems that Cardiovascular diseases are the top 1 killer worldwide
#followed by neoplasms, chronic respiratory diseases, lower respiratory diseases and neonatal disorders

#Lets check the year with most deaths worldwide
DataWorldWide1 <- DataNew %>%
  filter(country == 'World') %>%
  select(country, year, death, cause) %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(totaldeath = sum(death)) %>%
  arrange(-totaldeath) %>%
  ggplot(aes(x=year,y=totaldeath)) +
  geom_col(fill = "purple", color="darkgrey") +
  labs(subtitle = "Death Over the Years", x="Year", y="Total Deaths") +
  coord_flip()
DataWorldWide1

#It seems that 2019 was the year with the highest death

#Lets plot the number 1 killer to see progression over the years
DataCardio <- DataNew %>%
  filter(cause == 'cardiovascular_diseases' & country == 'World') %>%
  ggplot(aes(x=year, y=death)) +
  geom_line(color="blue") +
  labs(subtitle = "Cardiovascular Diseases Over the Years", x="Year", y="Total Deaths")
DataCardio

#Lets plot the number 2 killer to see progression over the years
DataNeo <- DataNew %>%
  filter(cause == 'neoplasms' & country == 'World') %>%
  ggplot(aes(x=year, y=death)) +
  geom_line(color="darkred") +
  labs(subtitle = "Neoplasms Over the Years", x="Year", y="Total Deaths")
DataNeo

#Lets plot the number 3 killer to see progression over the years
DataCRD <- DataNew %>%
  filter(cause == 'chronic_respiratory_diseases' & country == 'World') %>%
  ggplot(aes(x=year, y=death)) +
  geom_line(color="darkblue") +
  labs(subtitle = "Chronic Respiratory Diseases Over the Years", x="Year", y="Total Deaths")
DataCRD

#Lets check the country with most deaths 
DeathPerCountry <- DataNew %>%
  select(country, code, year, death, cause) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(totaldeath = sum(death)) %>%
  arrange(-totaldeath)
DeathPerCountry

#It seems that our data frame needs more clean-up as there are several non-countries
#World, continents and regions needs to be excluded

DataNewClean <- DataNew %>%
  filter(country != 'World',
         country != 'G20',
         country != 'World Bank Lower Middle Income',
         country != 'World Bank Upper Middle Income',
         country != 'East Asia & Pacific (WB)',
         country != 'Western Pacific Region (WHO)',
         country != 'South-East Asia Region (WHO)',
         country != 'South Asia (WB)',
         country != 'OECD Countries',
         country != 'World Bank High Income',
         country != 'European Region (WHO)',
         country != 'Europe & Central Asia (WB)',
         country != 'Sub-Saharan Africa (WB)',
         country != 'African Region (WHO)',
         country != 'Region of the Americas (WHO)',
         country != 'World Bank Low Income',
         country != 'Eastern Mediterranean Region (WHO)',
         country != 'Latin America & Caribbean (WB)',
         country != 'North America (WB)',
         country != 'Middle East & North Africa (WB)',
         country != 'Middle East & North Africa',
         country != 'South Asia',
         country != 'Sub-Saharan Africa',
         country != 'South America',
         country != 'Southeast Asia',
         country != 'Eastern Europe',
         country != 'North America',
         country != 'Western Europe',
         country != 'Central America & Caribbean',
         country != 'East Asia',
         country != 'Central Asia',
         country != 'Australasia & Oceania',
         country != 'International',
         country != 'West Germany (FRG)',
         country != 'East Germany (GDR)',
         country != 'Western Sahara')

#Lets check again
DeathPerCountry <- DataNewClean %>%
  select(country, code, year, death, cause) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(totaldeath = sum(death)) %>%
  arrange(-totaldeath)
DeathPerCountry

#Obviously, the number of deaths correspond with the population, so we have to calculate the percentages of death
#Lets calculate the deathrate

#Death per country overall
DeathPerCountry <- DataNewClean %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathPerCountry

#Death per country 
DeathPerCause <- DataNewClean %>%
  group_by(country, cause) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathPerCause

#Death per year
DeathPerYear <- DataNewClean %>%
  group_by(country, year) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathPerYear

#Lets take the number one killer and see the deathrate
DeathForCardio <- DataNewClean %>%
  filter(cause == 'cardiovascular_diseases') %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathForCardio

#The country with the highest death rate for cardiovascular diseases are Bulgaria followed by Ukraine and Serbia

#Neoplasms
DeathForNeo <- DataNewClean %>%
  filter(cause == 'neoplasms') %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathForNeo

#Aids
DeathForAids <- DataNewClean %>%
  filter(cause == 'aids') %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathForAids

#Conflict & Terrorism (CAT)
DeathForCAT <- DataNewClean %>%
  filter(cause == 'conflict_and_terrorism') %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(totalpop = sum(population), totaldeath = sum(death), deathrate = percent(totaldeath/totalpop)) %>%
  arrange(-deathrate)
DeathForCAT

#Lets expand this further and plot conflict and terrorism on a map
MapData <- map_data("world")

#Lets change the name of the column 'region'
MapData <- MapData %>%
  rename(country = 'region')

#Some country names are spelled differently between the two datasets, this will create a prblem later on
#Example, Czech Republic and Czechia or Ivory Coast - Cote d'Ivoire
#Lets fix this

MapData[MapData == 'USA'] <- 'United States'
MapData[MapData == 'Democratic Republic of the Congo'] <- 'Democratic Republic of Congo'
MapData[MapData == 'Republic of Congo'] <- 'Congo'
MapData[MapData == 'Ivory Coast'] <- "Cote d'Ivoire"
MapData[MapData == 'UK'] <- 'United Kingdom'
MapData[MapData == 'Czech Republic'] <- 'Czechia'

#lets merge them
DeathMap <- left_join(MapData, DeathForCAT, by="country")
View(DeathMap)

#Lets create our first map
Map1 <- ggplot(DeathMap, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=deathrate),color="black")

#Lets change the colors of the map
Map2 <- Map1 + scale_fill_gradient(name = "Death Rate", low="yellow", high="red", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  labs(subtitle ="Global Death Rate by Conflict & Terrorism", caption="Created by Osama Sarm")
Map2

#Lets do the same thing for Cardiovascular diseases
DeathMap1 <- left_join(MapData, DeathForCardio, by="country")

Map1 <- ggplot(DeathMap1, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=deathrate),color="black")

Map2 <- Map1 + scale_fill_gradient(name = "Death Rate", low="lightblue", high="purple", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  labs(subtitle ="Global Death Rate by Cardiovascular Diseases", caption="Created by Osama Sarm")
Map2

#It seems that there's a trend in eastern europe for high death rate by Cardiovascular diseases

#Lets do the same thing for Aids
DeathMap2 <- left_join(MapData, DeathForAids, by="country")

Map1 <- ggplot(DeathMap2, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=deathrate),color="black")

Map2 <- Map1 + scale_fill_gradient(name = "Death Rate", low="yellow", high="darkred", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  labs(subtitle ="Global Death Rate by Aids", caption="Created by Osama Sarm")
Map2

#Its seems that Aids death rate is high is south africa continent

#This data is huge in size and can be used in many different ways
#to investigate many trends, but I wanted to build a template that can be used by anyone
#so feel free to copy and use this kernal or the code chucks to investigate any trend you see relevant

