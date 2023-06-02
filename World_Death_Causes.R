#Lets start installing the needed packages
install.packages("tidyverse")

#lets load the packages
library("tidyverse")
library("ggplot2")
library("dplyr")

#lets load the data
#renaming our data
Data <- annual_deaths_by_causes

#renaming our columns
Data_New <- Data %>%
  rename(alzheimer_disease = "alzheimer's_diesease") %>%
  rename(parkinsons_disease = "parkinson's_disease") %>%
  rename(aids = "hiv/aids")

#Calculating the total per year
Data_New <- Data_New %>%
  mutate(total_per_year = meningitis + alzheimer_disease + parkinsons_disease + nutritional_deficiency +
           malaria + drowning + interpersonal_violence + maternal_disorders +
           aids + drug_use_disorders + tuberculosis + cardiovascular_diseases +
           lower_respiratory_infections + neonatal_disorders + alcohol_use_disorders +
           self_harm + exposure_to_forces_of_nature + diarrheal_diseases +
           environmental_heat_and_cold_exposure + neoplasms + conflict_and_terrorism +
           diabetes_mellitus + chronic_kidney_disease + poisonings +
           protein_energy_malnutrition + terrorism + road_injuries + chronic_respiratory_diseases +
           chronic_liver_diseases + digestive_diseases + fire_heat_hot_substance +
           acute_hepatitis)


#Let's look into total death by country

Global_Death <- Data_New %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(total_death = sum(total_per_year)) %>%
  arrange(-total_death)

View(Global_Death)

#Analyze the data
#We have 22 illnesses & 10 circumstantial deaths

#1.Calculating the leading cause of death globally

Data_New_1 <- Data_New %>%
  filter(country=='World') %>%
  group_by(year)

View(Data_New_1)

#After creating a plot for each death, those are the top three

ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=cardiovascular_diseases),color="blue")+
  geom_point(mapping=aes(x=year,y=neoplasms),color="pink")+
  geom_point(mapping=aes(x=year,y=chronic_respiratory_diseases),color="black")+
  theme_bw()+labs(subtitle="Illnesses with the hightest deaths",x="Year",y="Illness")+
  annotate("text",x=1995,y=15000000,label="Cardiovascular Diseases")+
  annotate("text",x=2000,y=9000000,label="Neoplasms")+
  annotate("text",x=2010,y=5000000,label="Respiratory Diseases")
  
#Conclusion: the leading cause of death globally is cardiovascular diseases followed by neoplasms & chronic_respiratory_diseases

ggplot(Data_New_1, mapping=aes(x=year, y=cardiovascular_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Cardiovascular Diseases", x="Year",y="Number of deaths")

#2.Calculating the year with most deaths

ggplot(Data_New_1, mapping=aes(x=year,y=total_per_year,color=year))+geom_point()+geom_smooth()+
  theme_bw()+labs(subtitle = "Total deaths over the years",x="Year",y="Deaths")

#The deaths are increasing on an annual basis, so the most recent year available of 2019 is the year with the highest number of death

#3.Calculating the top five killer illnesses with trends over the years

ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=meningitis),color="blue")+
  geom_point(mapping=aes(x=year,y=alzheimer_disease),color="darkred")+
  geom_point(mapping=aes(x=year,y=parkinsons_disease),color="black")+
  geom_point(mapping=aes(x=year,y=nutritional_deficiency),color="green")+
  geom_point(mapping=aes(x=year,y=malaria),color="lightgreen")+
  geom_point(mapping=aes(x=year,y=maternal_disorders),color="darkgreen")+
  geom_point(mapping=aes(x=year,y=aids),color="red")+
  geom_point(mapping=aes(x=year,y=drug_use_disorders),color="yellow")+
  geom_point(mapping=aes(x=year,y=tuberculosis),color="grey")+
  geom_point(mapping=aes(x=year,y=diarrheal_diseases),color="white")+
  geom_point(mapping=aes(x=year,y=diabetes_mellitus),color="purple")+
  geom_point(mapping=aes(x=year,y=chronic_kidney_disease),color="orange")+
  geom_point(mapping=aes(x=year,y=protein_energy_malnutrition),color="brown")+
  geom_point(mapping=aes(x=year,y=chronic_liver_diseases),color="cyan")+
  geom_point(mapping=aes(x=year,y=digestive_diseases),color="navy")+
  geom_point(mapping=aes(x=year,y=acute_hepatitis),color="maroon")+
  labs(subtitle="Illnesses",x="Year",y="Illness")+theme_bw()

#Top illnesses are:
#cardiovascular_diseases at no.1
#neoplasms at no.2
#chronic_respiratory_diseases at no.3
#diarrheal_diseases at no.4
#digestive_diseases at no. 5

#Let's look at the trends over the years
#Starting with cardiovascular_diseases
ggplot(Data_New_1, mapping=aes(x=year, y=cardiovascular_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Cardiovascular Diseases Over the Years", x="Year",y="Number of deaths")

#Then neoplasms
ggplot(Data_New_1, mapping=aes(x=year, y=neoplasms,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Neoplasms Diseases Over the Years", x="Year",y="Number of deaths")

#Then chronic_respiratory_diseases
ggplot(Data_New_1, mapping=aes(x=year, y=chronic_respiratory_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Chronic Respiratory Diseases Over the Years", x="Year",y="Number of deaths")

#Then diarrheal_diseases
ggplot(Data_New_1, mapping=aes(x=year, y=diarrheal_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Diarrheal Diseases Over the Years", x="Year",y="Number of deaths")

#Then digestive_diseases
ggplot(Data_New_1, mapping=aes(x=year, y=digestive_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Digestive Diseases Over the Years", x="Year",y="Number of deaths")

#It seems that the top five illnesses are only increasing per year except Diarheal which is decreasing over the years

#4.Calculating the top five circumstantial deaths globally

ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=drowning),color="blue")+
  geom_point(mapping=aes(x=year,y=interpersonal_violence),color="darkred")+
  geom_point(mapping=aes(x=year,y=self_harm),color="black")+
  geom_point(mapping=aes(x=year,y=exposure_to_forces_of_nature),color="green")+
  geom_point(mapping=aes(x=year,y=environmental_heat_and_cold_exposure),color="red")+
  geom_point(mapping=aes(x=year,y=conflict_and_terrorism),color="navy")+
  geom_point(mapping=aes(x=year,y=poisonings),color="grey")+
  geom_point(mapping=aes(x=year,y=terrorism),color="white")+
  geom_point(mapping=aes(x=year,y=road_injuries),color="purple")+
  geom_point(mapping=aes(x=year,y=fire_heat_hot_substance),color="orange")+
  labs(subtitle="Circumstantial Deaths",x="Year",y="Number of Deaths")+theme_bw()

#Top five circumstantial deaths are:
#Road Injuries
#Self harm
#Interpersonal violence
#Drowning
#Conflict & Terrorism

#Let's look at the trend over the years
#Starting with Road Injuries
ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=road_injuries,color=year))+
  labs(subtitle="Deaths by Road Injuries",x="Year",y="Number of Deaths")+theme_dark()

#Then Self harm
ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=self_harm,color=year))+
  labs(subtitle="Deaths by Self-Harm",x="Year",y="Number of Deaths")+theme_dark()

#Then Interpersonal violence
ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=interpersonal_violence,color=year))+
  labs(subtitle="Deaths by Interpersonal Violence",x="Year",y="Number of Deaths")+theme_dark()

#Then Drowning
ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=drowning,color=year))+
  labs(subtitle="Deaths by Drowning",x="Year",y="Number of Deaths")+theme_dark()

#Then Conflict & Terrorism
ggplot(Data_New_1)+geom_point(mapping=aes(x=year,y=conflict_and_terrorism,color=year))+
  labs(subtitle="Deaths by Conflict and Terrorism",x="Year",y="Number of Deaths")+theme_dark()

#Iraq in depth
#Creating a new dataframe for Iraq

Data_Iraq <- Data_New %>%
  filter(country=='Iraq')

View(Data_Iraq)

#Checking the illness that causes the most deaths

ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=meningitis),color="blue")+
  geom_point(mapping=aes(x=year,y=alzheimer_disease),color="darkred")+
  geom_point(mapping=aes(x=year,y=parkinsons_disease),color="black")+
  geom_point(mapping=aes(x=year,y=nutritional_deficiency),color="green")+
  geom_point(mapping=aes(x=year,y=malaria),color="lightgreen")+
  geom_point(mapping=aes(x=year,y=maternal_disorders),color="darkgreen")+
  geom_point(mapping=aes(x=year,y=aids),color="red")+
  geom_point(mapping=aes(x=year,y=drug_use_disorders),color="yellow")+
  geom_point(mapping=aes(x=year,y=tuberculosis),color="grey")+
  geom_point(mapping=aes(x=year,y=protein_energy_malnutrition),color="brown")+
  geom_point(mapping=aes(x=year,y=chronic_liver_diseases),color="cyan")+
  geom_point(mapping=aes(x=year,y=acute_hepatitis),color="maroon")+
  geom_point(mapping=aes(x=year,y=chronic_respiratory_diseases),color="gold")+
  geom_point(mapping=aes(x=year,y=diarrheal_diseases),color="navy")+
  labs(subtitle="Deaths in Iraq by Illnesses",x="Year",y="Illness")+theme_bw()


#Top 5 Illnesses seems to be:
#cardiovascular_diseases
ggplot(Data_Iraq, mapping=aes(x=year, y=cardiovascular_diseases,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Cardiovascular Diseases Over the Years", x="Year",y="Number of deaths")

#neoplasms
ggplot(Data_Iraq, mapping=aes(x=year, y=neoplasms,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Neoplasms Diseases Over the Years", x="Year",y="Number of deaths")

#diabetes_mellitus
ggplot(Data_Iraq, mapping=aes(x=year, y=diabetes_mellitus,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Diabetes Mellitus Diseases Over the Years", x="Year",y="Number of deaths")

#chronic_kidney_disease
ggplot(Data_Iraq, mapping=aes(x=year, y=chronic_kidney_disease,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Chronic Kidney Diseases Over the Years", x="Year",y="Number of deaths")

#digestive_diseases
ggplot(Data_Iraq, mapping=aes(x=year, y=neoplasms,color=year))+
  geom_point()+geom_smooth()+theme_bw()+labs(subtitle="Digestive Diseases Over the Years", x="Year",y="Number of deaths")

#Checking circumstantial deaths

ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=drowning),color="blue")+
  geom_point(mapping=aes(x=year,y=interpersonal_violence),color="darkred")+
  geom_point(mapping=aes(x=year,y=self_harm),color="black")+
  geom_point(mapping=aes(x=year,y=exposure_to_forces_of_nature),color="green")+
  geom_point(mapping=aes(x=year,y=environmental_heat_and_cold_exposure),color="red")+
  geom_point(mapping=aes(x=year,y=poisonings),color="grey")+
  geom_point(mapping=aes(x=year,y=road_injuries),color="purple")+
  geom_point(mapping=aes(x=year,y=fire_heat_hot_substance),color="orange")+
  labs(subtitle="Deaths by Circumstance",x="Year",y="Numbers of Deaths")+theme_bw()

#Removing conflict_and_terrorism and terrorism

#The highest seem to be conflict_and_terrorism as no.1
ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=conflict_and_terrorism,color=year))+
  labs(subtitle="Deaths by Conflict and Terrorism",x="Year",y="Number of Deaths")+theme_dark()

#Then road_injuries at no.2
ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=road_injuries,color=year))+
  labs(subtitle="Deaths by Road Injuries",x="Year",y="Number of Deaths")+theme_dark()

#Then terrorism at no.3
ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=terrorism,color=year))+
  labs(subtitle="Deaths by Terrorism",x="Year",y="Number of Deaths")+theme_dark()

#Then interpersonal violence no.4
ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=interpersonal_violence,color=year))+
  labs(subtitle="Deaths by Interpersonal Violence",x="Year",y="Number of Deaths")+theme_dark()

#Then self_harm
ggplot(Data_Iraq)+geom_point(mapping=aes(x=year,y=self_harm,color=year))+
  labs(subtitle="Deaths by Self-Harm",x="Year",y="Number of Deaths")+theme_dark()

#Let's look into death over the years for Iraq

ggplot(Data_Iraq,mapping=aes(x=year,y=total_per_year))+geom_point()+
  labs(subtitle="Deaths in Iraq Over the Years",x="Year",y="Death per Year")+theme_bw()

#Terrorsim and Conflict

#Let's now look into global death by terrorism

Death_by_Terror <- Data_New %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(terror_death = sum(terrorism)) %>%
  arrange(-terror_death)

tibble(Death_by_Terror)

#Let's look into global death by conflict and terrorism

Death_by_CnT <- Data_New %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(cnt_death = sum(conflict_and_terrorism)) %>%
  arrange(-cnt_death)

tibble(Death_by_CnT)

#Let's group by year to graph them

Death_by_Terror2 <- Data_New %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(terror_death = sum(terrorism)) %>%
  arrange(-terror_death)

tibble(Death_by_Terror2)

#Now for Conflict and terrorsim

Death_by_CnT2 <- Data_New %>%
  group_by(year) %>%
  drop_na() %>%
  summarise(cnt_death = sum(conflict_and_terrorism)) %>%
  arrange(-cnt_death)

tibble(Death_by_CnT2)

#Let's compare them
ggplot()+
  geom_point(data=Death_by_Terror2, aes(x=year,y=terror_death,color=year,size=terror_death))+
  geom_point(data=Death_by_CnT2, aes(x=year,y=cnt_death,color=year,size=cnt_death))+theme_grey()+
  labs(subtitle="Comparing Terror to Conflict & Terror",x="Year",y="Terrorism & Conflict Combined")

#There seem to be an outlier in 1994. Let's investigate this further
#Let's sample our data to filter only 1994

Data_1994 <- Data_New %>%
  filter(year == '1994')

tibble(Data_1994)

#Let's check out which countries that were effected in 1994

Death_by_Terror3 <- Data_1994 %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(terror_death = sum(terrorism)) %>%
  arrange(-terror_death)

tibble(Death_by_Terror3)

#Terror seem to be normal for 1994

#Let's check Conflict and Terrorism

Death_by_CnT3 <- Data_1994 %>%
  group_by(country) %>%
  drop_na() %>%
  summarise(cnt_death = sum(conflict_and_terrorism)) %>%
  arrange(-cnt_death)

tibble(Death_by_CnT3)

#The outlier comes from Rwanda due to Rwanda genocide in 1994 where more than 500,000 died in that genocide
