#Installing Tidyverse
install.packages("tidyverse")

#Loading Packages
library("tidyverse")
library("ggplot2")
library("lubridate")

#Creating a new Dataframe after importing the dataset
Instagram_Data <- Instagram_Data_Osamaex

#Renaming Columns
Instagram_Data <- Instagram_Data %>%  
  rename(Subcategory = `Sub-Category`) %>%  
  rename(SiteLocation = `Location of photograph by Site`) %>%  
  rename(CountryLocation = `Location of photograph by Country`) %>%  
  rename(DateofPost = `Date of Post`) %>%  
  rename(PostTime = `Time of Post`) %>%  
  rename(PhotoID=`Image ID`)

#Q1: Finding a correlation between the type of Photography posted, to the engagement it generates

#Lets look first at the number of categories & sub-categories used
n_distinct(Instagram_Data$Category)
n_distinct(Instagram_Data$Subcategory)

n_dis

#Lets have a look at how many posts per Category was posted
Instagram_Data %>%
  group_by(Category) %>%
  summarise("ncategories" = n()) %>%
  arrange(ncategories, by_group= FALSE)

#Let's see the most common categories by filtering out the rest
Instagram_Data_Filtered <- Instagram_Data %>%
  group_by(Category) %>%
  filter(n() > 20)

#Comparing the most liked Categories vs. the most used
ggplot(Instagram_Data_Filtered)+geom_point(mapping=aes(x=Likes,y=Category),color="blue")
ggsave("Category_Correlation.png")

ggplot(Instagram_Data_Filtered)+geom_bar(mapping=aes(x=Category),fill="blue")+theme(axis.text.x=element_text(angle=45))
ggsave("Categories_Used.png")

#Let's see the most common Sub-categories by filtering out the rest
Instagram_Data_Filtered <- Instagram_Data_Filtered %>%
  group_by(Subcategory) %>%
  filter(n() > 20)

#Comparing the most liked Sub-Categories vs. the most used
ggplot(Instagram_Data_Filtered)+geom_point(mapping=aes(x=Likes,y=Subcategory),color="green")
ggsave("Subcategory_Correlation.png",width=6,height=5)

ggplot(Instagram_Data_Filtered)+geom_bar(mapping=aes(x=Subcategory),fill="green")+theme(axis.text.x=element_text(angle=45))
ggsave("Subcategories_Used.png",width=5,height=8)

#Q2: Finding a correlation between the location of the photo and the engagement
ggplot(Instagram_Data)+geom_point(mapping=aes(x=Likes,y=SiteLocation),color="red")
ggsave("Location_Correlation.png",width=6,height=10)

ggplot(Instagram_Data)+geom_point(mapping=aes(x=Likes,y=CountryLocation),color="orange")
ggsave("Country_Correlation.png")

#Q3: Finding a correlation between the time of post with the engagement
#Lets check if the time is formatted correctly
str(Instagram_Data)
#To convert chr to time I used
Instagram_Data <- Instagram_Data %>%
  mutate(PostTimeNew = strptime(PostTime,format="%I:%M %p"))

#Then I separated just the hours from time by creating another column
Instagram_Data <- Instagram_Data %>%
  mutate(PostH = hour(PostTimeNew))

#Seeing the most common hour for posting
Instagram_Data %>%
  group_by(PostH) %>%
  summarise("ncategories" = n()) %>%
  arrange(ncategories, by_group= FALSE) %>%
  print(n=22)

#Checking if there's a correlation with the hours of posting
ggplot(data=Instagram_Data,aes(x=PostH,y=Likes))+
  geom_line(color="blue")+
  geom_point(color="purple")
ggsave("Line_Chart.png",width=10,height=8)

#Checking if there's a correlation with the period of posing by creating a new column
Instagram_Data <- Instagram_Data %>%
  mutate(time_period= case_when(
    PostH > 06 & PostH < 12 ~ "Morning",
    PostH >= 12 & PostH < 19 ~ "Afternoon",
    PostH >= 19 & PostH < 21 ~ "Evening",
    PostH >=21 | PostH <= 5 ~ "Night"))

#Then creating another chart
ggplot(Instagram_Data)+geom_bar(mapping=aes(x=time_period),fill="orange")
ggsave("time_period.png")

#Q4: Finding a correlation between the gap duration between posts to the enagement it generates
#Lets check if the date column is formatted correctly
str(Instagram_Data)
#Converting the date to a proper date format
Instagram_Data <- Instagram_Data %>%
  mutate(DateofPost = as.Date(DateofPost))

#We have to create a column to calculate the date gap between two posts
Instagram_Data <- Instagram_Data %>%
  mutate(DateDiff = DateofPost - lag(DateofPost)) 

#Let's separate the year
Instagram_Data <- Instagram_Data %>%
  mutate(YearofPost= as.numeric(format(DateofPost,"%Y")))

#Plotting
ggplot(Instagram_Data)+geom_point(mapping=aes(x=Likes,y=DateDiff,color=YearofPost))
ggsave("DateCorrelation.png",width=10,height=5) 


#Q5
#Finding a correlation between the number of posts per month and per year, with the engagement
View(Instagram_Data)

#We need two columns, one for year only, and one for month/year. The year column already exist from Q4
#To separate month/year from date, I used
Instagram_Data <- Instagram_Data %>%
  mutate(YearMonthofPost= (format(DateofPost,"%Y:%m")))

#Lets check the number of posts per year
ggplot(Instagram_Data)+geom_bar(mapping=aes(x=YearofPost),color="black")

#Plotting the engagement with posts per year
ggplot(Instagram_Data)+geom_point(mapping=aes(x=YearofPost,y=Likes,color=YearofPost))

#Let's take a sample of our data to have a more precise look into engagement per month
Instagram_Data_Filtered <- Instagram_Data %>%
  filter(Instagram_Data$YearofPost > 2017 & YearofPost <2022) 

View(Instagram_Data_Filtered)
#Plotting the engagement with number of posts per month
ggplot(Instagram_Data_Filtered)+geom_point(mapping=aes(x=YearMonthofPost,y=Likes,shape=as.factor(YearofPost),color=YearofPost))
ggsave("Filtered Years.png",width=20,height=5)


#Q6
#Finding a correlation between the engagement of last post, with engagement of current post

#We have to create a column for the likes of the last post, we will call it 'likes_Previous'
Instagram_Data <- Instagram_Data %>%
  mutate(Likes_Previous = lag(Likes))

#Lets sample out dataset to have a precise look
Instagram_Data_Filtered <- Instagram_Data %>%
  filter(Instagram_Data$YearofPost > 2019 & YearofPost <2021)

#Comparing the two plots
ggplot(Instagram_Data_Filtered)+geom_point(mapping=aes(x=PhotoID,y=Likes),color="Red")+
  geom_point(mapping=aes(x=PhotoID,y=Likes_Previous),color="Blue")+
  theme(axis.text.x=element_text(angle=45))
ggsave("Likes and PreLikes.png", width=15, height=10)