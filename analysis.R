
# Data Analysis for INFO 201 Project

library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)

# Bruno Section

updated_cache <- wbcache()
edu_duration <- wb(country = "countries_only", indicator = "SE.COM.DURS", mrv = 10, cache = updated_cache) %>% filter(date=="2018") %>% rename(Country = country)
change_in_happ <- read.csv("data/CountryChangeInHappiness2018.csv", stringsAsFactors = FALSE) %>% arrange(Country)
happ_data <- read.csv("data/CountryHappiness2018.csv", stringsAsFactors = FALSE) %>% arrange(Country)
raw_happ_data <- read.csv("data/UNRawHappinessData2018.csv", stringsAsFactors = FALSE) %>% arrange(Country)
#Eliminating data for countries not in all three data sets
countries_in_all_data <- intersect(intersect(change_in_happ$Country, happ_data$Country),edu_duration$Country)
change_in_happ <- filter(change_in_happ, change_in_happ$Country %in% countries_in_all_data)
happ_data <- filter(happ_data, happ_data$Country %in% countries_in_all_data)
edu_duration <- filter(edu_duration, edu_duration$Country %in% countries_in_all_data)
data_set <- left_join(left_join(change_in_happ, happ_data, "Country"), edu_duration, "Country") %>% select(-X,-X.1,-X.2,-X.3,-X.4,-X.5,-X.6,-X.7,-X.8,-X.9,-X.10,-X.11,-X.12)

Current_Education_Happiness_Plot <- ggplot(data = data_set, mapping = aes(x=value, y=Happiness.score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
  labs(title = "Reported Happiness and Years of Compulsory Education") +
  xlab("Years of Compulsory Education") +
  ylab("Relative Happiness")

change_in_happ <- read.csv("data/CountryChangeInHappiness2018.csv", stringsAsFactors = FALSE) %>% arrange(Country) %>% filter(Country!="Sierra Leone")
full_edu_data <- wb(country = "countries_only", indicator = "SE.PRM.ENRR", mrv = 3, cache = updated_cache) %>% 
  filter(date!="2019") %>% 
  rename(Country = country)
#Separating data by year to find change and eliminating data for countries not in all three data sets
edu_data_2018 <- filter(full_edu_data, date=="2018")
edu_data_2017 <- filter(full_edu_data, date=="2017")
countries_in_all_data_2 <- intersect(intersect(edu_data_2017$Country,edu_data_2018$Country), change_in_happ$Country)
edu_data_2018 <- filter(edu_data_2018, edu_data_2018$Country %in% countries_in_all_data_2)
edu_data_2017 <- filter(edu_data_2017, edu_data_2017$Country %in% countries_in_all_data_2)
change_in_happ <- filter(change_in_happ, change_in_happ$Country %in% countries_in_all_data_2)
change_in_happ$change_in_education <- edu_data_2018$value - edu_data_2017$value

Change_in_Education_Happiness_Plot <- ggplot(data = change_in_happ, mapping = aes(x=change_in_education,y=Changes.in.happiness.scores)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Change in Happiness and School Enrollment from 2017 to 2018 by Country") +
  xlab("Change in Primary School Enrollment %") +
  ylab("Change in Relative Happiness")

# Grace Section



# Jennifer Section
updated_cache <- wbcache()
health_expenditure <- wb(country = "countries_only", cache = updated_cache, indicator = c("SH.XPD.CHEX.GD.ZS"), mrv = 20) 
happy_df <- read.csv('data/UNRawHappinessData.csv', stringsAsFactors = FALSE)
colnames(happy_df)[1] <-  "country"
countries_in_both <- intersect(health_expenditure$country, happy_df$country)
health_expenditure <- filter(health_expenditure, health_expenditure$country %in% countries_in_both)
happy_df <- filter(happy_df, happy_df$country %in% countries_in_both)

expectancy <- left_join(health_expenditure, happy_df, by = "country") %>% 
  #filter(date == 2016 & Year == 2016) %>% 
  group_by(iso3c) %>% 
  summarise(
    spending = mean(value),
    Avg_life_expectancy=mean(Healthy.life.expectancy.at.birth)
  )
expectancy_spend <- arrange(expectancy, iso3c) %>% 
  select(iso3c, spending) %>% 
  filter(!is.na(spending)) %>% 
  head(30)
  

Country_spend <-ggplot(data = expectancy_spend, mapping = aes(x = reorder(iso3c, spending), y = spending)) + 
  geom_col()+
  labs(title= "Health Care Expenditures per Country", x = "Country", y = "Spending(% of GDP)")+
  theme(axis.text.x = element_text(size= 5,angle= 90))

  
#Compares Health Care Spending(% of GDP) to Healthy Life Expectancy for High income countries
Health_plot <- ggplot(data = expectancy, mapping = aes(x = spending, y = Avg_life_expectancy))+
  geom_point()+
  geom_smooth(method = "lm" , formula = y~x)+
  labs(title= "Health Care Expenditures Compared to Healthy Life Expectancy", x = "Average Spending(% of GDP)", y = "Healthy Life Expectancy")+
  xlim(1,17)


# Tony Section