# Question Analysis for INFO 201 Project

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
edu_data_2017 <- filter(full_edu_data, date=="201`7")
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
fdi_data <- wb(country = "KOR", indicator = "BX.KLT.DINV.WD.GD.ZS", mrv = 13, return_wide = TRUE) %>%   
  rename("FDI" = BX.KLT.DINV.WD.GD.ZS) %>% 
  rename("Year" = date)
fdi_mean <- summarize(fdi_data, FDI_Average = mean(fdi_data$FDI)) 

happiness_df <- read.csv("data/UNRawHappinessData.csv", stringsAsFactors = FALSE)
southkorea_happiness <- happiness_df %>% 
  filter(Country.name == "South Korea") %>% 
  select(Year, Perceptions.of.corruption, Democratic.Quality) 

combined_df <- merge(fdi_data, southkorea_happiness, by.x = "Year", sort = TRUE) %>% 
  rename("Country" = country) %>% 
  rename("Perceptions of Corruption" = Perceptions.of.corruption) %>% 
  rename("Democratic Quality" = Democratic.Quality) %>% 
  select("Year", "FDI", "Perceptions of Corruption", "Democratic Quality")

southkorea_fdi_happiness_lineplot <- ggplot(data = combined_df) +
  geom_point(mapping = aes(x = Year, y = combined_df$FDI, color = "FDI")) +
  geom_point(mapping = aes(x = Year, y = combined_df$`Perceptions of Corruption`, color = "Perceptions of Corruption")) +
  geom_point(mapping = aes(x = Year, y = combined_df$`Democratic Quality`, color = "Democratic Quality")) +
  geom_path(mapping = aes(x = Year, y = FDI, group = 1, color = "FDI")) +
  geom_path(mapping = aes(x = Year, y = combined_df$`Perceptions of Corruption`, group = 1, color = "Perceptions of Corruption")) +
  geom_path(mapping = aes(x = Year, y = combined_df$`Democratic Quality`, group = 1, color = "Democratic Quality")) +
  labs(title = "Relationship Between Foreign Direct Investment (FDI) and Government Quality in South Korea", x = "Year", y = "Variables", color = "Variables") 



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
# Called the World Bank Dataset regarding the percentage of debt in the US in the last 20 years.
us_debt_df <- wb(country = "USA",
                 indicator = c("GC.DOD.TOTL.GD.ZS"),
                 mrv = 20,
                 return_wide = TRUE)

# Renamed the "date" column to "Year" so it matches the happiness data set, and made the values
# as numbers rather than dates.
us_debt_df <- us_debt_df %>%
  mutate(date = as.numeric(date), "Year" = date, date = NULL)

# Renamed the indicator column to "debt" and selected only the relevant columns.
us_debt_df <- us_debt_df %>%
  mutate(debt = GC.DOD.TOTL.GD.ZS) %>%
  select(Year, debt)

# Called the happiness data and filtered it for perceptions of corruption.
happiness_df <- read.csv('data/UNRawHappinessData.csv', stringsAsFactors = FALSE)

corruption_happiness_df <- happiness_df %>%
  filter(Country.name == "United States") %>%
  select(Year, Perceptions.of.corruption) %>%
  # Multiplied the percentages by 100 so it matches the debt percentages.
  mutate(Perceptions.of.corruption = Perceptions.of.corruption * 100)

# Left joined the corruption data frame to the debt data frame, sorting by the year. And since
# the happiness data only dates back to 2006, filtered out the years for greater than or equal
# to 2006.
debt_corruption <- left_join(us_debt_df, corruption_happiness_df, by = "Year") %>%
  filter(Year >= 2006)

# Gathered the two datasets so I can plot it.
debt_corruption_gather <- debt_corruption %>%
  gather(key = Type,
         value = percentage,
         -Year)

# Used line and point plots to show changed over time and compare the two datas together.
us_debt_corruption_plot <- ggplot(data = debt_corruption_gather) +
  geom_line(mapping = aes(x = Year, y = percentage, color = Type)) +
  geom_point(mapping = aes(x = Year, y = percentage, color = Type)) +
  labs(title = "US Debt vs. Perception of Corruption",
       x = "Year",
       y = "Percentage",
       color = "Group") +
  scale_color_discrete(labels = c("Perceptions of Corruption", "Debt, Total (% of GDP)"))





