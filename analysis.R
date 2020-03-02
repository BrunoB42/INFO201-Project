
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
#raw_happ_data <- read.csv("data/UNRawHappinessData2018.csv", stringsAsFactors = FALSE) %>% arrange(Country)
###Eliminating data for countries not in all three data sets
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

change_in_happ <- read.csv("data/CountryChangeInHappiness2018.csv", stringsAsFactors = FALSE) %>% arrange(Country)
full_edu_data <- wb(country = "countries_only", indicator = "SE.PRM.ENRR", mrv = 3, cache = updated_cache) %>%
  filter(date!="2019") %>%
  rename(Country = country)
###Separating data by year to find change and eliminating data for countries not in all three data sets
edu_data_2018 <- filter(full_edu_data, date=="2018")
edu_data_2017 <- filter(full_edu_data, date=="2017")
countries_in_all_data_2 <- intersect(intersect(edu_data_2017$Country,edu_data_2018$Country), change_in_happ$Country)
edu_data_2018 <- filter(edu_data_2018, edu_data_2018$Country %in% countries_in_all_data_2)
edu_data_2017 <- filter(edu_data_2017, edu_data_2017$Country %in% countries_in_all_data_2)
change_in_happ <- filter(change_in_happ, change_in_happ$Country %in% countries_in_all_data_2)
change_in_happ$change_in_education <- edu_data_2018$value - edu_data_2017$value
change_in_happ_no_outlier <- filter(change_in_happ,Country!="Sierra Leone") %>% filter(Country!="Malawi")

Change_in_Education_Happiness_Plot <- ggplot(data = change_in_happ_no_outlier, mapping = aes(x=change_in_education,y=Changes.in.happiness.scores)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Change in Happiness and School Enrollment from 2017 to 2018 by Country") +
  xlab("Change in Primary School Enrollment %") +
  ylab("Change in Relative Happiness")

###Generating histograms and summaries of data columns for analysis
Education_Hist <- ggplot(data = edu_duration, mapping = aes(x = value)) +
  geom_histogram(binwidth = 1, color="white", fill="black") +
  labs(title = "Years of Compulsory Education by Country Worldwide") +
  xlab("Years of compulsory Education") +
  ylab("Count")
Education_Summary <- summary(edu_duration["value"])

Happiness_Hist <- ggplot(data = happ_data, mapping = aes(x = Happiness.score)) +
  geom_histogram(binwidth = 0.25, color="white", fill="black") +
  labs(title = "Relative Happiness by Country Worldwide") +
  xlab("Relative Reported Happiness") +
  ylab("Count")
Happiness_Summary <- summary(happ_data["Happiness.score"])

Change_in_Education_Hist <- ggplot(data = change_in_happ, mapping = aes(x = change_in_education)) +
  geom_histogram(binwidth = 1, color="white", fill="black") +
  labs(title = "Change in Education from 2017 to 2018 by Country") +
  xlab("Change in Primary School Enrollment %") +
  ylab("Count")
Change_in_Education_Summary <- summary(change_in_happ["change_in_education"])

Change_in_Happiness_Hist <- ggplot(data = change_in_happ, mapping = aes(x = Changes.in.happiness.scores)) +
  geom_histogram(binwidth = 0.2, color="white", fill="black") +
  labs(title = "Change in Happiness from 2017 to 2018 by Country") +
  xlab("Change in Relative Happiness") +
  ylab("Count")
Change_in_Happiness_Summary <- summary(change_in_happ["Changes.in.happiness.scores"])

edu_duration_raw <- wb(country = "countries_only", indicator = "SE.COM.DURS", mrv = 10, cache = updated_cache) %>% head(12)
happ_data_raw <- read.csv("data/CountryHappiness2018.csv", stringsAsFactors = FALSE) %>% head(12)


# Grace Section



# Jennifer Section
updated_cache <- wbcache()
health_expenditure <- wb(country = "countries_only", cache = updated_cache, indicator = c("SH.XPD.CHEX.GD.ZS"), mrv = 20) 
happy_df <- read.csv('data/UNRawHappinessData.csv', stringsAsFactors = FALSE)
colnames(happy_df)[1] <-  "country"
countries_in_both <- intersect(health_expenditure$country, happy_df$country)
health_expenditure <- filter(health_expenditure, health_expenditure$country %in% countries_in_both) 
happy_df <- filter(happy_df, happy_df$country %in% countries_in_both)

spend_expect <- left_join(health_expenditure, happy_df, by = "country")
spending_stats <- as.list(summary(spend_expect$value))
life_stats <- as.list(summary(spend_expect$Healthy.life.expectancy.at.birth))

expectancy <- left_join(health_expenditure, happy_df, by = "country") %>% 
  group_by(iso3c) %>% 
  summarise(
    spending = mean(value),
    Avg_life_expectancy=mean(Healthy.life.expectancy.at.birth)
  ) 

summary_stats <- as.list(summary(expectancy$Avg_life_expectancy))

life_plot <- ggplot(data=spend_expect, aes(spend_expect$Healthy.life.expectancy.at.birth)) +
  geom_histogram(color="darkblue", fill="lightblue")+
  labs(title = "Histogram for Healthy Life Expectancy", x = "Life Expectancy", y = "Frequency")

life_average_plot <- ggplot(data=expectancy, aes(expectancy$Avg_life_expectancy)) +
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 4)+
  labs(title = "Histogram for Average Life Expectancy", x = "Average Life Expectancy", y = "Frequency")



# Tony Section

us_debt_df <- wb(country = "USA",
                 indicator = c("GC.DOD.TOTL.GD.ZS"),
                 mrv = 20,
                 return_wide = TRUE)

us_debt_preanalysis <- us_debt_df %>%
  filter(date >= 2006) %>%
  mutate(debt = GC.DOD.TOTL.GD.ZS, date = as.numeric(date)) %>%
  select(date, debt)

us_debt_summary <- us_debt_preanalysis %>%
  summarize(mean = mean(debt),
            range = us_debt_preanalysis[11, "debt"] - us_debt_preanalysis[1, "debt"])

us_debt_correlation <- cor(us_debt_preanalysis)

us_debt_over_time_plot <- ggplot(data =  us_debt_preanalysis) +
  geom_line(mapping = aes(x = date, y = debt)) +
  geom_point(mapping = aes(x = date, y = debt)) +
  labs(title = "US Debt Over Time",
       x = "Year",
       y = "Debt Percentage of GDP")

us_debt_preanalysis[1, "debt"]
us_debt_preanalysis[11, "debt"]
