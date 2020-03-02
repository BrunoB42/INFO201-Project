
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

data_set_to_display <- data_set %>% head(15) %>% select(-indicator)
current_cor <- cor(data_set$value,data_set$Happiness.score)

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
change_in_happ_no_outlier <- filter(change_in_happ,Country!="Sierra Leone") %>% filter(Country!="Malawi")

Change_in_Education_Happiness_Plot <- ggplot(data = change_in_happ_no_outlier, mapping = aes(x=change_in_education,y=Changes.in.happiness.scores)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Change in Happiness and School Enrollment from 2017 to 2018 by Country") +
  xlab("Change in Primary School Enrollment %") +
  ylab("Change in Relative Happiness")

change_in_happ_to_display <- change_in_happ %>% head(15)
change_cor <- cor(change_in_happ$change_in_education, change_in_happ$Changes.in.happiness.scores)

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
Change_in_Happiness_Summary <- as.list(summary(change_in_happ$Changes.in.happiness.scores))

edu_duration_raw <- wb(country = "countries_only", indicator = "SE.COM.DURS", mrv = 10, cache = updated_cache) %>% head(12)
happ_data_raw <- read.csv("data/CountryHappiness2018.csv", stringsAsFactors = FALSE) %>% head(12)

# Grace Section

# 1) In my analysis, I used two different data sets: data in regards to how much countries receive foreign direct investment (FDI), 
# which was reported by the World Bank, and data from the World Happiness Report published by the United Nations (UN), focusing on perceptions of corruption
# and democratic quality. Specifically, I analyzed data about South Korea, and how the values have changd with respect to time. 

# 2) The two data sets come from the World Bank (list of indicators) and the UN's World Happiness Report. 
# Numerous organizations, such as International Monetary Fund (IMF), International Financial Statistics and Balance of Payments databases, 
# and International Debt Statistics collaborated with the World Bank to collect data on the countries' FDI quantities.
# For the UN report, the UN utilizes Gallup World Polls for some measures. In addition, sample sizes are usually 2,000-3,000 people for each country with a confidence interval of 95%.
# Here are the links for the two databases that I explored throughout this project: 
# * https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS?end=2018&start=1970&view=chart
# * https://worldhappiness.report/ed/2019/

# 3) Sample data set from World Bank:
fdi_data <- wb(country = "KOR", indicator = "BX.KLT.DINV.WD.GD.ZS", mrv = 13, return_wide = TRUE)

# Sample data set from UN:
happiness_df <- read.csv("data/UNRawHappinessData.csv", stringsAsFactors = FALSE)
southkorea_happiness <- happiness_df %>% 
  filter(Country.name == "South Korea") %>% 
  select(Year, Perceptions.of.corruption) 

# 4) In terms of the FDI data, the column labeled "BX.KLT.DINV.WD.GD.ZS" represents FDI net inflows (% of GDP).
# This means that FDI is the net inflows of investment to acquire a lasting management interest
# (10% or more of voting stock) in an enterprise operating in an economy besides the investor.
# This FDI includes, but not limited to: 
# * Sum of equity capital 
# * Reinvestment of earnings
# * Long-term capital
# * Short-term capital
# The net inflows are divided by the respective country's gross domestic product(GDP). 

# In terms of the UN's World Happiness Report, I focused on two specific columns: Perceptions of Corruption and Democratic Quality.
# The corruption values were collected by Transparency International, and measures the perceived degree of corruption in public sectors.
# Lower numbers mean that the public views high degrees of corruption within the government, while higher numbers indicate relative improvement. 
# For democratic quality, this includes six measures: 
# * Voice and accountability
# * Political stability and absence of violence
# * Government effectiveness
# * Regulatory quality
# * Rule of law
# * Control of corruption
# Similar to the corruption scale, lower numbers mean more undemocratic conditions, while higher numbers mean more democratic conditions.
# Perceptions of corruption and democratic quality contribute to a country's overall happiness 
# because the government can exercise huge influence over a citizen's life based on decisions, such as specific policies i.e. health care, voting rights, etc.

# Subsection 2.2 Summary Analysis

# 1) Summary of descriptive statistics
corruption_mean <- summarize(southkorea_happiness, Perceptions_of_Corruption_Avg = mean(southkorea_happiness$Perceptions.of.corruption))
corruption_max <- summarize(southkorea_happiness, Perceptions_of_Corruption_Max = max(southkorea_happiness$Perceptions.of.corruption))
corruption_min <- summarize(southkorea_happiness, Perceptions_of_Corruption_Min = min(southkorea_happiness$Perceptions.of.corruption, na.rm = TRUE))

# 2) Graphic or plot showing distribution/trend
sk_corruption_line <- ggplot(data = southkorea_happiness) + 
  geom_point(mapping = aes(x = Year, y = Perceptions.of.corruption)) +
  geom_path(mapping = aes(x = Year, y = Perceptions.of.corruption)) +
  labs(title = "Perceived Corruption Throughout Time in South Korea's Public Sector", x = "Year", y = "Perceived Level of Corruption")

# 3) Any outliers?
# There are no outliers. Most of the values range from 0.75-0.87, which indicates that South Korean citizens do not perceive as much corruption from their government. 

fdi_data <- wb(country = "KOR", indicator = "BX.KLT.DINV.WD.GD.ZS", mrv = 13, return_wide = TRUE) %>% # Gather data about South Korea's FDI over the past 12 years  
  rename("FDI" = BX.KLT.DINV.WD.GD.ZS) %>% # Renaming columns
  rename("Year" = date)
fdi_mean <- summarize(fdi_data, FDI_Average = mean(fdi_data$FDI)) # Find average

happiness_df <- read.csv("data/UNRawHappinessData.csv", stringsAsFactors = FALSE) # Collect data from the UN
southkorea_happiness <- happiness_df %>% 
  filter(Country.name == "South Korea") %>% # Only view South Korea's data
  select(Year, Perceptions.of.corruption, Democratic.Quality) # View only two columns

combined_df <- merge(fdi_data, southkorea_happiness, by.x = "Year", sort = TRUE) %>%  # Make a single table
  rename("Country" = country) %>% 
  rename("Perceptions of Corruption" = Perceptions.of.corruption) %>% 
  rename("Democratic Quality" = Democratic.Quality) %>% 
  select("Year", "FDI", "Perceptions of Corruption", "Democratic Quality")

southkorea_fdi_happiness_lineplot <- ggplot(data = combined_df) + # Plot three lines to compare South Korea's FDI, perceived corruption, and democratic quality
  geom_point(mapping = aes(x = Year, y = combined_df$FDI, color = "FDI")) +
  geom_point(mapping = aes(x = Year, y = combined_df$`Perceptions of Corruption`, color = "Perceptions of Corruption")) +
  geom_point(mapping = aes(x = Year, y = combined_df$`Democratic Quality`, color = "Democratic Quality")) +
  geom_path(mapping = aes(x = Year, y = FDI, group = 1, color = "FDI")) +
  geom_path(mapping = aes(x = Year, y = combined_df$`Perceptions of Corruption`, group = 1, color = "Perceptions of Corruption")) +
  geom_path(mapping = aes(x = Year, y = combined_df$`Democratic Quality`, group = 1, color = "Democratic Quality")) +
  labs(title = "Foreign Direct Investment (FDI) vs. Government Quality in South Korea", x = "Year", y = "Variables", color = "Variables") 

# Jennifer Section
health_expenditure <- wb(country = "countries_only", cache = updated_cache, indicator = c("SH.XPD.CHEX.GD.ZS"), mrv = 20) 
happy_df <- read.csv('data/UNRawHappinessData.csv', stringsAsFactors = FALSE)
colnames(happy_df)[1] <-  "country"
# made sure that countries in health data and happy data matched
countries_in_both <- intersect(health_expenditure$country, happy_df$country)
health_expenditure <- filter(health_expenditure, health_expenditure$country %in% countries_in_both)
happy_df <- filter(happy_df, happy_df$country %in% countries_in_both)

#joined happy and health expenditure data together
spend_expect <- left_join(health_expenditure, happy_df, by = "country")
#produced statistics for value and life expectancy and turned into list to extract from
spending_stats <- as.list(summary(spend_expect$value))
life_stats <- as.list(summary(spend_expect$Healthy.life.expectancy.at.birth))

#joined happy and health expenditure, calculated average % of GDP spent and average life expectancy
expectancy <- left_join(health_expenditure, happy_df, by = "country") %>% 
  group_by(iso3c) %>% 
  summarise(
    spending = mean(value),
    Avg_life_expectancy=mean(Healthy.life.expectancy.at.birth)
  )

# Got summary statistics and correlation of columns
stats <- select(expectancy, spending , Avg_life_expectancy ) 
correlation <- as.list(cor(stats))
summary_spend <- as.list(summary(expectancy$spending))
summary_life<- as.list(summary(expectancy$Avg_life_expectancy))
  
#Created a data table with the first 20 countries
expectancy_table <- left_join(health_expenditure, happy_df, by = "country") %>% 
  group_by(iso3c) %>% 
  summarise(
    spending = mean(value),
    Avg_life_expectancy=mean(Healthy.life.expectancy.at.birth)
  ) %>% 
  head(10)
expectancy_spend <- arrange(expectancy, iso3c) %>% 
  select(iso3c, spending) %>% 
  filter(!is.na(spending)) %>% 
  head(30)
  
#Plotted a bar graph of the average % of GDP spent per country
Country_spend <-ggplot(data = expectancy_spend, mapping = aes(x = reorder(iso3c, spending), y = spending)) + 
  geom_col()+
  labs(title= "Health Care Expenditures per Country", x = "Country", y = "Spending(% of GDP)")+
  theme(axis.text.x = element_text(size= 5,angle= 90))

#got summary statistics of average life expectancy
summary_stats <- as.list(summary(expectancy$Avg_life_expectancy))

#Created histogram of life expectancy from original data.
life_plot <- ggplot(data=spend_expect, aes(spend_expect$Healthy.life.expectancy.at.birth)) +
  geom_histogram(color="darkblue", fill="lightblue")+
  labs(title = "Histogram for Healthy Life Expectancy", x = "Life Expectancy", y = "Frequency")
#Created histogram from calculated average life expectancy
life_average_plot <- ggplot(data=expectancy, aes(expectancy$Avg_life_expectancy)) +
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 4)+
  labs(title = "Histogram for Average Life Expectancy", x = "Average Life Expectancy", y = "Frequency")
  
#Compares Health Care Spending(% of GDP) to Healthy Life Expectancy for countries
Health_plot <- ggplot(data = expectancy, mapping = aes(x = spending, y = Avg_life_expectancy))+
  geom_point()+
  geom_smooth(method = "lm" , formula = y~x)+
  labs(title= "Health Care Expenditures Compared to Healthy Life Expectancy", x = "Average Spending(% of GDP)", y = "Healthy Life Expectancy")+
  xlim(1,17)

# Tony Section


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
  # Multiplied the percentages by 100 so the plot is easier to compare.
  mutate(Perceptions.of.corruption = Perceptions.of.corruption * 100)

# Left joined the corruption data frame to the debt data frame, sorting by the year. And since
# the happiness data only dates back to 2006, filtered out the years for greater than or equal
# to 2006.
debt_corruption <- left_join(us_debt_df, corruption_happiness_df, by = "Year") %>%
  filter(Year >= 2006)

# Calculated corruption
debt_corruption_correlation <- debt_corruption %>%
  select(debt, Perceptions.of.corruption) %>%
  cor()

# Gathered the two datasets so I can plot it.
debt_corruption_gather <- debt_corruption %>%
  gather(key = Type,
         value = percentage,
         -Year)

# Used line and point plots to show changed over time and compare the two datas together.
us_debt_corruption_plot <- ggplot(data = debt_corruption_gather) +
  geom_line(mapping = aes(x = Year, y = percentage, color = Type)) +
  geom_point(mapping = aes(x = Year, y = percentage, color = Type)) +
  geom_smooth(mapping = aes(x = Year, y = percentage), method = "lm", formula = y ~ x) +
  labs(title = "US Debt vs. Perception of Corruption",
       x = "Year",
       y = "Value",
       color = "Group") +
  scale_color_discrete(labels = c("Debt, Total (% of GDP)", "Perceptions of Corruption Score"))

us_debt_preanalysis <- us_debt_df %>%
  rename(date = Year) %>% 
  filter(date >= 2006) %>%
  #mutate(debt = GC.DOD.TOTL.GD.ZS, date = as.numeric(date)) %>%
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

