
# Data Analysis for INFO 201 Project

library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)

# Bruno Section



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