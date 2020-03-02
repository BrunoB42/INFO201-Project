
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