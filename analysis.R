# Data Analysis for INFO 201 Project

library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)

# Bruno Section



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



# Tony Section