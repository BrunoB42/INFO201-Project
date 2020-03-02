# Data Description for INFO 201 Project

library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)

# Bruno Section

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
fdi_mean <- summarize(fdi_data, FDI_Avg = mean(fdi_data$BX.KLT.DINV.WD.GD.ZS))
corruption_max <- summarize(southkorea_happiness, Perceptions_of_Corruption_Max = max(southkorea_happiness$Perceptions.of.corruption))
democratic_min <- summarize(southkorea_happiness, Democratic_Quality_Min = min(southkorea_happiness$Democratic.Quality, na.rm = TRUE))

# 2) Graphic or plot showing distribution/trend
sk_corruption_line <- ggplot(data = southkorea_happiness) + 
  geom_point(mapping = aes(x = Year, y = Perceptions.of.corruption)) +
  geom_path(mapping = aes(x = Year, y = Perceptions.of.corruption)) +
  labs(title = "Perceived Corruption Throughout Time in South Korea's Public Sector", x = "Year", y = "Perceived Level of Corruption")

# 3) Any outliers?
# There are no outliers. Most of the values range from 0.75-0.90, which indicates that South Korean citizens do not perceive as much corruption from their government. 

# Jennifer Section

# Tony Section