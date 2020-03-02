
# Data Analysis for INFO 201 Project

library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)

# Bruno Section



# Grace Section



# Jennifer Section



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
