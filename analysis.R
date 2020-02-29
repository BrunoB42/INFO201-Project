
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

us_debt_df <- us_debt_df %>%
  mutate(date = as.numeric(date), "Year" = date, date = NULL)

us_debt_df <- us_debt_df %>%
  mutate(debt = GC.DOD.TOTL.GD.ZS) %>%
  select(Year, debt)

debt_corruption <- left_join(us_debt_df, corruption_happiness_df, by = "Year") %>%
  filter(Year >= 2006)

debt_corruption_gather <- debt_corruption %>%
  gather(key = Type,
         value = percentage,
         -Year)

us_debt_corruption_plot <- ggplot(data = debt_corruption_gather) +
  geom_line(mapping = aes(x = Year, y = percentage, color = Type)) +
  geom_point(mapping = aes(x = Year, y = percentage, color = Type)) +
  labs(title = "US Debt vs. Perception of Corruption",
       x = "Year",
       y = "Percentage",
       color = "Group") +
  scale_color_discrete(labels = c("Perceptions of Corruption", "Debt, Total (% of GDP)"))





