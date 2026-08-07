library(tidyverse)
library(here)

health_data <- readRDS(
  here("data", "health_data_clean.rds")
)

data_2023 <- health_data %>%
  filter(
    year == 2023,
    !is.na(gdp_per_capita),
    gdp_per_capita > 0
  )

# Baseline model

model_baseline <- lm(
  life_expectancy ~ log(health_spending),
  data = data_2023
)

# Model controlling for GDP per capita

model_gdp <- lm(
  life_expectancy ~ log(health_spending) +
    log(gdp_per_capita),
  data = data_2023
)

summary(model_baseline)
summary(model_gdp)

# Predicted life expectancy and residuals

data_2023 <- data_2023 %>%
  mutate(
    predicted_baseline = predict(model_baseline),
    predicted_gdp = predict(model_gdp),
    performance_baseline = life_expectancy - predicted_baseline,
    performance_gdp = life_expectancy - predicted_gdp
  )

# Ranking after controlling for GDP

top_performers_gdp <- data_2023 %>%
  arrange(desc(performance_gdp)) %>%
  select(
    country,
    region,
    health_spending,
    gdp_per_capita,
    life_expectancy,
    performance_baseline,
    performance_gdp
  ) %>%
  slice_head(n = 15)

print(as_tibble(top_performers_gdp), n = 15)