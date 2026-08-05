library(tidyverse)
library(ggplot2)
library(scales)
library(here)

health_data <- readRDS(
  here("data", "health_data_clean.rds")
)

data_2023 <- health_data %>%
  filter(year == 2023)

nrow(data_2023)

n_distinct(data_2023$iso3c)

health_plot <- ggplot(
  data_2023,
  aes(
    x = health_spending,
    y = life_expectancy
  )
) +
  geom_point(
    aes(color = region),
    size = 2.8,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ log(x),
    se = TRUE,
    color = "#202020",
    linewidth = 0.9
  ) +
  scale_x_log10(
    labels = label_dollar()
  ) +
  labs(
    title = "The Health Spending Paradox",
    subtitle = "Health expenditure and life expectancy across countries, 2023",
    x = "Health expenditure per capita (PPP, current international $)",
    y = "Life expectancy at birth (years)",
    color = "Region",
    caption = "Source: World Bank, World Development Indicators"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey40"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

health_plot

dir.create(here("figures"), showWarnings = FALSE)

ggsave(
  here("figures", "health_spending_paradox.png"),
  plot = health_plot,
  width = 11,
  height = 6,
  dpi = 300
)

health_model <- lm(
  life_expectancy ~ log(health_spending),
  data = data_2023
)

summary(health_model)

data_2023 <- data_2023 %>%
  mutate(
    predicted_life_expectancy = predict(health_model),
    health_performance = life_expectancy -
      predicted_life_expectancy
  )

top_performers <- data_2023 %>%
  arrange(desc(health_performance)) %>%
  select(
    country,
    region,
    health_spending,
    life_expectancy,
    predicted_life_expectancy,
    health_performance
  ) %>%
  slice_head(n = 15)

print(as_tibble(top_performers), n = 15)