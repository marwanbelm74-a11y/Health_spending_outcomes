library(tidyverse)
library(here)

health_data <- readRDS(
  here("data", "health_data_clean.rds")
)

data_2023 <- health_data %>%
  filter(
    year == 2023,
    !is.na(gdp_per_capita),
    gdp_per_capita > 0,
    !is.na(health_spending),
    health_spending > 0,
    !is.na(life_expectancy)
  ) %>%
  select(
    iso3c,
    country,
    region,
    health_spending,
    gdp_per_capita,
    life_expectancy
  )

country_pairs <- data_2023 %>%
  inner_join(
    data_2023,
    by = character(),
    suffix = c("_1", "_2")
  ) %>%
  filter(iso3c_1 < iso3c_2) %>%
  mutate(
    spending_gap = abs(
      log(health_spending_1 / health_spending_2)
    ),
    income_gap = abs(
      log(gdp_per_capita_1 / gdp_per_capita_2)
    ),
    life_expectancy_gap = abs(
      life_expectancy_1 - life_expectancy_2
    )
  ) %>%
  filter(
    spending_gap <= log(1.25),
    income_gap <= log(1.30),
    life_expectancy_gap >= 5
  ) %>%
  arrange(desc(life_expectancy_gap))

top_pairs <- country_pairs %>%
  select(
    country_1,
    country_2,
    health_spending_1,
    health_spending_2,
    gdp_per_capita_1,
    gdp_per_capita_2,
    life_expectancy_1,
    life_expectancy_2,
    life_expectancy_gap
  ) %>%
  slice_head(n = 10)

print(as_tibble(top_pairs), n = 10, width = Inf)

library(ggplot2)
library(scales)

selected_pairs <- country_pairs %>%
  filter(
    (country_1 == "Jordan" & country_2 == "Eswatini") |
      (country_1 == "Equatorial Guinea" & country_2 == "Sri Lanka") |
      (country_1 == "Ecuador" & country_2 == "South Africa")
  ) %>%
  mutate(
    comparison = case_when(
      country_1 == "Jordan" ~ "Jordan vs Eswatini",
      country_1 == "Equatorial Guinea" ~
        "Equatorial Guinea vs Sri Lanka",
      country_1 == "Ecuador" ~
        "Ecuador vs South Africa"
    )
  )

plot_data <- selected_pairs %>%
  select(
    comparison,
    country_1,
    country_2,
    life_expectancy_1,
    life_expectancy_2,
    life_expectancy_gap
  ) %>%
  pivot_longer(
    cols = c(life_expectancy_1, life_expectancy_2),
    names_to = "country_position",
    values_to = "life_expectancy"
  ) %>%
  mutate(
    country = if_else(
      country_position == "life_expectancy_1",
      country_1,
      country_2
    ),
    comparison = factor(
      comparison,
      levels = c(
        "Ecuador vs South Africa",
        "Equatorial Guinea vs Sri Lanka",
        "Jordan vs Eswatini"
      )
    )
  )
comparison_plot <- ggplot() +
  geom_segment(
    data = selected_pairs,
    aes(
      x = pmin(life_expectancy_1, life_expectancy_2),
      xend = pmax(life_expectancy_1, life_expectancy_2),
      y = comparison,
      yend = comparison
    ),
    linewidth = 1.5,
    color = "#C7CDD4"
  ) +
  geom_point(
    data = plot_data,
    aes(
      x = life_expectancy,
      y = comparison,
      color = country_position
    ),
    size = 4
  ) +
  geom_text(
    data = plot_data,
    aes(
      x = life_expectancy,
      y = comparison,
      label = paste0(
        country,
        "\n",
        round(life_expectancy, 1),
        " years"
      )
    ),
    vjust = -0.8,
    size = 3.5,
    lineheight = 0.95
  ) +
  scale_color_manual(
    values = c(
      "life_expectancy_1" = "#E76F51",
      "life_expectancy_2" = "#2A9D8F"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(55, 85),
    breaks = seq(55, 85, 5),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    title = "Same Budget, Different Lives",
    subtitle = "Similar resources. Different life expectancies.",
    x = "Life expectancy at birth (years)",
    y = NULL,
    caption = "Source: World Bank, World Development Indicators | 2023"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey40"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 15, 20)
  )

comparison_plot

comparison_plot

ggsave(
  here("figures", "same_budget_different_lives.png"),
  plot = comparison_plot,
  width = 10,
  height = 5,
  dpi = 300
)