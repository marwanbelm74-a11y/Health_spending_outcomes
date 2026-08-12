library(tidyverse)
library(ggplot2)
library(scales)
library(here)

health_data <- readRDS(
  here("data", "health_data_clean.rds")
)

# Check data availability over time
year_coverage <- health_data %>%
  group_by(year) %>%
  summarise(
    countries = n_distinct(iso3c),
    average_life_expectancy = mean(life_expectancy),
    median_life_expectancy = median(life_expectancy),
    median_health_spending = median(health_spending),
    .groups = "drop"
  ) %>%
  arrange(year)

print(year_coverage)

# Compare countries with observations in both years
progress_data <- health_data %>%
  filter(year %in% c(2000, 2023)) %>%
  select(
    iso3c,
    country,
    region,
    year,
    life_expectancy,
    health_spending
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(
      life_expectancy,
      health_spending
    )
  ) %>%
  filter(
    !is.na(life_expectancy_2000),
    !is.na(life_expectancy_2023),
    !is.na(health_spending_2000),
    !is.na(health_spending_2023)
  ) %>%
  mutate(
    life_expectancy_gain =
      life_expectancy_2023 - life_expectancy_2000,
    
    spending_ratio =
      health_spending_2023 / health_spending_2000
  ) %>%
  arrange(desc(life_expectancy_gain))

# Number of countries available for comparison
nrow(progress_data)


# Countries with the largest life expectancy gains
top_progress <- progress_data %>%
  select(
    country,
    region,
    life_expectancy_2000,
    life_expectancy_2023,
    life_expectancy_gain
  ) %>%
  slice_max(
    life_expectancy_gain,
    n = 15
  )

print(top_progress, n = 15)



# Prepare the 15 countries with the largest gains
plot_progress <- progress_data %>%
  slice_max(
    life_expectancy_gain,
    n = 15
  ) %>%
  mutate(
    country = forcats::fct_reorder(
      country,
      life_expectancy_gain
    )
  )

# Visualize changes in life expectancy
progress_plot <- ggplot(plot_progress) +
  
  geom_segment(
    aes(
      x = life_expectancy_2000,
      xend = life_expectancy_2023,
      y = country,
      yend = country
    ),
    color = "#D1D5DB",
    linewidth = 1.4
  ) +
  
  geom_point(
    aes(
      x = life_expectancy_2000,
      y = country
    ),
    color = "#9CA3AF",
    size = 3.5
  ) +
  
  geom_point(
    aes(
      x = life_expectancy_2023,
      y = country
    ),
    color = "#168A72",
    size = 3.5
  ) +
  
  geom_text(
    aes(
      x = life_expectancy_2023,
      y = country,
      label = paste0(
        "+",
        round(life_expectancy_gain, 1),
        " years"
      )
    ),
    hjust = -0.15,
    size = 3.5,
    color = "#168A72",
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    limits = c(40, 85),
    breaks = seq(40, 80, 10)
  ) +
  
  labs(
    title = "Life expectancy gains",
    subtitle = "Countries with the largest gains in life expectancy, 2000–2023",
    x = "Life expectancy at birth (years)",
    y = NULL,
    caption = paste(
      "Source: World Bank, World Development Indicators.",
      "Top 15 of 183 countries with observations in both years."
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#555555"
    ),
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 35, 15, 15)
  )

progress_plot


# Save the figure
dir.create(
  here("figures"),
  showWarnings = FALSE
)

ggsave(
  here("figures", "life_expectancy_gains.png"),
  plot = progress_plot,
  width = 11,
  height = 7,
  dpi = 300
)