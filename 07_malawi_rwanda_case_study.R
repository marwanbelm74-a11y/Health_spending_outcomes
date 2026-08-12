library(tidyverse)
library(WDI)
library(here)

# Indicators for the Malawi and Rwanda case study
case_indicators <- c(
  life_expectancy = "SP.DYN.LE00.IN",
  under_five_mortality = "SH.DYN.MORT",
  hiv_prevalence = "SH.DYN.AIDS.ZS",
  art_coverage = "SH.HIV.ARTC.ZS"
)

# Download annual data for both countries
case_data <- WDI(
  country = c("MWI", "RWA"),
  indicator = case_indicators,
  start = 2000,
  end = 2023
) %>%
  mutate(
    country = case_when(
      iso2c == "MW" ~ "Malawi",
      iso2c == "RW" ~ "Rwanda",
      TRUE ~ country
    )
  ) %>%
  arrange(country, year)

# Check the number of observations
case_data %>%
  count(country) %>%
  print()

# Check missing values
case_data %>%
  group_by(country) %>%
  summarise(
    missing_life_expectancy = sum(is.na(life_expectancy)),
    missing_child_mortality = sum(is.na(under_five_mortality)),
    missing_hiv_prevalence = sum(is.na(hiv_prevalence)),
    missing_art_coverage = sum(is.na(art_coverage)),
    .groups = "drop"
  ) %>%
  print()

# Save the dataset
write_csv(
  case_data,
  here("data", "malawi_rwanda_case_data.csv")
)


# Compare selected years
case_summary <- case_data %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2019, 2023)) %>%
  select(
    country,
    year,
    life_expectancy,
    under_five_mortality,
    hiv_prevalence,
    art_coverage
  ) %>%
  arrange(country, year)

print(case_summary, n = 12, width = Inf)

# Prepare data for the comparative visualization
case_plot_data <- case_data %>%
  select(
    country,
    year,
    life_expectancy,
    under_five_mortality,
    art_coverage
  ) %>%
  pivot_longer(
    cols = c(
      life_expectancy,
      under_five_mortality,
      art_coverage
    ),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      life_expectancy = "Life expectancy (years)",
      under_five_mortality = "Under-five mortality (per 1,000 births)",
      art_coverage = "Antiretroviral treatment coverage (%)"
    ),
    indicator = factor(
      indicator,
      levels = c(
        "Life expectancy (years)",
        "Under-five mortality (per 1,000 births)",
        "Antiretroviral treatment coverage (%)"
      )
    )
  )


case_plot <- ggplot(
  case_plot_data,
  aes(
    x = year,
    y = value,
    color = country
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 1
  ) +
  scale_color_manual(
    values = c(
      "Malawi" = "#168A72",
      "Rwanda" = "#E76F51"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2023, 5)
  ) +
  labs(
    title = "Malawi and Rwanda: Health Trends",
    subtitle = "Life expectancy, child mortality and HIV treatment coverage, 2000–2023",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: World Bank, World Development Indicators."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    legend.position = "top",
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 20, 15, 15)
  )

case_plot

ggsave(
  here("figures", "malawi_rwanda_health_trends.png"),
  plot = case_plot,
  width = 10,
  height = 9,
  dpi = 300
)