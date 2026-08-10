library(tidyverse)
library(ggplot2)
library(scales)
library(here)

health_data <- readRDS(
  here("data", "health_data_clean.rds")
)

# Prepare the 2023 sample
financing_data <- health_data %>%
  filter(
    year == 2023,
    !is.na(life_expectancy),
    !is.na(health_spending),
    health_spending > 0,
    !is.na(gdp_per_capita),
    gdp_per_capita > 0,
    !is.na(public_health_share),
    !is.na(out_of_pocket_share)
  )

# Descriptive statistics
financing_summary <- financing_data %>%
  summarise(
    countries = n_distinct(iso3c),
    average_public_share = mean(public_health_share),
    average_out_of_pocket_share = mean(out_of_pocket_share),
    median_public_share = median(public_health_share),
    median_out_of_pocket_share = median(out_of_pocket_share)
  )

print(financing_summary)

# Correlations
correlation_results <- financing_data %>%
  summarise(
    public_share_correlation = cor(
      public_health_share,
      life_expectancy
    ),
    out_of_pocket_correlation = cor(
      out_of_pocket_share,
      life_expectancy
    )
  )

print(correlation_results)

# Baseline model
model_baseline <- lm(
  life_expectancy ~
    log(health_spending) +
    log(gdp_per_capita),
  data = financing_data
)

# Healthcare financing model
model_financing <- lm(
  life_expectancy ~
    log(health_spending) +
    log(gdp_per_capita) +
    public_health_share +
    out_of_pocket_share,
  data = financing_data
)

summary(model_baseline)
summary(model_financing)

# Compare model fit
model_comparison <- tibble(
  model = c("Baseline", "Healthcare financing"),
  r_squared = c(
    summary(model_baseline)$r.squared,
    summary(model_financing)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_baseline)$adj.r.squared,
    summary(model_financing)$adj.r.squared
  )
)

print(model_comparison)

# Test the joint contribution of financing variables
model_test <- anova(
  model_baseline,
  model_financing
)

print(model_test)


# Residualize life expectancy and public financing
outcome_model <- lm(
  life_expectancy ~
    log(health_spending) +
    log(gdp_per_capita) +
    out_of_pocket_share,
  data = financing_data
)

public_share_model <- lm(
  public_health_share ~
    log(health_spending) +
    log(gdp_per_capita) +
    out_of_pocket_share,
  data = financing_data
)

financing_plot_data <- financing_data %>%
  mutate(
    life_expectancy_residual = resid(outcome_model),
    public_share_residual = resid(public_share_model)
  )

# Compare raw and partial correlations
raw_correlation <- cor(
  financing_plot_data$public_health_share,
  financing_plot_data$life_expectancy
)

partial_correlation <- cor(
  financing_plot_data$public_share_residual,
  financing_plot_data$life_expectancy_residual
)

print(
  tibble(
    relationship = c("Raw", "Adjusted"),
    correlation = c(raw_correlation, partial_correlation)
  )
)



raw_plot <- ggplot(
  financing_plot_data,
  aes(
    x = public_health_share,
    y = life_expectancy
  )
) +
  geom_point(
    color = "#2A9D8F",
    alpha = 0.65,
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#202020",
    linewidth = 0.9
  ) +
  labs(
    title = "Before Controls",
    subtitle = paste0(
      "Correlation: ",
      round(raw_correlation, 2)
    ),
    x = "Public financing share (%)",
    y = "Life expectancy (years)"
  ) +
  theme_minimal(base_size = 12)

adjusted_plot <- ggplot(
  financing_plot_data,
  aes(
    x = public_share_residual,
    y = life_expectancy_residual
  )
) +
  geom_point(
    color = "#E76F51",
    alpha = 0.65,
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#202020",
    linewidth = 0.9
  ) +
  labs(
    title = "After Controls",
    subtitle = paste0(
      "Partial correlation: ",
      round(partial_correlation, 2)
    ),
    x = "Residual public financing share (percentage points)",
    y = "Residual life expectancy (years)"
  ) +
  theme_minimal(base_size = 12)



if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(patchwork)

financing_plot <- raw_plot + adjusted_plot +
  plot_annotation(
    title = "Beyond Correlation",
    subtitle = paste(
      "Healthcare financing and life expectancy",
      "across countries, 2023"
    ),
    caption = paste(
      "Source: World Bank, World Development Indicators.",
      "Adjusted for GDP per capita, total health spending",
      "and out-of-pocket financing share."
    )
  )

financing_plot

dir.create(
  here("figures"),
  showWarnings = FALSE
)

ggsave(
  here("figures", "healthcare_financing.png"),
  plot = financing_plot,
  width = 12,
  height = 6,
  dpi = 300
)