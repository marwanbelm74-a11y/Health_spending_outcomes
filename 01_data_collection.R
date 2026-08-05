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