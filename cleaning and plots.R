library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(openair)
library(ggplot2)

no2_bysite_df <- read_csv("data/no2_calibrated.csv")
no2_long <- no2_bysite_df %>%
  pivot_longer(
    cols = -local_timestamp,
    names_to = "site",
    values_to = "no2"
  )

no2_long <- no2_long %>%
  mutate(
    date = as.POSIXct(local_timestamp, tz = "America/New_York"),
    site = as.factor(site)
  ) %>%
  select(date, site, no2)

timeVariation(no2_long, pollutant = "no2", group = "site")


ggplot(no2_long, aes(x = no2)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  facet_wrap(~site, scales = "free_y") +
  labs(title = "NO2 Concentration Distributions by Site", x = "NO₂ (ppb)", y = "Count") +
  theme_minimal()

