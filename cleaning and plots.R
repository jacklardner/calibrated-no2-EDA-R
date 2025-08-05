library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(openair)
library(ggplot2)

no2_bysite_df <- read_csv("data/no2_calibrated.csv")
ref_df <- read_csv("data/ep_no2_2023.csv")

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
  geom_histogram(binwidth = 1, fill = "steelblue", color = "cyan") +
  facet_wrap(~site, scales = "free_y") +
  labs(title = "NO2 Concentration Distributions by Site", x = "NO2 (ppb)", y = "Count") +
  theme_minimal()



site_data <- no2_long %>% filter(site == "250_no2")
timeVariation(site_data, pollutant = "no2", main = "Time Variation for Site 250")


ref_df_clean <- ref_df %>%
  mutate(
    datetime = as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
    no2 = sample_measurement
  ) %>%
  select(date = datetime, no2) %>%
  filter(!is.na(no2))

timeVariation(ref_df_clean, pollutant = "no2", main = "Time Variation - DEM")




sensor_df <- read_csv("data/NO2_DPW_withFlatline.csv")

sensor_df_clean <- sensor_df %>%
  filter(flatline_flag == FALSE) %>%
  mutate(
    timestamp_parsed = as.POSIXct(timestamp_local, tz = "UTC"),
    
    date = with_tz(timestamp_parsed, tzone = "America/New_York"),
    
    hour = floor_date(date, unit = "hour")
  )

#average on the hour
sensor_hourly <- sensor_df_clean %>%
  group_by(hour) %>%
  summarise(no2 = mean(no2, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = hour)

timeVariation(sensor_hourly, pollutant = "no2", main = "Hourly-Averaged NO2 - MOD-00811")

ggplot(sensor_hourly, aes(x = no2)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", boundary = 0) +
  labs(
    title = "Histogram of Hourly-Averaged NO2 - MOD-00811",
    x = "NO2 (ppb)",
    y = "Count"
  ) +
  theme_minimal()
