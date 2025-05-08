# Load required libraries
library(ggplot2)
library(dplyr)

# Define top 5 European football leagues
top5_leagues <- c("GB1", "FR1", "IT1", "ES1", "L1")

# Define season date range
season_start <- as.Date("2024-08-01")
season_end <- as.Date("2025-06-30")

# Clean and filter appearances data
appearances_top5 <- appearances %>%
  filter(competition_id %in% top5_leagues) %>%
  mutate(date = as.Date(date))

# Clean and filter player valuations data
player_valuations_top5 <- player_valuations %>%
  filter(player_club_domestic_competition_id %in% top5_leagues) %>%
  mutate(date = as.Date(date))

# Prepare player data with standardized birth_date
players_clean <- players %>%
  mutate(birth_date = as.Date(substr(date_of_birth, 1, 10))) %>%
  select(player_id, birth_date, position)

# Filter valuations for current season and join birth date
valuations_season <- player_valuations_top5 %>%
  filter(between(date, season_start, season_end)) %>%
  left_join(players_clean %>% select(player_id, birth_date), by = "player_id") %>%
  mutate(age = as.numeric(difftime(date, birth_date, units = "days")) / 365.25)

# Get most recent valuation per player and add position
player_analysis <- valuations_season %>%
  group_by(player_id) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(players_clean %>% select(player_id, position), by = "player_id")

# Create age bins
player_analysis <- player_analysis %>%
  mutate(
    age_bin = factor(case_when(
      age < 19 ~ "16-18",
      age < 21 ~ "19-20",
      age < 23 ~ "21-22",
      age < 25 ~ "23-24",
      age < 27 ~ "25-26",
      age < 29 ~ "27-28",
      age < 31 ~ "29-30",
      age < 33 ~ "31-32",
      age < 35 ~ "33-34",
      age < 37 ~ "35-36",
      age < 39 ~ "37-38",
      age <= 40 ~ "39-40"
    ), levels = c("16-18", "19-20", "21-22", "23-24", "25-26", "27-28", 
                  "29-30", "31-32", "33-34", "35-36", "37-38", "39-40")),
    position = ifelse(is.na(position), "Unknown", position)
  )

# Summarize for heatmap
heatmap_data <- player_analysis %>%
  filter(!is.na(market_value_in_eur) & !is.na(position) & !is.na(age_bin)) %>%
  group_by(age_bin, position) %>%
  summarize(
    avg_market_value = mean(market_value_in_eur, na.rm = TRUE),
    median_market_value = median(market_value_in_eur, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    avg_market_value_millions = avg_market_value / 1e6,
    median_market_value_millions = median_market_value / 1e6
  )

# Ensure all age-position combinations are included
all_combinations <- expand.grid(
  age_bin = levels(player_analysis$age_bin),
  position = unique(player_analysis$position)
)

# Join with full combinations and fill missing with NA
heatmap_data <- all_combinations %>%
  left_join(heatmap_data, by = c("age_bin", "position")) %>%
  mutate(
    avg_market_value_millions = ifelse(is.na(avg_market_value_millions), NA, avg_market_value_millions),
    label = ifelse(is.na(avg_market_value_millions), "N/A", sprintf("%.0f€", avg_market_value_millions))
  )

# Order positions
heatmap_data$position <- factor(
  heatmap_data$position, 
  levels = c("Goalkeeper", "Defender", "Midfield", "Attack", "Unknown")
)

# Plot the heatmap
ggplot(heatmap_data, aes(x = age_bin, y = position, fill = avg_market_value_millions)) +
  geom_tile() +
  geom_text(aes(label = label), color = "white", size = 5, fontface = "bold") +
  scale_fill_gradient(
    low = "blue", high = "orange", 
    na.value = "grey50",
    name = "Avg. Market Value (M€)",
    breaks = seq(0, ceiling(max(heatmap_data$avg_market_value_millions, na.rm = TRUE)), by = 10)
  ) +
  labs(
    title = "Age vs Position Heatmap",
    subtitle = "(Top 5 European Leagues, 2024–2025 Season)",
    x = "Age Group",
    y = "Position",
    caption = "Data includes most recent valuation per player"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )