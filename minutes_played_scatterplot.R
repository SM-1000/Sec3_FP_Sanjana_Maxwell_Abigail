# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)

# Set file paths
documents <- "/Users/smeno/OneDrive/Documents"
valuations_path <- file.path(documents, "player_valuations.csv")
appearances_path <- file.path(documents, "appearances.csv")
players_path <- file.path(documents, "players.csv")

# Load datasets
player_valuations <- read_csv(valuations_path)
appearances <- read_csv(appearances_path)
players <- read_csv(players_path)

# Filter for Top 5 European leagues
top5_leagues <- c("GB1", "FR1", "IT1", "ES1", "L1")

if ("competition_id" %in% names(appearances)) {
  appearances <- appearances %>%
    filter(competition_id %in% top5_leagues)
}

if ("player_club_domestic_competition_id" %in% names(player_valuations)) {
  player_valuations <- player_valuations %>%
    filter(player_club_domestic_competition_id %in% top5_leagues)
}

# Convert date columns
appearances <- appearances %>%
  mutate(date = as.Date(date))

player_valuations <- player_valuations %>%
  mutate(date = as.Date(date))

# Filter for 2024–2025 season
season_start <- as.Date("2024-08-01")
season_end <- as.Date("2025-06-30")

valuations_season <- player_valuations %>%
  filter(date >= season_start & date <= season_end)

# Aggregate player appearances
player_stats <- appearances %>%
  filter(date >= season_start & date <= season_end) %>%
  group_by(player_id) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_appearances = n(),
    total_minutes = sum(minutes_played, na.rm = TRUE),
    avg_minutes_per_appearance = total_minutes / total_appearances,
    .groups = "drop"
  )

# Average market value
player_values <- valuations_season %>%
  group_by(player_id) %>%
  summarise(avg_market_value = mean(market_value_in_eur, na.rm = TRUE), .groups = "drop")

# Merge stats and values
player_analysis <- player_stats %>%
  inner_join(player_values, by = "player_id") %>%
  left_join(
    players %>% select(player_id, position),
    by = "player_id"
  )

# Plot: Minutes vs. Market Value
appearance_plot <- ggplot(
  player_analysis,
  aes(x = total_minutes, y = avg_market_value, color = position)
) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Market Value vs. Minutes Played",
    subtitle = "Top 5 European Leagues (2024–2025 Season)",
    x = "Total Minutes Played",
    y = "Avg. Market Value (EUR)",
    color = "Position"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 30, 10)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(appearance_plot)



