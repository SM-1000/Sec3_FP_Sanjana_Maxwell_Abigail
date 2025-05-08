# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(scales)

# Set file paths
documents <- "/Users/smeno/OneDrive/Documents"
valuations_path <- file.path(documents, "player_valuations.csv")
appearances_path <- file.path(documents, "appearances.csv")
players_path <- file.path(documents, "players.csv")

# Load datasets
player_valuations <- read_csv(valuations_path)
appearances <- read_csv(appearances_path)
players <- read_csv(players_path)

# Filter to Top 5 European leagues
top5_leagues <- c("GB1", "FR1", "IT1", "ES1", "L1")

if ("competition_id" %in% colnames(appearances)) {
  appearances <- appearances %>%
    filter(competition_id %in% top5_leagues)
}
if ("player_club_domestic_competition_id" %in% colnames(player_valuations)) {
  player_valuations <- player_valuations %>%
    filter(player_club_domestic_competition_id %in% top5_leagues)
}

# Convert date columns
appearances <- appearances %>% mutate(date = as.Date(date))
player_valuations <- player_valuations %>% mutate(date = as.Date(date))

# Filter for 2024–2025 season
season_start <- as.Date("2024-08-01")
season_end <- as.Date("2025-06-30")

valuations_season <- player_valuations %>%
  filter(between(date, season_start, season_end))

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

player_values <- valuations_season %>%
  group_by(player_id) %>%
  summarise(avg_market_value = mean(market_value_in_eur, na.rm = TRUE), .groups = "drop")

player_analysis <- player_stats %>%
  inner_join(player_values, by = "player_id") %>%
  left_join(select(players, player_id, position), by = "player_id")

# Filter out goalkeepers from the player analysis
player_analysis <- player_analysis %>%
  filter(position != "Goalkeeper")  # Exclude goalkeepers

# Plot: Market Value vs. Goals
goal_plot <- ggplot(player_analysis, aes(x = total_goals, y = avg_market_value, color = position)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Market Value vs. Number of Goals",
    subtitle = "Top 5 European Leagues (2024–2025 Season)",
    x = "Total Goals",
    y = "Avg. Market Value (EUR)",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(goal_plot)

print(goal_plot)
