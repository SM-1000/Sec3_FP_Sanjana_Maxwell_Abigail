# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(scales)
library(knitr)

# Set file paths for CSV files
documents <- "/Users/smeno/OneDrive/Documents"
valuations_path <- file.path(documents, "player_valuations.csv")
appearances_path <- file.path(documents, "appearances.csv")
players_path <- file.path(documents, "players.csv")

# Load datasets
player_valuations <- read_csv(valuations_path)
appearances <- read_csv(appearances_path)
players <- read_csv(players_path)

# Optional: Filter for Top 5 European leagues
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
if ("date" %in% colnames(appearances)) {
  appearances <- appearances %>% mutate(date = as.Date(date))
}
if ("date" %in% colnames(player_valuations)) {
  player_valuations <- player_valuations %>% mutate(date = as.Date(date))
}

# Filter valuation data for the 2024–2025 season
season_start <- as.Date("2024-08-01")
season_end <- as.Date("2025-06-30")

valuations_season <- player_valuations %>%
  filter(between(date, season_start, season_end))

# Aggregate appearances data by player
player_stats <- appearances %>%
  filter(date >= season_start & date <= season_end) %>%
  group_by(player_id) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_appearances = n(),
    total_minutes = sum(minutes_played, na.rm = TRUE),
    avg_minutes_per_appearance = total_minutes / total_appearances
  )

# Get average market value per player
player_values <- valuations_season %>%
  group_by(player_id) %>%
  summarise(avg_market_value = mean(market_value_in_eur, na.rm = TRUE))

# Load additional library
library(knitr)

# Combine player data
player_analysis <- player_stats %>%
  inner_join(player_values, by = "player_id") %>%
  left_join(select(players, player_id, name, position), by = "player_id") %>%
  mutate(goal_contributions = total_goals)

# Select top 10 by market value
top10_players <- player_analysis %>%
  arrange(desc(avg_market_value)) %>%
  slice_head(n = 10) %>%
  mutate(
    avg_market_value_millions = round(avg_market_value / 1e6, 2)
  ) %>%
  select(
    name,
    position,
    total_goals,
    total_appearances,
    total_minutes,
    avg_minutes_per_appearance,
    avg_market_value_millions
  )

# Rename for table output
colnames(top10_players) <- c(
  "Player Name",
  "Position",
  "Goals",
  "Appearances",
  "Minutes Played",
  "Avg. Minutes per Appearance",
  "Avg. Market Value (EUR, Millions)"
)

# Display as markdown table
kable(top10_players, format = "markdown", align = "c", caption = "Top 10 Players by Average Market Value (2024–2025 Season)")
