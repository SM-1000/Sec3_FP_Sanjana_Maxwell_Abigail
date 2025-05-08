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

if ("competition_id" %in% colnames(appearances)) {
  appearances <- appearances %>%
    filter(competition_id %in% top5_leagues)
}

# Convert date column to Date format
if ("date" %in% colnames(appearances)) {
  appearances <- appearances %>%
    mutate(date = as.Date(date))
}

# Define season boundaries
season_start <- as.Date("2024-08-01")
season_end <- as.Date("2025-06-30")

# Summarize goal contributions per player
player_stats <- appearances %>%
  filter(date >= season_start & date <= season_end) %>%
  group_by(player_id) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_goal_contributions = total_goals + total_assists
  )

# Merge with player positions and remove goalkeepers
player_analysis <- player_stats %>%
  left_join(
    select(players, player_id, position),
    by = "player_id"
  ) %>%
  filter(position != "Goalkeeper")  # Exclude goalkeepers

# Generate box plot
box_plot <- ggplot(player_analysis, aes(x = position, y = total_goal_contributions, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    title = "Distribution of Goal Contributions by Position",
    subtitle = "Top 5 European Leagues (2024–2025 Season)",
    x = "Position",
    y = "Total Goal Contributions"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(box_plot)

