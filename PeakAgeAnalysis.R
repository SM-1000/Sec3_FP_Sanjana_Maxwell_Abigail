# Load required libraries
library(ggplot2)
library(dplyr)

# Define top 5 European football leagues
top5_leagues <- c("GB1", "FR1", "IT1", "ES1", "L1")  # Premier League, Ligue 1, Serie A, La Liga, Bundesliga

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

# Prepare player data with standardized birth date
players <- players %>%
  mutate(birth_date = as.Date(substr(date_of_birth, 1, 10)))  # Extract date part

# Filter valuations for current season and calculate player age
valuations_season <- player_valuations_top5 %>%
  filter(between(date, season_start, season_end)) %>%
  left_join(players %>% select(player_id, birth_date), by = "player_id") %>%
  mutate(age = as.numeric(difftime(date, birth_date, units = "days")) / 365.25)

# Calculate minutes played per player for current season
player_season_stats <- appearances_top5 %>%
  filter(between(date, season_start, season_end)) %>%
  group_by(player_id) %>%
  summarize(
    total_minutes = sum(minutes_played, na.rm = TRUE),
    matches_played = n(),
    avg_minutes_per_match = total_minutes / matches_played,
    .groups = "drop"
  )

# Merge most recent valuation, minutes played, and player position
player_analysis <- valuations_season %>%
  group_by(player_id) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(player_season_stats, by = "player_id") %>%
  left_join(players %>% select(player_id, position), by = "player_id")

# Create age bins and rounded age for analysis
player_analysis <- player_analysis %>%
  mutate(
    age_bin = cut(age, breaks = seq(16, 40, by = 2), include.lowest = TRUE),
    age_rounded = floor(age)
  )

# Create the scatter plot with facets for each position
position_scatter <- ggplot(player_analysis, aes(x = age, y = total_minutes)) +
  geom_point(aes(color = position), alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, aes(color = position), alpha = 0.2) +
  facet_wrap(~ position, ncol = 2) +
  scale_color_manual(values = c(
    "Attack" = "#FF5733", 
    "Midfield" = "#33A1FD", 
    "Defender" = "#9370DB", 
    "Goalkeeper" = "#2ECC71"
  )) +
  scale_x_continuous(breaks = seq(16, 40, by = 4), limits = c(16, 40)) +
  labs(
    title = "Age vs Minutes Played by Position",
    subtitle = "Top 5 European Leagues (2024–2025 Season)",
    x = "Age (years)",
    y = "Total Minutes Played",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
