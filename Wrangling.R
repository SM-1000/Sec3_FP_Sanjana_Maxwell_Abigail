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

# Prepare player data with standardized dates - convert from string format
players <- players %>%
  mutate(birth_date = as.Date(substr(date_of_birth, 1, 10)))  # Extract date part from datetime string

# Filter valuations for current season
valuations_season <- player_valuations_top5 %>%
  filter(between(date, season_start, season_end)) %>%
  left_join(players, by = "player_id", suffix = c("", "_player")) %>%
  mutate(age = as.numeric(difftime(date, birth_date, units = "days")) / 365.25)

# Join appearances data to get minutes played per player for current season - corrected version
player_season_stats <- appearances_top5 %>%
  filter(between(date, season_start, season_end)) %>%
  group_by(player_id) %>%
  summarize(
    total_minutes = sum(minutes_played, na.rm = TRUE),
    matches_played = n(),
    avg_minutes_per_match = total_minutes / matches_played
  )

# Continue with the rest of the analysis
player_analysis <- valuations_season %>%
  # Get the most recent valuation for each player this season
  group_by(player_id) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  # Join with minutes played data
  left_join(player_season_stats, by = "player_id")
  
# Create age bins for easier analysis
player_analysis <- player_analysis %>%
  mutate(
    age_bin = cut(age, breaks = seq(16, 40, by = 2), include.lowest = TRUE),
    age_rounded = floor(age)
  )

# Analyze peak age by minutes played
age_minutes_analysis <- player_analysis %>%
  group_by(age_rounded) %>%
  summarize(
    avg_minutes = mean(total_minutes, na.rm = TRUE),
    median_minutes = median(total_minutes, na.rm = TRUE),
    player_count = n(),
    avg_market_value = mean(market_value_in_eur, na.rm = TRUE) / 1000000
  ) %>%
  filter(player_count >= 10) # Filter ages with enough players for reliable stats
