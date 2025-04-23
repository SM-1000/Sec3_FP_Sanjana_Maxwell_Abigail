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
