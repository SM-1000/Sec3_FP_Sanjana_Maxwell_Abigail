# Analyzing Player Metrics and Market Value in European Football

This project explores how player characteristics such as age, position, minutes played, and goal contributions impact individual market value and team success in the top 5 European football leagues during the 2024–2025 season.

## Overview

Using publicly available football data from Transfermarkt (via Kaggle), we examined the relationship between player attributes and market valuation, and how team-level metrics correlate with win percentages. Our analysis uses visual tools to uncover performance trends across positions and age groups, offering insights for talent evaluation and club investment strategy.

### Interesting Insight

One key finding is that players typically reach their highest market value just before their peak playing time. Attackers, for instance, peak in value around ages 24–26, while goalkeepers peak closer to 30. This anticipatory investment behavior suggests clubs value potential performance and longevity.  

## Repo Structure

- `.gitignore`: Standard Git ignore file.
- `AgePositionValueAnalysis.R`: Script analyzing player age, position, and market value.
- `PeakAgeAnalysis.R`: Script focused on identifying peak performance ages.
- `Wrangling.R`: Script used to clean and transform datasets.
- `README.md`: Project overview and key findings.
- `Project_Guidelines.md`: Project expectations and requirements.
- `Stat 184 Final Project(Initial_Draft...).pdf`: Draft report of project findings.
- `MLA9.csl`, `apa7.csl`: Citation style files for report formatting.
  
## Data Sources and Acknowledgements

The data was sourced from the [Transfermarkt Football Data Kaggle repository](https://www.kaggle.com/datasets/davidcariboo/player-scores), created by David Cariboo using web scraping tools and SQL. All datasets are licensed under the CC0 (Public Domain) license and updated regularly. We used four datasets:
- **Player Valuation** *(primary)* – Market values over time.
- **Player Appearances** – Match-level performance data.
- **Player Statistics & Demographics** – Positional and age data.
- **Game Statistics** – Match outcomes and team performance indicators.

## Authors

Abigail Chen

Maxwell Gerhart (mbg5979@psu.edu)

Sanjana Menon
