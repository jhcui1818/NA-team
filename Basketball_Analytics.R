library(dplyr)
event_codes <- read.table("data/NBA Hackathon - Event Codes.txt", header = TRUE, fill = TRUE)
game_lineup <- read.table("data/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", header = TRUE)
play_by_play <- read.table("data/NBA Hackathon - Play by Play Data Sample (50 Games).txt", header = TRUE)

# one game
one_game <- play_by_play %>%
  filter(Game_id == "021fd159b55773fba8157e2090fe0fe2")
# two teams played in one game
team1 <- game_lineup %>%
  filter(Team_id == "012059d397c0b7e5a30a5bb89c0b075e", Game_id == "021fd159b55773fba8157e2090fe0fe2")
team2 <- game_lineup %>%
  filter(Team_id == "cff694c8186a4bd377de400e4f60fe47", Game_id == "021fd159b55773fba8157e2090fe0fe2")
# first period
one_period <- one_game %>%
  filter(Period == 1)
