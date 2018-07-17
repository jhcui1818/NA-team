# Starting players' team id
# 224 NAs left
unique_game <- unique(game_lineup[,c(1,3,4)])
df_person1 <- df_scored %>%
  left_join(unique_game, by = c("Game_id", "Person1"="Person_id"))
df_person2 <- df_person1 %>%
  left_join(unique_game, by = c("Game_id", "Person2"="Person_id"))
colnames(df_person2)[8] <- "Original_Team_id"
colnames(df_person2)[11] <- "Person1_Team_id"
colnames(df_person2)[12] <- "Person2_Team_id"

# Substitution players' team id
# 177 NAs left
game_team <- game_lineup %>%
  group_by(Game_id, Team_id) %>%
  summarise(n=n())
game_team$Team <- rep(c("team1", "team2"),50)
library(tidyr)
game_team <- game_team %>% spread(Team, Team_id)
df_team <- left_join(df_person2, game_team[,c(1,3,4)], by = "Game_id")
df_team[which(is.na(df_team$Person1_Team_id) & df_team$Event_Msg_Type == 8),"Person1_Team_id"] <- 
  df_team[which(is.na(df_team$Person1_Team_id) & df_team$Event_Msg_Type == 8),"Person2_Team_id"]  

# Player who is not starting in this game but is starting in others and do not change teams in the middle
# need to add the temp data frame to the df_team
# 69 Nas left
a <- unique(game_lineup[,3:4])
temp <- df_team %>%
  filter(is.na(Person1_Team_id)) %>%
  left_join(a, by = c("Person1"="Person_id"))
temp[,8:15] <- apply(temp[,8:15],2,as.character)
for (i in 1:nrow(temp)){
  temp$Person1_Team_id[i] <- ifelse(temp$Team_id[i] == temp$team1[i] | temp$Team_id[i] == temp$team2[i], temp$Team_id[i], NA)
}

# according to substitue player's team id to assign their team id in other events they involved as person1
sub_team <- df_merged_play %>%
  filter(Event_Msg_Type == 8) %>%
  select(Game_id, Person1, Person1_Team_id)
sub_team <- unique(sub_team)
colnames(sub_team)[3] <- "sub_Team_id"
df_sub_merged <- df_merged_play %>%
  left_join(sub_team, by = c("Person1", "Game_id"))