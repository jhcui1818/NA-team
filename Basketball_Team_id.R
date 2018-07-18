library(dplyr)
event_codes <- read.table("data/NBA Hackathon - Event Codes.txt", header = TRUE, fill = TRUE)
game_lineup <- read.table("data/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", header = TRUE)
play_by_play <- read.table("data/NBA Hackathon - Play by Play Data Sample (50 Games).txt", header = TRUE)

# Select needed variables, keep only scored, free throw and substitution events
scored <- play_by_play%>%
  select(Game_id,Event_Num,Period,Event_Msg_Type,Action_Type,PC_Time,Option1,Team_id,Person1,Person2)%>%
  filter(Event_Msg_Type==1|Event_Msg_Type==3|Event_Msg_Type==8)
df_scored <- scored%>%
  group_by(Game_id,Period)%>%
  arrange(Game_id,Period,desc(PC_Time))
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
sub_team <- df_team %>%
  filter(Event_Msg_Type == 8) %>%
  ungroup() %>%
  select(Game_id, Person2, Person1_Team_id)
sub_team <- unique(sub_team)
colnames(sub_team)[3] <- "sub_Team_id"
df_sub_merged <- temp %>%
  filter(is.na(Person1_Team_id)) %>%
  left_join(sub_team, by = c("Game_id", "Person1" = "Person2"))
df_sub_merged$sub_Team_id <- as.character(df_sub_merged$sub_Team_id)
for (i in 1:nrow(df_sub_merged)){
  df_sub_merged$Person1_Team_id[i] <- ifelse(df_sub_merged$sub_Team_id[i] == df_sub_merged$team1[i] | df_sub_merged$sub_Team_id[i] == df_sub_merged$team2[i], df_sub_merged$sub_Team_id[i], NA)
}

#
LastNA <- df_sub_merged %>%
  filter(is.na(Person1_Team_id))
sub_team2 <- df_sub_merged %>%
  ungroup() %>%
  filter(!is.na(Person1_Team_id)) %>%
  select(Game_id, Person1, Person1_Team_id)
sub_team2 <- unique(sub_team2)
sub_merged2 <- df_sub_merged %>%
  filter(is.na(Person1_Team_id)) %>%
  left_join(sub_team2, by = c("Game_id", "Person1"))
for (i in 1:nrow(sub_merged2)){
  sub_merged2$Person1_Team_id.x[i] <- ifelse(sub_merged2$Person1_Team_id.y[i] == sub_merged2$team1[i] | sub_merged2$Person1_Team_id.y[i] == sub_merged2$team2[i], sub_merged2$Person1_Team_id.y[i], NA)
}
