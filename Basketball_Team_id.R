library(dplyr)
event_codes <- read.table("data/NBA Hackathon - Event Codes.txt", header = TRUE, fill = TRUE)
game_lineup <- read.table("data/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", header = TRUE)
play_by_play <- read.table("data/NBA Hackathon - Play by Play Data Sample (50 Games).txt", header = TRUE)
game_lineup[,c(1,3,4,5)] <- apply(game_lineup[,c(1,3,4,5)],2,as.character)
play_by_play[,c(1,11,12,13)] <- apply(play_by_play[,c(1,11,12,13)],2,as.character)
# Select needed variables, keep only scored, free throw and substitution events
scored <- play_by_play%>%
  select(Game_id,Event_Num,Period,Event_Msg_Type,Action_Type,PC_Time,Option1,Team_id,Person1,Person2)%>%
  filter(Event_Msg_Type==1|Event_Msg_Type==3|Event_Msg_Type==8|Event_Msg_Type==12)
# Starting players' team id
# 224 NAs left
unique_game <- unique(game_lineup[,c(1,3,4)])
df_person1 <- scored %>%
  left_join(unique_game, by = c("Game_id", "Person1"="Person_id"))
df_person2 <- df_person1 %>%
  left_join(unique_game, by = c("Game_id", "Person2"="Person_id"))
colnames(df_person2)[8] <- "Original_Team_id"
colnames(df_person2)[11] <- "Person1_Team_id"
colnames(df_person2)[12] <- "Person2_Team_id"

# Substitution players' team id
# 177 NAs left
player_team <- game_lineup %>%
  group_by(Game_id, Team_id) %>%
  summarise(n=n())
player_team$Team <- rep(c("team1", "team2"),50)
library(tidyr)
player_team <- player_team %>% spread(Team, Team_id)
df_team <- left_join(df_person2, player_team[,c(1,3,4)], by = "Game_id")
df_team[which(is.na(df_team$Person1_Team_id) & df_team$Event_Msg_Type == 8),"Person1_Team_id"] <- 
  df_team[which(is.na(df_team$Person1_Team_id) & df_team$Event_Msg_Type == 8),"Person2_Team_id"]  

#extract assigned substitute player and corresponding team id
sub_team <- df_team %>%
  filter(Event_Msg_Type == 8) %>%
  select(Game_id, Person2, Person1_Team_id)
sub_team <- unique(sub_team)
#assign team id to substitute player in other events
colnames(sub_team)[3] <- "sub_Team_id"
sub_team <- na.omit(sub_team)
df_team2 <- df_team %>%
  filter(is.na(Person1_Team_id)) %>%
  left_join(sub_team, by = c("Game_id", "Person1" = "Person2"))
for (i in 1:nrow(df_team2)){
  df_team2$Person1_Team_id[i] <- ifelse(df_team2$sub_Team_id[i] == df_team2$team1[i] | df_team2$sub_Team_id[i] == df_team2$team2[i], df_team2$sub_Team_id[i], NA)
}
#test how many NA is left
a1 <- df_team2 %>% filter( is.na(Person1_Team_id))
#204 starting events

#merge data set
df_team <- filter(df_team, !is.na(df_team$Person1_Team_id))
df_team <- rbind(df_team, df_team2[,1:14])
#order dataset
df_scored <- df_team%>%
  group_by(Game_id,Period)%>%
  arrange(Game_id,Period,desc(PC_Time))
#calculate at the moment of each event happened, the points gained by each team
df_scored$Team_score <- NA
df_scored$Event_score <- NA
df_scored$Event_score <- ifelse(df_scored$Event_Msg_Type == 1, df_scored$Option1, ifelse(df_scored$Event_Msg_Type == 3 & df_scored$Option1 == 1, df_scored$Option1, 0))
#test
a <- df_scored %>% filter(Event_Msg_Type==8)
sum(a$Event_score)
a <- df_scored %>% filter(Event_Msg_Type == 3 & Option1 == 2)
sum(a$Event_score)

#
df_scored$Team_score[which(df_scored$Period == 1 & df_scored$Event_Msg_Type == 12)] <- 0 
for(i in 1:nrow(df_scored)){
  
}

#######################
sum_scored <- df_scored %>%
  group_by(Game_id, Person1_Team_id) %>%
  summarise(n=n())
