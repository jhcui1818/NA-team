library(dplyr)
event_codes <- read.table("data/NBA Hackathon - Event Codes.txt", header = TRUE, fill = TRUE)
game_lineup <- read.table("data/NBA Hackathon - Game Lineup Data Sample (50 Games).txt", header = TRUE)
play_by_play <- read.table("data/NBA Hackathon - Play by Play Data Sample (50 Games).txt", header = TRUE)


# two teams played in one game
team1 <- game_lineup %>%
  filter(Team_id == "012059d397c0b7e5a30a5bb89c0b075e", Game_id == "021fd159b55773fba8157e2090fe0fe2")
team2 <- game_lineup %>%
  filter(Team_id == "cff694c8186a4bd377de400e4f60fe47", Game_id == "021fd159b55773fba8157e2090fe0fe2")
# merge game_lineup and play_by_play datasets
play_by_play$Person1 <- as.character(play_by_play$Person1)
game_lineup$Person_id <- as.character(game_lineup$Person_id)
df_merged <- left_join(play_by_play, game_lineup, by = c("Person1" = "Person_id"))
# one game
one_game <- df_merged %>%
  filter(Game_id.x == "021fd159b55773fba8157e2090fe0fe2")
# first period
one_period <- one_game %>%
  filter(Period == 1)

# Select needed variables, keep only scored, free throw and substitution events
scored <- play_by_play%>%
  select(Game_id,Event_Num,Period,Event_Msg_Type,Action_Type,PC_Time,Option1,Team_id,Person1,Person2)%>%
  filter(Event_Msg_Type==1|Event_Msg_Type==3|Event_Msg_Type==8)
df_scored <- scored%>%
  group_by(Game_id,Period)%>%
  arrange(Game_id,Period,desc(PC_Time))

temp <- play_by_play %>%
  filter(Event_Msg_Type == 3 & Action_Type == 17)
order_play <- play_by_play %>%
  group_by(Game_id,Period)%>%
  arrange(Game_id,Period,desc(PC_Time))
df_with_team <- left_join(df_scored, game_lineup, by = c("Person1" = "Person_id"))
df_with_team2 <- df_with_team %>%
  group_by(Game_id.x) %>%
  unique(by ="Event_Num")
# merge datasets
gm_id <- unique(game_lineup$Game_id)
merged_play <- list()
for (i in 1:50) {
  game_player <- unique(filter(game_lineup, game_lineup$Game_id == gm_id[i])[,3:4])
  game_event <- df_scored %>% 
    filter(Game_id == gm_id[i])
  merged_play[[i]] <- left_join(game_event, game_player, by = c("Person1"="Person_id"))
}
for (i in 1:50) {
  game_player <- unique(filter(game_lineup, game_lineup$Game_id == gm_id[i])[,3:4])
  game_event <- df_scored %>% 
    filter(Game_id == gm_id[i])
  merged_play[[i]] <- left_join(merged_play[[i]], game_player, by = c("Person2"="Person_id"))
}
library(data.table)
df_merged_play <- rbindlist(merged_play)
colnames(df_merged_play)[8] <- "Original_Team_id"
colnames(df_merged_play)[11] <- "Person1_Team_id"
colnames(df_merged_play)[12] <- "Person2_Team_id"
game_team <- game_lineup %>%
  group_by(Game_id, Team_id) %>%
  summarise(n=n())
game_team$Team <- rep(c("team1", "team2"),50)
library(tidyr)
game_team <- game_team %>% spread(Team, Team_id)
df_merged_play <- left_join(df_merged_play, game_team[,c(1,3,4)], by = "Game_id")
df_merged_play[which(is.na(df_merged_play$Person1_Team_id) & df_merged_play$Event_Msg_Type == 8),"Person1_Team_id"] <- df_merged_play[which(is.na(df_merged_play$Person1_Team_id) & df_merged_play$Event_Msg_Type == 8),"Person2_Team_id"]  
temp <- df_merged_play %>%
  filter(is.na(Person1_Team_id)) %>%
  left_join(a, by = c("Person1"="Person_id"))
temp[,8:15] <- apply(temp[,8:15],2,as.character)
for (i in 1:nrow(temp)){
temp$Person1_Team_id[i] <- ifelse(temp$Team_id[i] == temp$team1[i] | temp$Team_id[i] == temp$team2[i], temp$Team_id[i], NA)
}

unique_game <- unique(game_lineup[,c(1,3,4)])
df <- df_scored %>%
  left_join(unique_game, by = c("Game_id", "Person1"="Person_id"))
sub_team <- df_merged_play %>%
  filter(Event_Msg_Type == 8) %>%
  select(Game_id, Person1, Person1_Team_id)
sub_team <- unique(sub_team)
colnames(sub_team)[3] <- "sub_Team_id"
df_sub_merged <- df_merged_play %>%
  left_join(sub_team, by = c("Person1", "Game_id"))
library(arsenal)
summary(compare(df_merged_play, df_sub_merged))

u <- unique(df_sub_merged)
d <- df_sub_merged[which(identical(df_sub_merged, df_merged_play) == FALSE),]
##################################
for (i in 1:nrow(df_merged_play)) {
  if (is.na(df_merged_play$Person1_Team_id[i])) {
    df_merged_play$Person1_Team_id[i] <- unique(filter(game_lineup, game_lineup$Person_id[i] == df_merged_play$Person1[i])[,3:4])[,2]
  }
}
for (i in 1:nrow(df_merged_play)) {
  temp_team_id[i] <- unique(filter(game_lineup, game_lineup$Person_id[i] == df_merged_play$Person1[i])[,3:4])[,2]
}
a <- NA
for (i in 1:nrow(df_merged_play)) {
  a[i] <- ifelse(df_merged_play$Person1_Team_id[i] == df_merged_play$team1[i] | df_merged_play$Person1_Team_id[i] == df_merged_play$team2[i], i, NA)
}
a <- data.frame(a)
for (i in 1:nrow(df_merged_play)){
  tryCatch({
    print(i)
    if (is.na(df_merged_play$Person1_Team_id[i])) {
      df_merged_play$Person1_Team_id[i] <- unique(filter(game_lineup, game_lineup$Person_id[i] == df_merged_play$Person1[i])[,3:4])[,2]
    } else {
      df_merged_play$Person1_Team_id[i] <- df_merged_play$Person1_Team_id[i]
    }}, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}
sub_id <- df_merged_play %>%
  filter(Event_Msg_Type == 8 & is.na(Person1_Team_id) == TRUE)
make_id <- df_merged_play %>%
  filter(Event_Msg_Type == 1 & is.na(Person1_Team_id) == TRUE)
na_team_id <- df_merged_play %>%
  filter(is.na(Person1_Team_id) == TRUE)
a <- unique(game_lineup[,3:4])
b <- unique(game_lineup[,c(1,3,4)])
c <- left_join(a, b, by = c("Person_id", "Team_id"))
rep_a <- a %>% 
  group_by(Person_id) %>%
  summarise(n=n()) %>%
  filter(n == 2) %>%
  left_join(b, by = "Person_id")

sub_id_line <- df_merged_play[which(is.na(df_merged_play$Team_id.y) & df_merged_play$Event_Msg_Type == 8), "Team_id.y"]
colnames(df_merged_play)[7] <- "Original_Team_id"
colnames(df_merged_play)[10] <- "Person1_Team_id"
df_merged_play2 <- left_join(df_merged_play, game_team)

t <- df_merged_play[-which(is.na(df_merged_play$Person2_Team_id)),]
identical(t$Person1_Team_id, t$Person2_Team_id)
#person1 and person2 are always in the same team