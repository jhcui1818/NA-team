load("data/ggTrend_time.rda")
# create a timeline for plot from 2016-10-25 to 2018-04-11
timeline <- data.frame(seq(from = as.Date("2016/10/25"), to = as.Date("2018/04/11"), by = "day"))
colnames(timeline) <- "date"
library(dplyr)
ggTrend_time$date <- as.Date(ggTrend_time$date)
ggTrend_NBA <- filter(ggTrend_time, keyword == "NBA")
ggTrend_book <- filter(ggTrend_time, keyword == "book")
df_trend_NBA <- timeline %>%
  left_join(ggTrend_NBA, by = "date")
df_trend_book <- timeline %>%
  left_join(ggTrend_book, by = "date")

# trend are scaled per week, fill in search index for all dates
df_trend_NBA$old_hits <- df_trend_NBA$hits
df_trend_book$old_hits <- df_trend_book$hits
for(i in 1:(nrow(df_trend_NBA)-3)){
  while(is.na(df_trend_NBA$hits[i])) {
    subset <- df_trend_NBA[i:(i+7),]
    df_trend_NBA$hits[i] = subset$hits[is.na(subset$hits)==F]
  }
  i = i+7
}

for(i in 1:(nrow(df_trend_book)-3)){
  while(is.na(df_trend_book$hits[i])) {
    subset <- df_trend_book[i:(i+7),]
    df_trend_book$hits[i] = subset$hits[is.na(subset$hits)==F]
  }
  i = i+7
}

viewer$Game_Date <- as.Date(viewer$Game_Date)
df_trend_event <- viewer %>%
  left_join(df_trend_NBA, by = c("Game_Date" = "date")) %>%
  select(Game_ID, Viewers, Game_Date, Home, Away, All_Star, Weekday, hits)

# Splines for All_Star
library(boot)
cv.error.6 = rep(0,6)
for (i in 1:6){
  glm.fit = glm(Viewers ~ poly(All_Star, i), data = viewer)
  cv.error.6[i] = cv.glm(viewer, glm.fit, K=6)$delta[1]
}
plot(cv.error.6)
library(splines)
viewer$All_Star <- as.numeric(viewer$All_Star)
All_Star_lims <- range(viewer$All_Star)
All_Star_grid = seq(from=All_Star_lims[1], to=All_Star_lims[2])
fit1 = lm(Viewers ~ bs(All_Star, df = 5), data = viewer)
pred = predict(fit1, newdata = list(All_Star=All_Star_grid), se = T)
plot(viewer$All_Star, viewer$Viewers, col = "gray")
lines(All_Star_grid, pred$fit, lwd=2)
lines(All_Star_grid, pred$fit+2*pred$se, lty="dashed")
lines(All_Star_grid, pred$fit-2*pred$se, lty="dashed")

# Splines for hits
df_trend_event_noNA <- na.omit(df_trend_event)
library(boot)
cv.error.10 = rep(0,20)
for (i in 1:20){
  glm.fit = glm(Viewers ~ poly(hits, i), data = df_trend_event_noNA)
  cv.error.10[i] = cv.glm(df_trend_event_noNA, glm.fit, K=10)$delta[1]
}
plot(cv.error.10)
library(splines)
hits_lims <- range(df_trend_event_noNA$hits)
hits_grid = seq(from=hits_lims[1], to=hits_lims[2])
fit2 = lm(Viewers ~ bs(hits, df = 10), data = df_trend_event_noNA)
pred2 = predict(fit2, newdata = list(hits=hits_grid), se = T)
plot(df_trend_event_noNA$hits, df_trend_event_noNA$Viewers, col = "gray")
lines(hits_grid, pred2$fit2, lwd=2)
lines(hits_grid, pred2$fit2+2*pred2$se, lty="dashed")
lines(hits_grid, pred2$fit2-2*pred2$se, lty="dashed")

#GAM 