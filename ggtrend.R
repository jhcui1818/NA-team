#devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)
library(ggplot2)
library(lubridate)
ggTrend <- gtrends(c("book", "NBA"),gprop= "web", time = "2016-10-25 2018-04-11")
ggTrend_time <- ggTrend$interest_over_time
#save(ggTrend, file = "data/ggTrend_original.rda")
#save(ggTrend_time, file = "data/ggTrend_time.rda")
ggplot(ggTrend_time, aes(date, hits, color = keyword)) +
  geom_point() +
  geom_line()

viewer$Weekday <- as.factor(viewer$Weekday)
ggplot(viewer, aes(Weekday, Viewers)) +
  geom_boxplot()

ggplot(viewer, aes(Home, Viewers)) +
  geom_boxplot()

ggplot(viewer, aes(Away, Viewers)) +
  geom_boxplot()

ggplot(viewer, aes(All_Star, Viewers)) +
  geom_boxplot()

ggplot(df_trend_event_noNA, aes(x = Game_Date)) +
  geom_point(aes(y=Viewers/1000)) +
  geom_point(aes(y=hits, color = "red"))
