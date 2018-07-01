#devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)
ggTrend <- gtrends(c("book", "NBA"),gprop= "web", time = "2016-10-25 2018-04-11")
ggTrend_time <- ggTrend$interest_over_time
#save(ggTrend, file = "data/ggTrend_original.rda")
#save(ggTrend_time, file = "data/ggTrend_time.rda")