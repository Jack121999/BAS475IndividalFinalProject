library(gtrendsR)
library(fpp3)
library(shinyWidgets)
library(plotly)

trends = gtrends(keyword="All I want for Christmas is you")$interest_over_time
trends$hits <- as.numeric(ifelse(trends$hits == "<1", 0, trends$hits))

trends$date = as.Date(trends$date)

ts_df = as_tsibble(trends[c(1,2)], index = "date")

#https://jack121999.shinyapps.io/MidtermProject/