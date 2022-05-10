server <- function(input, output) {
  output$mainPlot = renderPlotly({
    autoplot(ts_df)
  })
  output$secondaryPlot = renderPlotly({
    if(input$plotType == "seasonality") {
      return(
        gg_season(ts_df)+ggtitle("Seasonality")
      )
    }
    else if(input$plotType == "autocorrelation") {
      return(autoplot(ACF(ts_df, hits))+ggtitle("Autocorrelation"))
    }
    else {
      return(
        model(ts_df, classical_decomposition(hits ~ season("1 year"), type = "multiplicative")) %>%
          components() %>%
          autoplot()
      )
    }
  })
  output$text = renderText({
    if(input$plotType == "seasonality") {
      return("From the plot, we can see that the number of searches for the song is relatively constant throughout the year until November, where they take off until peaking in late December and then returning to normal by January. There is a clear yearly seasonal trend here, with the all-time peak occurring in 2019.")
    }
    else if(input$plotType == "autocorrelation") {
      return("We see that the autocorrelation values are very high at the beginning, meaning that the first few lags are a good predictor of a given value. After that, the next few lags are not significant predictors until lag 8 when the remaining lags become good predictors again. If there was no seasonal pattern, we would expect that none of these would be significant, so the fact that the majority of them are significant shows a strong trend in the data.")
    }
    else {
      return("Starting by looking at the trend plot, we can see the peak of the songs popluarity near the end of 2019 (this can also be seen in the main plot at the top). Looking at the seasonal plot, we can see that the song is searched for approximatly 10 times as often as average during December, which is no supprise.")
    }
  })
  output$forecast = renderText({
    ts_df_with_lag <- ts_df %>%
      mutate(
        lag1 = lag(hits, 1),
        lag2 = lag(hits, 2),
        lag3 = lag(hits, 3),
        lag4 = lag(hits, 4),
      )
    
    paste(capture.output(ts_df_with_lag %>% model(TSLM(hits ~ lag1+lag2+lag3+lag4)) %>% report()), sep = '\n', collapse = '\n')
  })
  
  output$naivePlot = renderPlot({
    ts_df %>%
      model(NAIVE(hits)) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Naive")
  })
  
  output$seasNaivePlot = renderPlot({
    ts_df %>%
      model(SNAIVE(hits ~ lag(52))) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Seasonal Naive")
  })
  
  output$meanPlot = renderPlot({
    ts_df %>%
      model(MEAN(hits)) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Mean")
  })
  
  output$driftPlot = renderPlot({
    ts_df %>%
      model(RW(hits ~ drift())) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Drift")
  })
  
  output$holtsPlot = renderPlot({
    ts_df %>%
      model(ETS(hits ~ error("A") + trend("A") + season("N"))) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Holt's")
  })
  
  output$holtsWPlot = renderPlot({
    #ETS only allows periods of length 24 or less, so we have to decrease the length
    ts_with_skips = as_tsibble(ts_df[seq(1, nrow(ts_df), 4),], index = date)
    
    ts_with_skips %>%
      model(ETS(hits ~ error("M") + trend("Ad") + season("M", period = 13))) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "Holt's Winters")
  })
  
  output$arimaPlot = renderPlot({
    ts_df %>%
      model(ARIMA(hits)) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "ARIMA w/ Auto Parmaters")
  })
  #pdq(input$p, input$d, input$q) + PDQ(input$P, input$D, input$Q)
  output$chooseArimaPlot = renderPlot({
    ts_df %>%
      model(ARIMA(hits ~ pdq(as.integer(input$p), as.integer(input$d), as.integer(input$q)) + PDQ(as.integer(input$P), as.integer(input$D), as.integer(input$Q)))) %>%
      forecast(h=52) %>%
      autoplot(ts_df) + labs(title = "ARIMA w/ Chosen Parmaters")
  })
}
