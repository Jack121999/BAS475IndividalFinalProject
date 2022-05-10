ui <- fluidPage(
  h1("Christmas Song Popularity"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotType", "Secondary Plot Type:",
                   c("Seasonality" = "seasonality",
                     "Autocorrelation" = "autocorrelation",
                     "Decomposition" = "decomposition")),
      h3("ARIMA Parameters:"),
      radioGroupButtons(
        inputId = "p",
        label = "p:",
        choices = c(0, 1, 2)
      ),
      radioGroupButtons(
        inputId = "d",
        label = "d:",
        choices = c(0, 1, 2)
      ),
      radioGroupButtons(
        inputId = "q",
        label = "q:",
        choices = c(0, 1, 2)
      ),
      radioGroupButtons(
        inputId = "P",
        label = "P:",
        choices = c(0, 1, 2)
      ),
      radioGroupButtons(
        inputId = "D",
        label = "D:",
        choices = c(0, 1, 2)
      ),
      radioGroupButtons(
        inputId = "Q",
        label = "Q:",
        choices = c(0, 1, 2)
      ),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Instuctions",
                           h5("The purpose of this application is to show the popularity of Christmas songs throughout the year. This is done by tracking the number of searches for the song \"All I Want for Christmas Is You\" by Mariah Carey using Google Trends. A graph that shows the popularity over time is found under the \"Main Plot\" tab, with a secondary plot found under the next tab. The secondary plot can be a seasonality plot, an ACF plot, or a classical decomposition of the time series. Which plot is shown is selected by the user with the radial buttons on the left sidebar. The last tab shows a summary of a linear forecast made using the first 4 lags of the time series.") 
                  ),
                  tabPanel("Main Plot",
                           plotlyOutput("mainPlot")
                  ),
                  
                  tabPanel("Secondary Plot",
                           plotlyOutput("secondaryPlot"),
                           textOutput("text")
                  ),
                  
                  tabPanel("Lag Forcast Model",
                           h2("Forecast Model Using 4 Lags"),
                           verbatimTextOutput("forecast")
                  ),
                  tabPanel("Simple Models",
                           plotOutput("naivePlot"),
                           plotOutput("seasNaivePlot"),
                           plotOutput("meanPlot"),
                           plotOutput("driftPlot"),
                  ),
                  tabPanel("Exp. Smoothing",
                           plotOutput("holtsPlot"),
                           plotOutput("holtsWPlot")
                  ),
                  tabPanel("ARIMA",
                           plotOutput("arimaPlot"),
                           plotOutput("chooseArimaPlot")
                  ),
      )
    )
  )
)
