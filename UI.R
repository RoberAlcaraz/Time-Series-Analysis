###############################################################################
# UI FUNCTION
###############################################################################

pacman::p_load(shiny, tidyverse, plotly, shinythemes, shinyjs, DT,
               ggpubr, COVID19, forecast, tsoutliers, portes, MTS, tseries)

introPanel <- tabPanel(
  "1. COVID-19",
  sidebarLayout(position = "right",
                sidebarPanel(
                  h3(strong("Description of the variables: ")),
                  HTML(paste0("<ul><li>", 
                              code("deaths"), ": Cumulative number of deaths/100k inhabitants.</li><li>", 
                              code("confirmed"), ": Cumulative number of confirmed cases/100k inhabitants.</li></ul>")
                  )
                ),
                mainPanel(
                  h4(em("Roberto J. Alcaraz Molina, Ander Iturburu Aperribay,
                        Sergio Palacio Vega")),
                  h4(em("26/03/2021")),
                  br(),
                  h1(strong("Introduction")),
                  br(),
                  p("In this project, we will analyze the impact of the Covid-19 
                    in Germany and Italy. To that end, we will mainly analyze the 
                    series of accumulated deaths per 100k inhabitants (in a specific
                    part of the project due to multivariate forecasting, we will 
                    also consider accumulated confirmed cases)."),
                  p("Firstly, we will start analyzing the Covid series in Germany,
                    doing a visual description to observe how the quantity of 
                    confirmed cases and deaths have evolved through time. Then, we
                    will identify a suitable model for the deaths, dividing the 
                    series in three different periods to estimate the different 
                    parameters and to forecast in a more reliable way. After that, 
                    we will do the same procedure for Italy."),
                  p("Finally, we will forecast the deaths and confirmed cases
                    within the next two months in both countries, both using a univariate
                    and a multivariate setting.")
                )
  )
)

GvizPanel <- tabPanel(
  "GER: Visual description",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("Firstly, we can observe a plot of the two quantitative series that we 
        have at hands, cummulative confirmed cases and deaths:"),
      br(),
      selectInput("option", label = NULL, choices = c("Confirmed cases", "Deaths")),
      p("The first thing we can observe in both time series is the effect of the
        lockdown. We can observe how Germany almost flattered 
        totally the curve having days of no fatalities and almost no infections.
        Then, if we split the series in two parts, considering the time before 
        and after lockdown, we might say that the curve has a sort of quadratic 
        form, being in the first part convex and in the second part, concave.")
    ),
    mainPanel(
      h3(),
      plotlyOutput("plotG")
    )
  )
)

modPanelG <- tabPanel(
  "GER: Model",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Identification and estimation of the model")),
      p("Now, we will try to identify the model of the cumulative deaths in Germany.
        As we have seen in the previous plot, we will first take the first-difference
        two times (second order difference basically) in order to flatten the quadratic 
        forms. In the first tab, we can observe that transformed series."),
      p("If we also look to the correlogram in the second tab, we can easily
        see that we still have some weekly seasonality, and consequently, we 
        will take the 7-lag difference of the series."),
      p("After all these changes, we can see in the third tab that have almost a
        stationary series (it has heteroskedasticity) and so, it is a very naive 
        approach trying to identify a suitable general model for the whole series. 
        In order to enhance the performance of the identification of the series though,
        we will split the series in three periods. We will take into account
        the different restrictions set by the country and the slope of the curve.
        Hence, the first period will be between 2020-03-03 and 2020-04-11. 
        The second series will be between 2020-04-12 and 2020-08-19 and the last
        part will be between 2020-08-20 and 2021-03-22. In order to identify 
        the models we will consider the existence of outliers, therefore, we will
        use the function tso to identify them. Moreover, after the identification, we
        will do also the model diagnosis. ")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("2nd difference", plotOutput("ddgermany")),
        tabPanel("ACF and PACF (2nd D)", plotOutput("acf")),
        tabPanel("7th difference", plotOutput("d7ddgermany")),
        tabPanel("ACF and PACF (7th D)", plotOutput("acf7"))
      )
    )
  )
)

modPanel1G <- tabPanel(
  "GER: 1st Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("First series (2020/03/03 - 2020/04/11)")),
      p("On the right (first tab), we can observe the series in the first period,
        the one corresponding to the beginning of the pandemic reality."),
      p("If we look to the correlogram and partial correlogram, we might think
        that we have a white noise model."),
      p("Moreover, the tso function suggests that we have white noise in this series
        with no outliers."),
      p("Then, we can see the residuals of the model, the QQ-plot and the 
        Jarque-Bera and Box-Pierce Tests."),
      p("If we look to the p-value of the Jarque Bera Test, we can consider that
        the errors are normally distributed and additionally, thanks to Box-Pierce
        test we assure that our residuals are white-noise, a fact that was 
        visually expected from the previous correlogram and partial correlograms.")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1st Period", plotOutput("period1")),
        tabPanel("TSO", verbatimTextOutput("tso1")),
        tabPanel("Residuals", plotOutput("resid1")),
        tabPanel("QQ-plot", plotOutput("qqplot1")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt1"), verbatimTextOutput("b1"))
      )
    )
  )
)

modPanel2G <- tabPanel(
  "GER: 2nd Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Second series (2020/04/12 - 2020/08/19)")),
      p("Clicking on the first tab, we can observe the series in this second period."),
      p("If we look to the correlogram and partial correlogram, it's not easy to
        determine which model we have. The reason for this could be the existence
        of outliers."),
      p("Using the tso function, it identifies our model as a AR(2) with four 
        temporary changes, a level shift and a additive outlier. The estimated parameters
        can be seen as well."),
      p("In the QQ-plot we can observe that the residual do not seem to be normal, 
        which can be reassured by Jarque Bera Test. Maybe, an extra step of analysis 
        would be needed to grasp this issue.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("2nd Period", plotOutput("period2")),
        tabPanel("TSO", verbatimTextOutput("tso2")),
        tabPanel("Residuals", plotOutput("resid2")),
        tabPanel("QQ-plot", plotOutput("qqplot2")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt2"), verbatimTextOutput("b2"))
      )
    )
  )
)

modPanel3G <- tabPanel(
  "GER: 3rd Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Third series (2020/08/20 - 2021/03/22)")),
      p("On the first tab, we can observe the series in the third period, the one
        (practically) after summer."),
      p("Correlogram and partial correlogram are not very clear (maybe an MA due
        to significant peaks in ACF y decaying pattern in PACF)."),
      p("Using the tso function, it identifies our series as an ARMA(1, 1) with five 
        additive outliers and three temporary changes, all of them taking place 
        in the central part of the sries, where it gets increased its value's range."),
      p("Again, the normality assumption seems to be failing."),
      p("It is turn now to analyze Italy's case.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("3rd Period", plotOutput("period3")),
        tabPanel("TSO", verbatimTextOutput("tso3")),
        tabPanel("Residuals", plotOutput("resid3")),
        tabPanel("QQ-plot", plotOutput("qqplot3")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt3"), verbatimTextOutput("b3"))
      )
    )
  )
)

IvizPanel <- tabPanel(
  "ITA: Visual description",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("Secondly, we can see a graphic for the two quantitative series of cumulative
        confirmed cases and deaths regarding Italy:"),
      br(),
      selectInput("optionI", label = NULL, choices = c("Confirmed cases", "Deaths")),
      p("The lockdown is also influencing the trend. If we compare the confirmed cases
        between Italy and Germany, we can see a more dramatic increase in Italy, 
        specially after summer. Moreover, the scale in the y axis for Italy is much greater.
        With respect to the deaths, there are some marked and important differences.
        The first one is that Italy arrives to 50 (per 100k inhabitants) deaths in May,
        while this took place in December in Germany. The second one is that the 
        trend in Germnay seems to be flattening while the one for Italy is still increasing.
        As in the case of Germany there is a sort of piecewise quadratic trend.")
    ),
    mainPanel(
      h3(),
      plotlyOutput("plotI")
    )
  )
)

modPanelI <- tabPanel(
  "ITA: Model",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Identification and estimation of the model")),
      p("Now, we will try to identify the model of the cumulative deaths in Italy.
        We will first take the first-difference two times in order to flatten the quadratic forms. 
        On the right, we can observe that transformed series."),
      p("If we look to the plot of the series and the correlogram, we can easily
        see that we still have some weekly seasonality, and consequently, we 
        will take the first differnece with lag 7 from the series."),
      p("If we look to correlogram and partial correlogram, we cannot easily set
        the type of model. The main issue is that we might have several
        outliers that difficult the process of identifying our time series and that,
        again, the series seems to be formed by three clear different time periods, 
        possibly related with some adopted measures. 
        In this case, the first series will be between 2020-03-03 and 2020-05-27. The 
        second series will be between 2020-05-28 and 2020-08-30 and the last 
        series will be between 2020-08-31 and 2021-03-22. In order to identify 
        the models, we will again consider the existence of outliers. Moreover we will carry
        out the model diagnosis.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("2nd difference", plotOutput("dditaly")),
        tabPanel("ACF and PACF (2nd D)", plotOutput("acfI")),
        tabPanel("7th difference", plotOutput("d7dditaly")),
        tabPanel("ACF and PACF (7th D)", plotOutput("acf7I"))
      )
    )
  )
)

modPanel1I <- tabPanel(
  "ITA: 1st Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("First series (2020/03/03 - 2020/05/27)")),
      p("On the right, we can observe the serie in this first period."),
      p("If we look to the correlogram and partial correlogram, the identification
        is not straightforward because we might have some outliers, but a MA could
        be possible due to the significant peak in the ACF."),
      p("Using the tso function, it suggests that indeed we have a MA(1) with an additive
        outlier"),
      p("If we look to the residuals, normality again is failing.")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1st Period", plotOutput("period1I")),
        tabPanel("TSO", verbatimTextOutput("tso1I")),
        tabPanel("Residuals", plotOutput("resid1I")),
        tabPanel("QQ-plot", plotOutput("qqplot1I")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt1I"), verbatimTextOutput("b1I"))
      )
    )
  )
)

modPanel2I <- tabPanel(
  "ITA: 2nd Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Second series (2020/05/28 - 2020/08/30)")),
      p("On the first tab, we can observe the series in this second period, that 
        has two marked peaks in August."),
      p("If we look to the correlogram and partial correlogram, a MA process could
        have been again logical. However, an AR(2) is identified with 15 outliers!"),
      p("Even if in the QQ-plot the residuals doesn't seem completely normal, the
        Jarque-Bera Test suggests that we really have a normal distribution.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("2nd Period", plotOutput("period2I")),
        tabPanel("TSO", verbatimTextOutput("tso2I")),
        tabPanel("Residuals", plotOutput("resid2I")),
        tabPanel("QQ-plot", plotOutput("qqplot2I")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt2I"), verbatimTextOutput("b2I"))
      )
    )
  )
)

modPanel3I <- tabPanel(
  "ITA: 3rd Period",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Third series (2020/08/31 - 2021/03/22)")),
      p("Finally, we can observe the serie in this third period."),
      p("If we look to the correlogram and partial correlogram, we could expect a MA. 
      Indeed tso identifies our series as a MA(1) with three additive outliers"),
      p("Looking to the correlogram and partial correlogram it doesn't seem that
        our residuals are white noise."),
      p("Looking to the values of Jarque-Bera test and Box-Pierce test we cannot
        determine that the residuals are normally distributed and uncorrelated.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("3rd Period", plotOutput("period3I")),
        tabPanel("TSO", verbatimTextOutput("tso3I")),
        tabPanel("Residuals", plotOutput("resid3I")),
        tabPanel("QQ-plot", plotOutput("qqplot3I")),
        tabPanel("Jarque-Bera and Box-Pierce Tests", verbatimTextOutput("jbt3I"), verbatimTextOutput("b3I"))
      )
    )
  )
)

ufPanel <- tabPanel(
  "Univariate forecasting",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("In the following panel we are going to predict the deaths / 100k inhabitants
        in an univariate way. Below we can select if want to see the series of Italy
        or Germany. If we select Italy, as we said before, we will have our predictions
        based on an MA(1). On the other hand, if we select Germany, our predictions
        will be based on an ARMA(1,1). This is becuase, due to have split our series 
        in three parts, we only take advantage of the third period while forecasting."),
      p("Recall that the main issue we have faced so far is approximate lack of 
        gaussianity of the residuals. This could has its origin in the heteroskedasticity,
        so a next step that could be done is trying to fit a non-linear model, ARCH 
        or GARCH models."),
      radioButtons("countr", label = "Select the country: ", choices = c("Italy", "Germany")),
      sliderInput("n", label = "Select the days to forecast: ", min = 1, max = 60, value = 30, step = 1),
    ),
    mainPanel(
      plotlyOutput("upred")
    )
  )
)

mfPanel1 <- tabPanel(
  "GER: Multivariate forecasting",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("In order to improve our predictions, we will briefly consider a 
        multivariate case. In this case, we are going to introduce here the 
        variable confirmed cases as a predictor. Due to the analogy between the 
        univariate and multivariate case, we will try to fit a VARMA model to
        both stationary series of deaths and confirmed cases for both Germany and 
        Italy, of course. Recall two important things here: firstly, that on the 
        confirmed cases the same transformations are needed to render them stationary 
        appart from the heroskedastic issue (second order difference with lag one 
        and then first order difference with lag 7); secondly, we will only consider
        the third period in all series since our objective is just a prediction improvement."),
      p("In the following plot we have the heterokedastic series of confirmed cases
        in Germany and then, we have the series of deaths and confirmed 
        cases in the third period (the one important for our prediction aim)."),
      br(),
      p("First of all, let us identify a suitable multivariate model for Germany. 
        We will use Extended Crosss-Correlation Matrices within the ", code("Eccm"),
        " function in the ", code("MTS"), " package. The function gives the p-values
        of multivariate Ljung-Box statistics of the vector time series formed by
        deaths and confirmed cases."),
      br(),
      p("Looking at the resulting table of p-values, and looking for the most
        parsimonious model possible, we take a VARMA(2, 6)."),
      p("Below, we can select the number of horizons to do the predictions: "),
      sliderInput("n1", label = NULL, min = 1, max = 60, value = 30, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Deaths and confirmed cases (GER)", plotOutput("confplot1")),
        tabPanel("p-values table of Extended CC Matrices", verbatimTextOutput("p_values1")),
        tabPanel("Prediction for deaths", plotlyOutput("deaths1")),
        tabPanel("Prediction for confirmed cases", plotlyOutput("confirmed1"))
      )
    )
  )
)

mfPanel2 <- tabPanel(
  "ITA: Multivariate forecasting",
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("In the following plot we have the heterokedastic series of confirmed cases
        in Italy and then, we have the series of deaths and confirmed 
        cases in the third period (the one important for our prediction aim)."),
      br(),
      p("First of all, let us identify a suitable multivariate model for Italy 
        We will use again the same procedure, Extended Crosss-Correlation Matrices
        within the ", code("Eccm"), " function. The function gives the p-values
        of multivariate Ljung-Box statistics of the vector time series formed by
        deaths and confirmed cases."),
      br(),
      p("Looking at the resulting table of p-values, we take a VARMA(5, 3)."),
      p("Below, we can select the number of horizons to do the predictions: "),
      sliderInput("n2", label = NULL, min = 1, max = 60, value = 30, step = 1)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Deaths and confirmed cases (ITA)", plotOutput("confplot2")),
        tabPanel("p-values table of Extended CC Matrices", verbatimTextOutput("p_values2")),
        tabPanel("Prediction for deaths", plotlyOutput("deaths2")),
        tabPanel("Prediction for confirmed cases", plotlyOutput("confirmed2"))
      )
    )
  )
)

concPanel <- tabPanel(
  "Conclusions",
  mainPanel(
    h1("Conlusions"),
    br(),
    p("As a conclussion, we have seen some interesting insights about our project:"),
    p("First of all, we had to divide the series in three parts because of the 
      different patterns they had. It is almost impossible to fit a suitable model
      the complete series."),
    p("Regarding the forecasting method, it is more powerful the multivariate than 
      univariate case, since in the univariate after a few steps all the predictions
      are 0 whereas, in the multivariate case, the predictions oscillate due to 
      the use of ARMA models with higher orders."),
    p("Finally, the predictions for the confirmed cases and deaths seem
      better in Germany than in Italy: in Italy we have much more variability 
      in our predictions than in Germany, as we might expect from the beggining,
      since Germany has dealed much better with the Covid-19 than Italy trough
      all the pandemic time.")
  )
)



ui <- navbarPage("Time Series Analysis",
                 theme = shinytheme("sandstone"),
                 introPanel,
                 GvizPanel,
                 modPanelG,
                 modPanel1G,
                 modPanel2G,
                 modPanel3G,
                 IvizPanel,
                 modPanelI,
                 modPanel1I,
                 modPanel2I,
                 modPanel3I,
                 ufPanel,
                 mfPanel1,
                 mfPanel2,
                 concPanel
)

