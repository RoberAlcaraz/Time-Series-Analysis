###############################################################################
# SERVER FUNCTION
###############################################################################
pacman::p_load(shiny, tidyverse, plotly, shinythemes, shinyjs, DT,
               ggpubr, COVID19, forecast, tsoutliers, portes, MTS, tseries)

# Cleaning
germany <- COVID19::covid19(country = "Germany")
italy <- COVID19::covid19(country = "Italy")

# Final date 26/03/2021
germany <- germany[1:431, ]
italy <- italy[1:430, ]

datesitaly <- italy$date
datesitaly <- datesitaly[42:nrow(italy)]
datesgermany <- germany$date
datesgermany <- datesgermany[44:nrow(germany)]

pop.germany <- germany$population[1]
pop.italy <- italy$population[1]

italy <- italy[, c("confirmed", "deaths", "date")]
germany <- germany[, c("confirmed", "deaths", "date")]

germany <- germany[44:nrow(germany), ]
italy <- italy[42:nrow(italy), ]

germany[, c(1,2)] <- germany[,c(1,2)]*100000/pop.germany
italy[, c(1,2)] <- italy[,c(1,2)]*100000/pop.italy

ts.germany <- ts(germany, start = c(2020, 55), frequency = 365)
ts.italy <- ts(italy, start = c(2020, 55), frequency = 365)

deaths.germany = ts.germany[, 2]
deaths.italy = ts.italy[, 2]

confirmed.germany = ts.germany[, 1]
confirmed.italy = ts.italy[, 1]

# GER
# deaths
ddeaths.germany=diff(deaths.germany,1)
dddeaths.germany=diff(ddeaths.germany,1)
d7dddeaths.germany=diff(dddeaths.germany,7)

d7dddeaths.germany1 <- ts(d7dddeaths.germany[1:40])
d7dddeaths.germany2 <- ts(d7dddeaths.germany[41:170])
d7dddeaths.germany3 <- ts(d7dddeaths.germany[171:length(d7dddeaths.germany)])

set.seed(123)
td7dddeaths.germany1 <- tso(d7dddeaths.germany1)
td7dddeaths.germany2 <- tso(d7dddeaths.germany2)
td7dddeaths.germany3 <- tso(d7dddeaths.germany3)

# confirmed
dconfirmed.germany=diff(confirmed.germany,1)
ddconfirmed.germany=diff(dconfirmed.germany,1)
d7ddconfirmed.germany=diff(ddconfirmed.germany,7)

d7ddconfirmed.germany1 <- ts(d7ddconfirmed.germany[1:40])
d7ddconfirmed.germany2 <- ts(d7ddconfirmed.germany[41:170])
d7ddconfirmed.germany3 <- ts(d7ddconfirmed.germany[171:length(d7ddconfirmed.germany)])

# ITA
# deaths
ddeaths.italy <- diff(deaths.italy,1)
dddeaths.italy <- diff(ddeaths.italy,1)
d7dddeaths.italy <- diff(dddeaths.italy,7)

ddeaths.italy <- diff(deaths.italy,1)
dddeaths.italy <- diff(ddeaths.italy,1)
d7dddeaths.italy <- diff(dddeaths.italy,7)

d7dddeaths.italy1 <- ts(d7dddeaths.italy[1:86])
d7dddeaths.italy2 <- ts(d7dddeaths.italy[87:181])
d7dddeaths.italy3 <- ts(d7dddeaths.italy[182:length(d7dddeaths.italy)])

set.seed(123)
td7dddeaths.italy1 <- tso(d7dddeaths.italy1)
td7dddeaths.italy2 <- tso(d7dddeaths.italy2, maxit = 50)
td7dddeaths.italy3 <- tso(d7dddeaths.italy3)
# td7dddeaths.italy3 <- auto.arima(d7ddconfirmed.italy3)

# confirmed
dconfirmed.italy=diff(confirmed.italy,1)
ddconfirmed.italy=diff(dconfirmed.italy,1)
d7ddconfirmed.italy=diff(ddconfirmed.italy,7)

d7ddconfirmed.italy1 <- ts(d7ddconfirmed.italy[1:86])
d7ddconfirmed.italy2 <- ts(d7ddconfirmed.italy[87:181])
d7ddconfirmed.italy3 <- ts(d7ddconfirmed.italy[182:length(d7ddconfirmed.italy)])

server <- function(input, output){
  
  output$plotG <- renderPlotly({
    data <- data.frame(
      day <- datesgermany,
      confirmed <- as.numeric(ts.germany[, "confirmed"]),
      deaths <- ts.germany[, "deaths"]
    )
    
    if (input$option == "Confirmed cases"){
      data %>%
        ggplot(aes(x = day, y = confirmed)) +
        geom_line() + ggtitle("Confirmed cases / 100.000 inhabitants (GER)") +
        theme_bw()
    } else if (input$option == "Deaths"){
      data %>%
        ggplot(aes(x = day, y = deaths)) +
        geom_line() + ggtitle("Deaths / 100.000 inhabitants (GER)") +
        theme_bw()
    }
    
  })
  
  output$ddgermany <- renderPlot({
    # layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    plot(dddeaths.germany)
  })
  
  output$acf <- renderPlot({
    par(mfrow = c(1, 2))
    acf(dddeaths.germany, main = "ACF 2nd difference")
    pacf(dddeaths.germany, main = "PACF 2nd difference")
  })
  
  output$d7ddgermany <- renderPlot({
    # layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    plot(d7dddeaths.germany)
  })
  
  output$acf7 <- renderPlot({
    par(mfrow = c(1, 2))
    acf(d7dddeaths.germany, main = "ACF 7th difference")
    pacf(d7dddeaths.germany, main = "PACF 7th difference")
  })
  
  output$period1 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.germany1)
    acf(d7dddeaths.germany1, main = "ACF 1st Period")
    pacf(d7dddeaths.germany1, main = "PACF 1st Period")
  })
  
  
  output$tso1 <- renderPrint({
    td7dddeaths.germany1
  })
  
  output$resid1 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.germany1$fit$residuals)
    acf(td7dddeaths.germany1$fit$residuals, main = "ACF 1st Period Residuals")
    pacf(td7dddeaths.germany1$fit$residuals, main = "PACF 1st Period Residuals")
  })
  
  output$qqplot1 <- renderPlot({
    qqnorm(td7dddeaths.germany1$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.germany1$fit$residuals)
  })
  
  output$jbt1 <- renderPrint({
    jarque.bera.test(td7dddeaths.germany1$fit$residuals)
  })
  output$b1 <- renderPrint({
    Box.test(td7dddeaths.germany1$fit$residuals, lag = 24)
  })
  
  output$period2 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.germany2)
    acf(d7dddeaths.germany2, main = "ACF 2nd Period")
    pacf(d7dddeaths.germany2, main = "PACF 2nd Period")
  })
  
  
  output$tso2 <- renderPrint({
    td7dddeaths.germany2
  })
  
  output$resid2 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.germany2$fit$residuals)
    acf(td7dddeaths.germany2$fit$residuals, main = "ACF 2nd Period Residuals")
    pacf(td7dddeaths.germany2$fit$residuals, main = "PACF 2nd Period Residuals")
  })
  
  output$qqplot2 <- renderPlot({
    qqnorm(td7dddeaths.germany2$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.germany2$fit$residuals)
  })
  
  output$jbt2 <- renderPrint({
    jarque.bera.test(td7dddeaths.germany2$fit$residuals)
  })
  output$b2 <- renderPrint({
    Box.test(td7dddeaths.germany2$fit$residuals, lag = 24)
  })
  
  output$period3 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.germany3)
    acf(d7dddeaths.germany3, main = "ACF 3rd Period")
    pacf(d7dddeaths.germany3, main = "PACF 3rd Period")
  })
  
  
  output$tso3 <- renderPrint({
    td7dddeaths.germany3
  })
  
  output$resid3 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.germany3$fit$residuals)
    acf(td7dddeaths.germany3$fit$residuals, main = "ACF 3rd Period Residuals")
    pacf(td7dddeaths.germany3$fit$residuals, main = "PACF 3rd Period Residuals")
  })
  
  output$qqplot3 <- renderPlot({
    qqnorm(td7dddeaths.germany3$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.germany3$fit$residuals)
  })
  
  output$jbt3 <- renderPrint({
    jarque.bera.test(td7dddeaths.germany3$fit$residuals)
  })
  output$b3 <- renderPrint({
    Box.test(td7dddeaths.germany3$fit$residuals, lag = 24)
  })
  
  output$plotI <- renderPlotly({
    data <- data.frame(
      day <- datesitaly,
      confirmed <- as.numeric(ts.italy[, "confirmed"]),
      deaths <- ts.italy[, "deaths"]
    )
    
    if (input$optionI == "Confirmed cases"){
      data %>%
        ggplot(aes(x = day, y = confirmed)) +
        geom_line() + ggtitle("Confirmed cases / 100.000 inhabitants (ITA)") +
        theme_bw()
    } else if (input$optionI == "Deaths"){
      data %>%
        ggplot(aes(x = day, y = deaths)) +
        geom_line() + ggtitle("Deaths / 100.000 inhabitants (ITA)") +
        theme_bw()
    }
    
  })
  
  output$dditaly <- renderPlot({
    # layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    plot(dddeaths.italy)
  })
  
  output$acfI <- renderPlot({
    par(mfrow = c(1, 2))
    acf(dddeaths.italy, main = "ACF 2nd difference")
    pacf(dddeaths.italy, main = "PACF 2nd difference")
  })
  
  output$d7dditaly <- renderPlot({
    # layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    plot(d7dddeaths.italy)
  })
  
  output$acf7I <- renderPlot({
    par(mfrow = c(1, 2))
    acf(d7dddeaths.italy, main = "ACF 7th difference")
    pacf(d7dddeaths.italy, main = "PACF 7th difference")
  })
  
  output$period1I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.italy1)
    acf(d7dddeaths.italy1, main = "ACF 1st Period")
    pacf(d7dddeaths.italy1, main = "PACF 1st Period")
  })
  
  
  output$tso1I <- renderPrint({
    td7dddeaths.italy1
  })
  
  output$resid1I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.italy1$fit$residuals)
    acf(td7dddeaths.italy1$fit$residuals, main = "ACF 1st Period Residuals")
    pacf(td7dddeaths.italy1$fit$residuals, main = "PACF 1st Period Residuals")
  })
  
  output$qqplot1I <- renderPlot({
    qqnorm(td7dddeaths.italy1$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.italy1$fit$residuals)
  })
  
  output$jbt1I <- renderPrint({
    jarque.bera.test(td7dddeaths.italy1$fit$residuals)
  })
  output$b1I <- renderPrint({
    Box.test(td7dddeaths.italy1$fit$residuals, lag = 24)
  })
  
  output$period2I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.italy2)
    acf(d7dddeaths.italy2, main = "ACF 2nd Period")
    pacf(d7dddeaths.italy2, main = "PACF 2nd Period")
  })
  
  
  output$tso2I <- renderPrint({
    td7dddeaths.italy2
  })
  
  output$resid2I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.italy2$fit$residuals)
    acf(td7dddeaths.italy2$fit$residuals, main = "ACF 2nd Period Residuals")
    pacf(td7dddeaths.italy2$fit$residuals, main = "PACF 2nd Period Residuals")
  })
  
  output$qqplot2I <- renderPlot({
    qqnorm(td7dddeaths.italy2$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.italy2$fit$residuals)
  })
  
  output$jbt2I <- renderPrint({
    jarque.bera.test(td7dddeaths.italy2$fit$residuals)
  })
  output$b2I <- renderPrint({
    Box.test(td7dddeaths.italy2$fit$residuals, lag = 24)
  })
  
  output$period3I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7dddeaths.italy3)
    acf(d7dddeaths.italy3, main = "ACF 3rd Period")
    pacf(d7dddeaths.italy3, main = "PACF 3rd Period")
  })
  
  
  output$tso3I <- renderPrint({
    td7dddeaths.italy3
  })
  
  output$resid3I <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(td7dddeaths.italy3$fit$residuals)
    acf(td7dddeaths.italy3$fit$residuals, main = "ACF 3rd Period Residuals")
    pacf(td7dddeaths.italy3$fit$residuals, main = "PACF 3rd Period Residuals")
  })
  
  output$qqplot3I <- renderPlot({
    qqnorm(td7dddeaths.italy3$fit$residuals, main = "QQ-plot for the residuals")
    qqline(td7dddeaths.italy3$fit$residuals)
  })
  
  output$jbt3I <- renderPrint({
    jarque.bera.test(td7dddeaths.italy3$fit$residuals)
  })
  output$b3I <- renderPrint({
    Box.test(td7dddeaths.italy3$fit$residuals, lag = 24)
  })
  
  output$upred <- renderPlotly({
    if (input$countr == "Italy"){
      
      npred <- input$n
      newxreg <- outliers.effects(td7dddeaths.italy3$outliers, length(d7dddeaths.italy3) + npred)
      newxreg <- ts(newxreg[-seq_along(d7dddeaths.italy3),])
      p <- predict(td7dddeaths.italy3$fit, n.ahead = npred, newxreg = newxreg)
      
      pred <- p$pred
      
      serie <- c(d7dddeaths.italy, pred)
      # serie <- diffinv(serie, lag = 7, xi = dddeaths.italy[1:7])
      # serie <- diffinv(serie, differences = 2, xi = deaths.italy[1:2])
      
      # dates <- datesitaly
      dates <- datesitaly[10:387]
      for (i in 1:npred){
        d <- datesitaly[387] + i
        dates <- append(dates, d)
      }
      # d.it <- c(ts.italy[, "deaths"], serie[387:(387+npred-1)])
      
      data <- data.frame(
        day <- dates,
        # deaths <- d.it,
        deaths <- serie,
        cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
      )
      
      data %>%
        ggplot() +
        geom_line(aes(x = day, y = deaths, color = cat)) +
        ggtitle(paste("Predictions for ", input$n, " days in the stationary serie (ITA)")) +
        theme_bw()
      
    } else if (input$countr == "Germany"){
      
      npred <- input$n
      newxreg <- outliers.effects(td7dddeaths.germany3$outliers, length(d7dddeaths.germany3) + npred)
      newxreg <- ts(newxreg[-seq_along(d7dddeaths.germany3),])
      p <- predict(td7dddeaths.germany3$fit, n.ahead = npred, newxreg = newxreg)
      
      pred <- p$pred
      
      serie <- c(d7dddeaths.germany, pred)
      # serie <- diffinv(serie, lag = 7, xi = dddeaths.germany[1:7])
      # serie <- diffinv(serie, differences = 2, xi = deaths.germany[1:2])
      
      # dates <- datesgermany
      dates <- datesgermany[10:387]
      for (i in 1:npred){
        d <- datesgermany[387] + i
        dates <- append(dates, d)
      }
      # d.ger <- c(ts.germany[, "deaths"], serie[387:(387+npred-1)])
      
      data <- data.frame(
        day <- dates,
        # deaths <- d.ger,
        deaths <- serie,
        cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
      )
      
      data %>%
        ggplot() +
        geom_line(aes(x = day, y = deaths, color = cat)) +
        ggtitle(paste("Predictions for ", input$n, " days in the stationary serie (GER)")) +
        theme_bw()
      
    }
  })
  
  output$confplot1 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7ddconfirmed.germany, lwd=2)
    
    plot(d7ddconfirmed.germany3, lwd=2, ylab="Confirmed cases per 100k inhabitants", main = "3rd Period")
    plot(d7dddeaths.germany3, lwd=2, ylab="Deaths per 100k inhabitants", main = "3rd Period")
  })
  
  output$p_values1 <- renderPrint({
    Eccm(cbind(d7dddeaths.germany3, d7ddconfirmed.germany3))
  })
  
  output$deaths1 <- renderPlotly({
    
    npred <- input$n1
    pred <- VARMApred(modger, h=npred)
    pred <- pred$pred[, 1]
    
    serie <- c(d7dddeaths.germany, pred)
    # serie <- diffinv(serie, lag = 7, xi = dddeaths.germany[1:7])
    # serie <- diffinv(serie, differences = 2, xi = deaths.germany[1:2])
    
    # dates <- datesgermany
    dates <- datesgermany[10:387]
    for (i in 1:npred){
      d <- datesgermany[387] + i
      dates <- append(dates, d)
    }
    # d.ger <- c(ts.germany[, "deaths"], serie[387:(387+npred-1)])
    
    data <- data.frame(
      day <- dates,
      # deaths <- d.ger,
      deaths <- serie,
      cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
    )
    
    data %>%
      ggplot() +
      geom_line(aes(x = day, y = deaths, color = cat)) +
      ggtitle(paste("Predictions for ", input$n1, " days in the stationary serie (GER)")) +
      theme_bw()
    
  })
  
  output$deaths1 <- renderPlotly({
    
    modger <- VARMA(cbind(d7dddeaths.germany3, d7ddconfirmed.germany3), p=2, q=6)
    npred <- input$n1
    pred <- VARMApred(modger, h=npred)
    pred <- pred$pred[, 1]
    
    serie <- c(d7dddeaths.germany, pred)
    # serie <- diffinv(serie, lag = 7, xi = dddeaths.germany[1:7])
    # serie <- diffinv(serie, differences = 2, xi = deaths.germany[1:2])
    
    # dates <- datesgermany
    dates <- datesgermany[10:387]
    for (i in 1:npred){
      d <- datesgermany[387] + i
      dates <- append(dates, d)
    }
    # d.ger <- c(ts.germany[, "deaths"], serie[387:(387+npred-1)])
    
    data <- data.frame(
      day <- dates,
      # deaths <- d.ger,
      deaths <- serie,
      cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
    )
    
    data %>%
      ggplot() +
      geom_line(aes(x = day, y = deaths, color = cat)) +
      ggtitle(paste("Predictions for ", input$n1, " days in the stationary serie (GER)")) +
      theme_bw()
    
  })
  
  output$confirmed1 <- renderPlotly({
    
    npred <- input$n1
    modger <- VARMA(cbind(d7dddeaths.germany3, d7ddconfirmed.germany3), p=2, q=6)
    pred <- VARMApred(modger, h=npred)
    pred <- pred$pred[, 2]
    
    serie <- c(d7ddconfirmed.germany, pred)
    # serie <- diffinv(serie, lag = 7, xi = ddconfirmed.italy[1:7])
    # serie <- diffinv(serie, differences = 2, xi = confirmed.italy[1:2])
    
    # dates <- datesitaly
    dates <- datesgermany[10:387]
    for (i in 1:npred){
      d <- datesgermany[387] + i
      dates <- append(dates, d)
    }
    # d.it <- c(ts.italy[, "confirmed"], serie[387:(387+npred-1)])
    
    data <- data.frame(
      day <- dates,
      # confirmed <- d.it,
      confirmed <- serie,
      cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
    )
    
    data %>%
      ggplot() +
      geom_line(aes(x = day, y = confirmed, color = cat)) +
      ggtitle(paste("Predictions for ", input$n1, " days in the stationary serie (GER)")) +
      theme_bw()
    
  })
  
  output$confplot2 <- renderPlot({
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
    
    plot(d7ddconfirmed.italy, lwd=2)
    
    plot(d7ddconfirmed.italy3, lwd=2, ylab="Confirmed cases per 100k inhabitants", main = "3rd Period")
    plot(d7dddeaths.italy3, lwd=2, ylab="Deaths per 100k inhabitants", main = "3rd Period")
  })
  
  output$p_values2 <- renderPrint({
    Eccm(cbind(d7dddeaths.italy3, d7ddconfirmed.italy3))
  })
  
  output$deaths2 <- renderPlotly({
    
    npred <- input$n2
    modita <- VARMA(cbind(d7dddeaths.italy3, d7ddconfirmed.italy3), p=5, q=3)
    pred <- VARMApred(modita, h=npred)
    pred$orig
    pred <- pred$pred[, 1]
    
    serie <- c(d7dddeaths.italy, pred)
    # serie <- diffinv(serie, lag = 7, xi = dddeaths.italy[1:7])
    # serie <- diffinv(serie, differences = 2, xi = deaths.italy[1:2])
    
    # dates <- datesitaly
    dates <- datesitaly[10:387]
    for (i in 1:npred){
      d <- datesitaly[387] + i
      dates <- append(dates, d)
    }
    # d.it <- c(ts.italy[, "deaths"], serie[387:(387+npred-1)])
    
    data <- data.frame(
      day <- dates,
      # deaths <- d.it,
      deaths <- serie,
      cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
    )
    
    data %>%
      ggplot() +
      geom_line(aes(x = day, y = deaths, color = cat)) +
      ggtitle(paste("Predictions for ", input$n2, " days in the stationary serie (ITA)")) +
      theme_bw()
    
  })
  
  output$confirmed2 <- renderPlotly({
    
    npred <- input$n2
    modita <- VARMA(cbind(d7dddeaths.italy3, d7ddconfirmed.italy3), p=5, q=3)
    pred <- VARMApred(modita, h=npred)
    pred <- pred$pred[, 2]
    
    serie <- c(d7ddconfirmed.italy, pred)
    # serie <- diffinv(serie, lag = 7, xi = ddconfirmed.italy[1:7])
    # serie <- diffinv(serie, differences = 2, xi = confirmed.italy[1:2])
    
    # dates <- datesitaly
    dates <- datesitaly[10:387]
    for (i in 1:npred){
      d <- datesitaly[387] + i
      dates <- append(dates, d)
    }
    # d.it <- c(ts.italy[, "confirmed"], serie[387:(387+npred-1)])
    
    data <- data.frame(
      day <- dates,
      # confirmed <- d.it,
      confirmed <- serie,
      cat <- as.factor(ifelse(day <= 18710, "Observations", "Predictions"))
    )
    
    data %>%
      ggplot() +
      geom_line(aes(x = day, y = confirmed, color = cat)) +
      ggtitle(paste("Predictions for ", input$n2, " days in the stationary serie (ITA)")) +
      theme_bw()
    
  })
  
}