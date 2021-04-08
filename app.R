##############################################################################
# 
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#############################################################################
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
td7dddeaths.germany2 <- tso(d7dddeaths.germany2, maxit = 50)
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


source('UI.R', local = TRUE)
source('Server.R')

shinyApp(ui = UI, server = Server)
