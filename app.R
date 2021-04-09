##############################################################################
# 
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#############################################################################
pacman::p_load(shiny, tidyverse, plotly, shinythemes, shinyjs, DT,
               ggpubr, COVID19, forecast, tsoutliers, portes, MTS, tseries)

source('UI.R', local = TRUE)
source('Server.R')

shinyApp(ui = UI, server = Server)
