####
# shyny app for rythm metrics computing and display
####
library(shiny)
library(readtextgrid)
library(ggplot2)
library(dplyr)
library(gridExtra)

options(shiny.maxRequestSize=30*1024^2) 

# Create Shiny app ----
shinyApp(ui, server)

# ui contains the interface all returns must be added in the ui
#server contains the back operations and graphs creation
