
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggspectra)
library(photobiology)
library(photobiologyWavebands)

library(colorscience)
library(png)

shinyServer(function(input, output, session) {

  values <- reactiveValues()
  
  source("03_open_data_files.R", local = T)
  source("04_data_transformation.R", local = T)
  source("02_tab_2_calibr_spectrum.R", local = T)
  source("05_dataPlot.R", local = T)
  source("06_final_spectrum_plot.R", local = T)
  source("07_color_calculation.R", local = T)
  source("08_thickness.R", local = T)

  ##################
  

})


