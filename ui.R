
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

shinyUI(fluidPage(

  # Application title
  titlePanel("Spectroscopy Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("calibration_desc.Rmd"),
      
      h4("Files with data"),
      
      fileInput("file_backg", "CSV File with background (nm, signal)"),
      fileInput("file_ref", "CSV File with reference (nm, signal)"),
      fileInput("file_sig", "CSV File with signal (nm, signal)"),

      numericInput("n_mirrors", "Specify a number of silver mirrors in the light path",
                   value = 3),
      
      h4("Additional optical Elements"),
      checkboxGroupInput("elements", label = "Choose elements in the optical path from a specimen to camera", 
                         choices = c("dichroic mirror blue (not done)", "dichroic mirror green (not done)", 
                                     "notch filter blue", "notch filter green (not done)")
      )
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      
      h4("Raw data with modifications corresponding to specified optical elements in path"),
      plotlyOutput("dataPlot"),
      
      
      h4("Calculate final intensity spectrum using a specific formula"),
      textInput("formula_text", 
                "Specify a formula using backg, ref, sig for intensity of background, reference, signal",
                "sig - backg", width = "100%"),
      actionButton("calculate", "Calculate"),
      plotlyOutput("finalSpectrum_plot"),
      DT::dataTableOutput("finalSpectrum_dt"),
      h4("Save final data to file"),
      downloadButton("download_dt", "Download final data")
    )
  )
))
