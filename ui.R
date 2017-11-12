
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

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
                         choices = c("dichroic mirror blue", "dichroic mirror green", 
                                     "notch filter blue", "notch filter green")
      )
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("dataPlot")
    )
  )
))
