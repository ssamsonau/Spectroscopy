
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
      
      conditionalPanel(condition="input.conditionedPanels==1",
                       includeMarkdown("calibration_1.Rmd")
                       ),
      conditionalPanel(condition="input.conditionedPanels==2",
                       
                       includeMarkdown("calibration_2.Rmd"),
                       
                       fileInput("file_calibrate_reference", 
                                 "CSV File with data for calibration - Reference (nm, signal)"),
                       fileInput("file_calibrate_signal", 
                                 "CSV File - Collected (nm, signal)")
      ),
      
 
      conditionalPanel(condition="input.conditionedPanels==3",
                       fileInput("file_backg", "CSV File with background (nm, signal)"),
                       fileInput("file_ref", "CSV File with reference (nm, signal)"),
                       fileInput("file_sig", "CSV File with signal (nm, signal)"),
                       
                       checkboxInput("apply_collibration_correction", 
                                     "Apply collibratin correction obtained in previous tab",
                                     value = T),
                       
                       numericInput("n_mirrors", "Specify a number of silver mirrors in the light path",
                                    value = 4),
                       
                       h4("Additional optical Elements"),
                       checkboxGroupInput("elements", label = "Choose elements in the optical path from a specimen to camera", 
                                          choices = c("dichroic mirror blue (not done)", "dichroic mirror green (not done)", 
                                                      "notch filter blue", "notch filter green (not done)")
                       )
      ),
      conditionalPanel(condition="input.conditionedPanels==4",
                       radioButtons("notes_type", 
                                    "Measurement", 
                                    choices = c("Absorbance/Transmittance",
                                                "Fluorescence", 
                                                "Raman", 
                                                "Source characterization") 
                                    )
                       )
      
      
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Calibrate wavelength position", value = 1
                 
                 ),
        
        tabPanel("Calibrate spectum shape", value = 2,
                 
                 plotOutput("calibration_plot"), 
                 checkboxInput("calibration_see_corrected_on_plot", 
                               "See corrected signal on plot")
                 
                 ),
        tabPanel("Work with the signal", value = 3,
                 h4("Raw data with modifications corresponding to specified optical elements in path"),
                 h5("To magnify: Select area and double-click."),
                 actionButton("dataPlot_reset_but", "Reset Magnification"),
                 plotOutput("dataPlot", 
                            brush = brushOpts(
                              id = "dataPlot_brush",
                              resetOnNew = TRUE
                            ),
                            dblclick = "dataPlot_dblclick"),
                 
                 
                 h4("Calculate final intensity spectrum using a specific formula"),
                 # tags$li("In case of absorption spectroscopy. Data collected with light off: background, light on and empty quvette: reference, light one and liquid under study inside the cuvvete: signal. 
                 #         Formula "),
                 textInput("formula_text", 
                           "Specify a formula using backg, ref, sig for intensity of background, reference, signal",
                           "sig - backg", width = "100%"),
                 checkboxInput("is_raman", "Is this Raman spectrum? (choose before pressign calculate or press calculate again)", 
                               value = F, width = "100%"),
                 conditionalPanel('input.is_raman',
                                  numericInput("laser_wavelength", "Specify laser wavelength for Raman Shift calculation",
                                               value = 532, width = "100%")
                 ),
                 
                 actionButton("calculate", "Calculate (Recalculate)"),
                 
                 h5("To magnify: Select area and double-click."),
                 actionButton("finalSpectrum_plot_reset_but", "Reset Magnification"),
                 plotOutput("finalSpectrum_plot",
                            brush = brushOpts(
                              id = "finalSpectrum_plot_brush",
                              resetOnNew = TRUE
                            ),
                            dblclick = "finalSpectrum_plot_dblclick", 
                            hover = "finalSpectrum_plot_hover"),
                 verbatimTextOutput("finalSpectrum_plot_hover_text"),
                 h4("Table with data"),
                 DT::dataTableOutput("finalSpectrum_dt"),
                 h4("Save final data to file"),
                 downloadButton("download_dt", "Download final data")
                 
                 ),
        
       
        tabPanel("Applications notes", value = 4,
                 conditionalPanel(condition="input.notes_type == 'Absorbance/Transmittance'",
                                  h6("films"),
                                  h6("Beer-Lambert with cuvette https://www.chemguide.co.uk/analysis/uvvisible/beerlambert.html")
                                  ),
                 
                 conditionalPanel(condition="input.notes_type == 'Fluorescence'",
                                  h4("Fluorescence")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Raman'",
                                  h4("Raman")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Source characterization'",
                                  h4("Source characterization")
                 )
                 
                 
                 ),
                 
                 
        
        id = "conditionedPanels"
      )
      
     
     
    )
  )
))
