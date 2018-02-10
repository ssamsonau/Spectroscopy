
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
                       
                       checkboxInput("apply_calibration_correction", 
                                     "Apply calibration correction obtained in previous tab",
                                     value = T),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=903", 
                       #        "Silver mirrors"),
                       
                       ## Specify a number of silver mirrors in the light path
                       numericInput("n_mirrors", "",
                                    value = 4),
                       
                       # h4("Additional optical Elements:"),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=3313", 
                       #        "DMLP425R"),
                       # tags$br(),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=3313", 
                       #        "DMLP550R"),
                       # tags$br(),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=918", 
                       #        "FEL0450"),
                       # tags$br(),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=3880&pn=NF533-17", 
                       #        "NF533-17"),
                       # tags$br(),
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=4807", 
                       #        "BSW10R"),
                       # Choose elements in the optical path from a specimen to camera
                       checkboxGroupInput("elements", label = " ", 
                                          choices = c(" ")
                                          # ("Longpass Dichroic Mirror, 425 nm Cutoff",
                                          #             "Longpass Dichroic Mirror, 550 nm Cutoff", 
                                          #             "Longpass Filter, 450 nm Cutoff", 
                                          #             "Notch Filter green, 533 nm", 
                                          #             "50:50 (R:T) Plate Beamsplitter")
                       )
      ),
      conditionalPanel(condition="input.conditionedPanels==4",
                       radioButtons("notes_type", 
                                    "Measurement", 
                                    choices = c("Absorbance/Transmittance",
                                                "Fluorescence", 
                                                "Raman", 
                                                "Source characterization",
                                                "Thin film thickness measurement") 
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
                 actionButton("dataPlot_rescale_but", "Rescale y-Axis for data"),
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
                 actionButton("finalSpectrum_plot_rescale_but", "Rescale y-Axis for data"),
                 plotOutput("finalSpectrum_plot",
                            brush = brushOpts(
                              id = "finalSpectrum_plot_brush",
                              resetOnNew = TRUE
                            ),
                            dblclick = "finalSpectrum_plot_dblclick", 
                            hover = "finalSpectrum_plot_hover"),
                 verbatimTextOutput("finalSpectrum_plot_hover_text"),
                 fileInput("file_compare", 
                           "CSV File with data of spectrum, used to compare (nm, signal). This data will be rescaled to fit the same y range", 
                           width = "100%"),
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
                                  includeMarkdown("notes/Raman.Rmd")
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
