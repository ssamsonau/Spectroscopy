
# This is the user-interface definition of a Shiny web application.
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
                       
                       includeMarkdown("calibration_2.Rmd")
      ),
      
 
      conditionalPanel(condition="input.conditionedPanels==3",
                       fileInput("file_backg", "CSV File with background (nm, signal)"),
                       fileInput("file_ref", "CSV File with reference (nm, signal)"),
                       fileInput("file_sig", "CSV File with signal (nm, signal)"),
                       
                       checkboxInput("apply_calibration_correction", 
                                     "Apply calibration correction obtained in previous tab",
                                     value = T)
                       # tags$a(href="https://www.thorlabs.com/newgrouppage9.cfm?objectgroup_id=903", 
                       #        "Silver mirrors"),
                       
                       ## Specify a number of silver mirrors in the light path
                       # numericInput("n_mirrors", "",
                       #              value = 4),
                       
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
                       # checkboxGroupInput("elements", label = " ", 
                       #                    choices = c(" ")
                       #                    # ("Longpass Dichroic Mirror, 425 nm Cutoff",
                       #                    #             "Longpass Dichroic Mirror, 550 nm Cutoff", 
                       #                    #             "Longpass Filter, 450 nm Cutoff", 
                       #                    #             "Notch Filter green, 533 nm", 
                       #                    #             "50:50 (R:T) Plate Beamsplitter")
                       # )
      ),
      conditionalPanel(condition="input.conditionedPanels==4",
                       radioButtons("notes_type", 
                                    "Measurement", 
                                    choices = c(
                                      "General Considerations",
                                      "Transmittance, Extinction",
                                                "Fluorescence", 
                                                "Raman", 
                                                "Source characterization",
                                                "Reflectance, Color",
                                                "Thickness") 
                                    )
                       )
      
      
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Calibrate wavelength position", value = 1
                 
                 ),
        
        tabPanel("Calibrate spectum shape", value = 2,
                 h4(""), 
                 fileInput("file_calibrate_reference", 
                           "CSV File with data for calibration - Reference (nm, signal)", 
                           width = "100%"),
                 
                 # checkboxInput("fiber_correction", 
                 #               "Apply correction to reference signal, 
                 #               based on absorption of fiber optics cable 
                 #               (Expected value after fiber cable will decrease, 
                 #               comparing to rated values for lamp)",
                 #               value = F, width = "100%"),
                 fileInput("file_calibrate_signal", 
                           "CSV File - Collected (nm, signal)", width = "100%"),
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
                 uiOutput("spline_n_ui"),
                 checkboxInput("hide_dots", "Hide data points" , value = F),
                 
                 verbatimTextOutput("finalSpectrum_plot_hover_text"),
                 fileInput("file_compare", 
                           "CSV File with data of spectrum, used to compare (nm, signal). This data will be rescaled to fit the same y range", 
                           width = "100%"),
                 
                 h5("Color for the final spectrum is calculated and shown bellow:"),
                 tags$li("Please note, 
                    it may not match exactly what you see - this color would be seen if object is 
                    under perfectly flat white light (yes?)."),
                 tags$li("More details here:"),
                 tags$a("https://en.wikipedia.org/wiki/CIE_1931_color_space#Computing_XYZ_From_Spectral_Data"),
                 tags$li("Color calculated based on 360-740 nm wavelenght range"),
                 tags$li("Gray dots used to alling plot created in Mathematica with coordiantes in R plot"),
                 tags$li("Empty circle shows position of a white color"),
                 
                 plotOutput("color_plot", width = "700px", height = "650px"),
                 h4("Final color"),
                 verbatimTextOutput("color_text"),
                 
                 h4("Table with data"),
                 DT::dataTableOutput("finalSpectrum_dt"),
                 h4("Save final data to file"),
                 downloadButton("download_dt", "Download final data")
                 
                 ),
        
       
        tabPanel("Applications notes", value = 4,
                 conditionalPanel(condition="input.notes_type == 'Raman'",
                                  includeMarkdown("notes/Raman.Rmd")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Reflectance, Color'",
                                  includeMarkdown("notes/Reflectance, Color.Rmd")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Thickness'",
                                  includeMarkdown("notes/Thickness.Rmd")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Transmittance, Extinction'",
                                  withMathJax(includeMarkdown("notes/Transmittance, Extinction.Rmd"))
                 ),
                 conditionalPanel(condition="input.notes_type == 'Fluorescence'",
                                  includeMarkdown("notes/Fluorescence.Rmd")
                 ),
                 conditionalPanel(condition="input.notes_type == 'General Considerations'",
                                  includeMarkdown("notes/General Considerations.Rmd")
                 ),
                 conditionalPanel(condition="input.notes_type == 'Source characterization'",
                                  includeMarkdown("notes/Source characterization.Rmd")
                 )
                 
                 
                 ),
                 
                 
        
        id = "conditionedPanels"
      )
      
     
     
    )
  )
))
