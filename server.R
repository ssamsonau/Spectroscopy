
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

shinyServer(function(input, output) {

  values <- reactiveValues()
  
  observeEvent(input$file_backg, {
    #browser()
    values$backg_or <- readr::read_csv(input$file_backg$datapath)
    names(values$backg_or) <- c("wavelength", "intensity")
    values$backg_or$type <- "background"
  })
  
  observeEvent(input$file_ref, {
    values$ref_or <- readr::read_csv(input$file_ref$datapath)
    names(values$ref_or) <- c("wavelength", "intensity")
    values$ref_or$type <- "reference"
  })
  
  observeEvent(input$file_sig, {
    values$sig_or <- readr::read_csv(input$file_sig$datapath)
    names(values$sig_or) <- c("wavelength", "intensity")
    values$sig_or$type <- "signal"
  })

  
  raw_data <- reactive({
    if(all(sapply(c("backg_or", "ref_or", "sig_or"),
                  function(x) is.null(values[[x]]))))
      return()
    
    bind_rows(values$backg_or, values$ref_or, values$sig_or)
  })


    
  combined_data <- reactive({
    if(is.null(raw_data()))
      return(NULL)
    
    read_convert_make_f <- function(file_path, wl_unit){
      #browser()
      modification <- readr::read_csv(file_path)      
      names(modification) <- c("wavelength", "percentages")
      #range(modification$wavelength, na.rm = T)
      
      # in file it is in percentage - convert to fraction
      if(wl_unit == "um")
        f <- approxfun(x = modification$wavelength * 1000, y = modification$percentages/100)
      if(wl_unit == "nm")
        f <- approxfun(x = modification$wavelength, y = modification$percentages/100)  
      
      f
    }
  
    dt <- raw_data()
    
    # modifications according to calibration on tungsten lamp
    correction_from_calibration <- compare_calibration_data() %>% filter(type == "correction_300") %>%
      select(intensity_norm) %>% unlist()
    
    if(input$apply_calibration_correction){
      dt <- dt %>%
        group_by(type) %>%
        mutate(intensity = ifelse(type != "background", 
                                  intensity * correction_from_calibration , 
                                  intensity)) %>%
        ungroup()  
    }
    
    
    if(!is.null(input$n_mirrors) & input$n_mirrors != 3){
      
      f <- read_convert_make_f("optical_elements_data/silver_mirror_from_Metallic_Coating_Reflection_Data.csv",
                               "um")
      #f(300)
      
      # do modification for larger or lower number of mirrors in path comparing to 3
      # 3 is a base number, as calibration is done using 3 mirrors
      # for example if there are 4 mirrors. let say for given wl refl is 0.9. Then the signal we read was actually larger so we have to divide by 0.9
      # if there is one less mirror - signal was reduced less than in calibration - multiply by 0.9
      dt <- dt %>%
        group_by(type) %>%
        mutate(intensity = ifelse(type != "background", 
                                  intensity / (f(wavelength))^(input$n_mirrors - 3) , 
                                  intensity)) %>%
        ungroup()
    }
    
      
    if(!is.null(input$elements))
      for(nn in input$elements){
        f <- switch(nn, 
               "Notch Filter green, 533 nm" = {
                  read_convert_make_f("optical_elements_data/NF533-17_data.csv", "nm")
                  #f(400)
                  # transmission data: signal was larger than recieved one
               },
               "Longpass Filter, 450 nm Cutoff" = {
                 read_convert_make_f("optical_elements_data/FEL0450_data.csv", "nm")
                 #f(400)
                 # transmission data: signal was larger than recieved one
               },
               "Longpass Dichroic Mirror, 550 nm Cutoff" = {
                 read_convert_make_f("optical_elements_data/DMLP550_data.csv", "nm")
                 #f(400)
                 # transmission data: signal was larger than recieved one
               },
               "Longpass Dichroic Mirror, 425 nm Cutoff" = {
                 read_convert_make_f("optical_elements_data/DMLP425_data.csv", "nm")
                 #f(400)
                 # transmission data: signal was larger than recieved one
               },
               "50:50 (R:T) Plate Beamsplitter" = {
                 read_convert_make_f("optical_elements_data/BSW10_data.csv", "nm")
                 #f(400)
                 # transmission data: signal was larger than recieved one
               }
               )
        
        dt <- dt %>%
          group_by(type) %>%
          mutate(intensity = ifelse(type != "background", 
                                    intensity / f(wavelength), 
                                    intensity)) %>%
          ungroup()
      }
        
    dt
  })
  
  output$dataPlot <- renderPlot({
    
    if(is.null(combined_data()))
      return()
    
    ggplot(combined_data(), aes(x = wavelength, y = intensity, color = type)) +
      geom_point() +
      coord_cartesian(xlim = ranges_dataPlot$x, ylim = ranges_dataPlot$y, expand = FALSE)
    
  })
  
  ranges_dataPlot <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$dataPlot_dblclick, {
    #browser()
    brush <- input$dataPlot_brush
    if (!is.null(brush)) {
      ranges_dataPlot$x <- c(brush$xmin, brush$xmax)
      ranges_dataPlot$y <- c(brush$ymin, brush$ymax)
      
    } 
  })
  
  observeEvent(input$dataPlot_reset_but, {
    ranges_dataPlot$x <- NULL
    ranges_dataPlot$y <- NULL
  })
  
  observeEvent(input$dataPlot_rescale_but, {
    x_name <- rlang::sym(names(combined_data())[1])
    y_name <- rlang::sym(names(combined_data())[2])
    
    new_y_lim <- combined_data() %>%
      filter(between(!!x_name, ranges_dataPlot$x[1], ranges_dataPlot$x[2])) %>%
      summarise(max = max(!!y_name), min = min(!!y_name)) %>%
      unlist
    
    ranges_dataPlot$y <- new_y_lim
  })
  
  ranges_finalSpectrum_plot <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$finalSpectrum_plot_dblclick, {
    #browser()
    brush <- input$finalSpectrum_plot_brush
    if (!is.null(brush)) {
      ranges_finalSpectrum_plot$x <- c(brush$xmin, brush$xmax)
      ranges_finalSpectrum_plot$y <- c(brush$ymin, brush$ymax)
      
    }
  })
  
  observeEvent(input$finalSpectrum_plot_reset_but, {
    ranges_finalSpectrum_plot$x <- NULL
    ranges_finalSpectrum_plot$y <- NULL
  })
  
  observeEvent(input$finalSpectrum_plot_rescale_but, {
    #browser()
    x_name <- rlang::sym(names(finalSpectrum())[1])
    y_name <- rlang::sym(names(finalSpectrum())[2])
    
    new_y_lim <- finalSpectrum() %>%
      filter(between(!!x_name, ranges_finalSpectrum_plot$x[1], ranges_finalSpectrum_plot$x[2])) %>%
      summarise(max = max(!!y_name), min = min(!!y_name)) %>%
      unlist
    
    ranges_finalSpectrum_plot$y <- new_y_lim
  })
  
  
  finalSpectrum <- reactive({
    #browser()
    input$calculate
    input$file_compare
    
    isolate({
      
      wl <- combined_data()$wavelength
      
      backg <- combined_data() %>%
        filter(type == "background") %>%
        select(intensity) %>% unlist 
      
      ref <- combined_data() %>%
        filter(type == "reference") %>%
        select(intensity) %>% unlist 
      
      sig <- combined_data() %>%
        filter(type == "signal") %>%
        select(intensity) %>% unlist 
      
      res <- eval(parse(text = input$formula_text))
      
      # only needed part of wl 
      wl <- wl[seq_along(res)]
      
      if(input$is_raman){
        dt <- tibble(
          RamanShift = ((1/input$laser_wavelength) - (1/wl))*1E7,
          Intensity = res
        )
      }else{
        dt <- tibble(
          wavelength = wl,
          Intensity = res
        )
      }
      
      dt$type <- rep("Final Spectrum", nrow(dt))
      
      if(!is.null(input$file_compare)){
        #browser()
        compare_signal_path <- input$file_compare$datapath
        
        compare_signal <- readr::read_csv(compare_signal_path)
        names(compare_signal) <- c(names(dt)[1], names(dt)[2])
        compare_signal$type <- "compare"  
        
        #browser()
        dt <- bind_rows(dt, compare_signal)
      }
      
    })
    
    dt
  })
  
  output$finalSpectrum_plot <- renderPlot({
    
    dt <- finalSpectrum()
    if(is.null(dt))
      return(NULL)
    dt_for_fitting <- dt %>% filter(type == "Final Spectrum")
    
    p <- ggplot(dt, aes_string(x = names(dt)[1], y = names(dt)[2], color = "type")) +
      geom_point() +
      geom_line(data=data.frame(spline(dt_for_fitting, n = 3 * nrow(dt_for_fitting)), 
                                "type" = rep("Spline fit", 3 * nrow(dt_for_fitting))), 
                aes(x = x, y = y, color = type)) +
      coord_cartesian(xlim = ranges_finalSpectrum_plot$x, 
                      ylim = ranges_finalSpectrum_plot$y, expand = FALSE) +
      xlab(paste0(names(finalSpectrum())[1], " (", ifelse(input$is_raman, "cm-1", "nm"), ")"))
    
    p
  })
  
  output$finalSpectrum_dt <- DT::renderDataTable({
    
    if(is.null(finalSpectrum()))
      return()
    
    finalSpectrum()
  }, rownames = F)
  
  output$download_dt <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("Data_", Sys.time(), ".csv")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {# Write to a file specified by the 'file' argument
      write.table(finalSpectrum(), file, sep = ",",
                  row.names = FALSE)
    }
  )
  
  
  output$finalSpectrum_plot_hover_text <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    paste( "Selected coordinates: ", xy_str(input$finalSpectrum_plot_hover))
  })
  
  
  
  ##################
  
  source("tab_2_calibr_spectrum.R", local = T)

})


