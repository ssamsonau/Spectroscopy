
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
    if(all(sapply("backg_or", function(x) is.null(values[[x]]))))
      return()
    
    bind_rows(values$backg_or, values$ref_or, values$sig_or)
  })
  
  final_data <- reactive({
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
    
    if(!is.null(input$n_mirrors) & input$n_mirrors != 3){
      
      f <- read_convert_make_f("elements_data/silver_mirror_from_Metallic_Coating_Reflection_Data.csv",
                               "um")
      #f(300)
      
      # do modification for larger or lower number of mirrors in path comparing to 3
      # 3 is a base number, as calibration is done using 3 mirrors
      # for example if there are 4 mirrors. let say for given wl refl is 0.9. Then the signal we read was actually larger so we have to divide by 0.9
      # if there is one less mirror - signal was reduced less than in calibration - multiply by 0.9
      dt <- dt %>%
        group_by(type) %>%
        mutate(intensity = intensity / (f(wavelength))^(input$n_mirrors - 3) ) %>%
        ungroup()
    }
    
      
    if(!is.null(input$elements))
      for(nn in input$elements){
        switch(nn, 
               "notch filter blue" = {
                  f <- read_convert_make_f("elements_data/NF533-17_data.csv", "nm")
                  #f(400)
                  # transmission data: signal was larger than recieved one
                  dt <- dt %>%
                    group_by(type) %>%
                    mutate(intensity = intensity / f(wavelength)) %>%
                    ungroup()
               }
               )
      }
        
    dt
  })
  
  output$dataPlot <- renderPlotly({
    
    if(is.null(final_data()))
      return()
    
    ggplot(final_data()) +
      geom_point(aes(x = wavelength, y = intensity, color = type))
    ggplotly()
  })
  
  
  finalSpectrum <- eventReactive(input$calculate, {
    #browser()
    
    wl <- final_data()$wavelength
    
    backg <- final_data() %>%
      filter(type == "background") %>%
      select(intensity) %>% unlist 
    
    ref <- final_data() %>%
      filter(type == "reference") %>%
      select(intensity) %>% unlist 
    
    sig <- final_data() %>%
      filter(type == "signal") %>%
      select(intensity) %>% unlist 
    
    res <- eval(parse(text = input$formula_text))

    # only needed part of wl 
    wl <- wl[seq_along(res)]
    
    if(input$is_raman){
      dt <- tibble(
        RamanShift = ((1/532) - (1/wl))*1E7,
        Intensity = res
      )
    }else{
      dt <- tibble(
        wavelength = wl,
        Intensity = res
      )
    }
    
    dt
  })
  
  output$finalSpectrum_plot <- renderPlotly({
    
    dt <- finalSpectrum()
    if(is.null(dt))
      return()
    
    ggplot(dt) +
      geom_point(aes_string(x = names(dt)[1], y = names(dt)[2]))
    
    
    ggplotly()
  })
  
  output$units <- renderText({
    paste(names(finalSpectrum())[1], "is given in", ifelse(input$is_raman, "cm-1", "nm"))
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
  

})
