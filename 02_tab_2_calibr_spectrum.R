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


################## Calibrate
compare_calibration_data <- reactive({
  #browser()
  
  calibrate_signal_path <- ifelse(is.null(input$file_calibrate_signal),
                                  "calibration/tungsten SLS201/SLS201_position 2.asc",
                                  input$file_calibrate_signal$datapath)
  
  calibr_signal <- readr::read_csv(calibrate_signal_path)
  names(calibr_signal) <- c("wavelength", "intensity")
  calibr_signal$type <- "measured"
  
  
  calibrate_reference_path <- ifelse(is.null(input$file_calibrate_reference),
                                     "calibration/tungsten SLS201/SLS201L_Spectrum.csv",
                                     input$file_calibrate_reference$datapath)
  
  calibr_ref <- readr::read_csv(calibrate_reference_path) %>%
    select(1:2)
  
  names(calibr_ref) <- c("wavelength", "intensity")
  
  # if(input$fiber_correction){
  #   # adjust to fiber cable
  #   f <- read_convert_make_f("calibration/fiber transmission in percents (obtained as ratio of 2 fiber signal over 1 fiber signal).csv", "nm")
  #   
  #   calibr_ref <- calibr_ref %>%
  #     mutate(intensity = intensity * f(wavelength))  # how fiber will reduce intersity based on transmission
  # }
  
  
  
  # match wavelength values
  new_calibr_ref <- calibr_signal
  new_calibr_ref$type <- "reference"
  new_calibr_ref$intensity <- approx(x = calibr_ref$wavelength ,
                                     y = calibr_ref$intensity, 
                                     xout = calibr_signal$wavelength)$y
  
  dt <- bind_rows(new_calibr_ref, calibr_signal) 
  
  dt <- dt %>%
    group_by(type) %>%
    select(wavelength, intensity, type) %>%
    filter(between(wavelength, 300, 1050)) %>%
    mutate(intensity_norm = intensity / max(intensity, na.rm = T)) %>%
    ungroup()
  
  
  #Calculate correction
  dt_r <- dt %>%  filter(type == "reference") %>% select(wavelength, intensity_norm)
  dt_m <- dt %>%  filter(type == "measured") %>% select(wavelength, intensity_norm)
  
  #browser()
  
  correction_factor <- tibble(
    wavelength =  dt_m$wavelength, 
    intensity_norm = dt_r$intensity_norm  / dt_m$intensity_norm
  ) 
  
  correction_factor$type <- "correction_factor"
  
  bind_rows(dt, correction_factor) %>%
    filter(between(wavelength, 300, 1050)) 
})


output$calibration_plot <- renderPlot({
  
  
  dt <- compare_calibration_data()
  if(is.null(dt))
    return()
  
  if(input$calibration_see_corrected_on_plot){
    correction_from_calibration <- dt %>% 
      filter(type == "correction_factor") %>%
      select(intensity_norm) %>% unlist()
    
    dt <- dt %>%
      mutate(intensity_norm = ifelse(type == "measured", 
                                     intensity_norm * correction_from_calibration, 
                                     intensity_norm)) %>%
      mutate(type = stringr::str_replace_all(type, "measured", "measured and corrected"))
  }
  
  
  ggplot(dt, aes(x = wavelength, y = intensity_norm)) +
    stat_wb_column(data = dt %>% filter(type == "measured"), w.band = VIS_bands(), alpha = 0.4) +
    scale_fill_identity()+
    geom_line(aes(color = type)) +
    
    ggtitle("Normalized by the maximum value for both signals. Red curve shows factor, by which measured signal should be multiplied. ")
  #coord_cartesian(xlim = ranges_finalSpectrum_plot$x, 
  #                ylim = ranges_finalSpectrum_plot$y, expand = FALSE) +
  #xlab(paste0(names(finalSpectrum())[1], " (", ifelse(input$is_raman, "cm-1", "nm"), ")"))
  
  
})