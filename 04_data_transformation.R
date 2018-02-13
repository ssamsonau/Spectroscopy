

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