
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory
  },
  handlerExpr = {
    if (input$directory > 0) {
      # condition prevents handler execution on initial app launch
      
      # launch the directory selection dialog with initial path read from the widget
      path = choose.dir(default = readDirectoryInput(session, 'directory'))
      
      # update the widget value
      updateDirectoryInput(session, 'directory', value = path)
    }
  }
)


thickness_res_df <- eventReactive(input$thickness_mapping_calculation_but, {
  
  ############### function
  find_thickness <- function(df_current){
    
    if(is.null(ranges_finalSpectrum_plot$x)){
      df <- df_current
    }else{
      df <- df_current %>% 
        filter(between(wavelength, ranges_finalSpectrum_plot$x[1],
                       ranges_finalSpectrum_plot$x[2]))
    }
    
    #browser()
    library(gridExtra)
    library(Peaks)
    library.dynam('Peaks', 'Peaks', lib.loc=NULL)   # see http://r.789695.n4.nabble.com/Problem-with-quot-Peaks-quot-package-followup-td4674949.html
    p <- SpectrumSearch(df$Intensity  %>% as.numeric, 
                        background = T,  
                        #sigma = input$t_sigma, 
                        threshold = input$t_threshold)
    
    peak_w <- df[p$pos, "wavelength"] %>% unlist
    peak_int <- df[p$pos, "Intensity"] %>% unlist
    
    if(input$use_manually_chosen_peak_w){
      points <- str_split(input$manual_thickness_points, 
                          pattern = ",", simplify = T)  %>% 
        as.numeric()
      
      plot_peaks <- tibble(
        peak_w = points, 
        peak_int = rep(1, length(points))
      ) 
    }
    
    if(!input$use_manually_chosen_peak_w){
      
      plot_peaks <- tibble(
        peak_w = peak_w, 
        peak_int = peak_int
      )
      
    }
    
    
    df_new <- tibble(
      lambda = plot_peaks$peak_w,
      inverse_wavelength = 1/ plot_peaks$peak_w
    ) %>%
      arrange(-lambda) %>%
      mutate(peak_m = row_number())
    
    #browser()
    
    mod <- lm(inverse_wavelength ~ peak_m, data = df_new)
    
    slope <- mod$coefficients[2]
    slope_sigma <- summary(mod)$coefficients[2, 2]
    ind_refr <- input$index_of_refraction # 1.63
    d <- 1/(2 * ind_refr * slope)
    
    #https://en.wikipedia.org/wiki/Propagation_of_uncertainty
    #f = aA
    # d = 1/(2 n slope)
    d_sigma =  slope_sigma / (2 * ind_refr * slope^2)
    #d_sigma
    
    c(d, d_sigma)
  }
  
  
  ################# action
  #browser()
  
  ###### read file
  dir_with_th_files <- input$directory__chosen_dir
  p <- list.files(dir_with_th_files)
  
  thickness_res_df <- tibble(
    file = character(0),
    d = numeric(0)
  )
  
  for(current_file in p){
    
    full_path <- paste0(dir_with_th_files, "\\", current_file)
    ##############read
    #browser()
    file_thickness <- NULL
    tryCatch({
      file_thickness <- readr::read_csv(full_path) 
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Something is wrong with a file:", e)
      ))  
    }
    )
    
    if(ncol(file_thickness) != 2){
      showModal(modalDialog(
        title = "Error",
        paste("Something is wrong with a file:", e)
      ))
    }
    
    if(!is.null(file_thickness)){
      names(file_thickness) <- c("wavelength", "Intensity")
    } 
    
    dd <- find_thickness(file_thickness)
    
    #browser()
    
    thickness_res_df <- thickness_res_df %>%
      bind_rows(
        tibble(
          file = current_file,
          d = dd[1],
          d_sigma = dd[2]
        )
      )
  }
  
  thickness_res_df
  
})

output$thickness_dt <- DT::renderDataTable({
  ignoreNULL = TRUE  
  #browser()
  thickness_res_df()
})

output$thickness_map <- renderPlotly({
  ignoreNULL = TRUE  
  #browser()
  df <- thickness_res_df()
  
  positions <- str_extract_all(df$file, "[0-9\\-,]", simplify = F) 
  
  coordinates <- lapply(positions, function(x){
   str_c(x, collapse = "") %>%
      str_replace_all("-", ".") %>%
      str_split(",", simplify = T) %>%
      as.numeric()
  })
  
  x <- sapply(coordinates, function(x) x[1]) 
  y <- sapply(coordinates, function(x) x[2])
    
  df %>%
    mutate(x = x, 
           y = y)
  
  #browser()
  
  if(input$plot_2D_3D == "2D"){
    ggplot(df, aes(x, y)) +
      geom_raster(aes(fill = d)) +
      theme(aspect.ratio = 1)

    ggplotly()
  }else{
    plot_ly(df, x = ~x, y = ~y, z = ~d, color = ~d, colors = c('#BF382A', '#0C4B8E')) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'x'),
                          yaxis = list(title = 'y'),
                          zaxis = list(title = 'Thickness (nm)', 
                                       range = c(0, max(df$d)))
               ) 
      )
  }
  
  
})

output$download_thickness_dt <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste0("Data_", Sys.time(), ".csv")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {# Write to a file specified by the 'file' argument
    write.table(thickness_res_df(), file, sep = ",",
                row.names = FALSE)
  }
)
