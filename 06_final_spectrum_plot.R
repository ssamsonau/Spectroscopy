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
  if(is.null(ranges_finalSpectrum_plot$x))
    return(NULL)
  
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
      new_y_scale <- dt %>% 
        summarise(min_y = min(!!rlang::sym(names(dt[2]))), 
                  max_y = max(!!rlang::sym(names(dt[2])))) %>%
        unlist
      
      comp_y <- rlang::sym(names(dt[2]))
      
      old_y_scale <- compare_signal %>%
        summarise(min_y = min(!!comp_y), max_y = max(!!comp_y)) %>%
        unlist
      
      compare_signal <- compare_signal %>%
        mutate(rlang::UQ(comp_y) := rlang::UQ(comp_y) - old_y_scale[1]) %>%
        mutate(rlang::UQ(comp_y) := rlang::UQ(comp_y)/diff(old_y_scale) * diff(new_y_scale)) %>%
        mutate(rlang::UQ(comp_y) := rlang::UQ(comp_y) + new_y_scale[1])
      
      #browser()
      dt <- bind_rows(dt, compare_signal)
    }
    
  })
  
  dt
})

output$spline_n_ui <- renderUI({
  dt <- finalSpectrum()
  if(is.null(dt))
    return(NULL)
  dt_for_fitting <- dt %>% filter(type == "Final Spectrum")
  
  numericInput("spline_n", "Number of dots for spline (default 3 * number of points)",
               value = 3 * nrow(dt_for_fitting), width = "100%")
})


output$finalSpectrum_plot <- renderPlot({
  
  dt <- finalSpectrum()
  if(is.null(dt))
    return(NULL)
  dt_for_fitting <- dt %>% filter(type == "Final Spectrum")
  
  p <- ggplot(dt, aes_string(x = names(dt)[1], y = names(dt)[2], color = "type"))
    
  if(!input$is_raman){
    p <- p + 
      stat_wb_column(data = dt %>% 
                       filter(type == "Final Spectrum"), w.band = VIS_bands(), alpha = 0.4) +
      scale_fill_identity()
  }
  
  if(!input$hide_dots){
    p <- p + geom_point()
  }
  
  p <- p +
    geom_line(data=data.frame(spline(dt_for_fitting, n = input$spline_n), 
                              "type" = rep("Spline fit", input$spline_n)), 
              aes(x = x, y = y, color = type), size = 1.2) +
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

output$color_plot <- renderPlot({
  #browser()
  library(colorscience)
  
  chromaticity.diagram.color(conversionFunction=CIE1931XYZ2CIE1976uv, 
                             xlim=c(0, 0.7),
                             ylim=c(0, 0.65),
                             xlab="u'",ylab="v'")
  library(png)
  im <- png::readPNG("spectral_image/Spectral Image made in Wolfram Mathematica.png")
  
  rasterImage(im, xleft = -0.014, xright = 0.714, ybottom = -0.015, ytop = 0.664)
  
  #browser()
  df <- finalSpectrum() %>%
    filter(type == "Final Spectrum") %>%
    select(wavelength, Intensity) %>%
    filter(between(wavelength, 360, 740)) %>%
    na.omit() %>%
    mutate(wavelength = round(wavelength, digits = 0)) %>%
    group_by(wavelength) %>%
    summarise(Intensity = mean(Intensity)) %>%
    ungroup() %>%
    mutate(Intensity = Intensity/max(Intensity)) %>%
    as.matrix()
  
  #XYZ <- spectra2XYZ(MaterialReferenceData[,c('wavelength','BlueSky')])
  XYZ <- spectra2XYZ(df)
  uv <- CIE1931XYZ2CIE1976uv(XYZ)
  
  values$final_color <- rgb(XYZ2RGB(XYZ/sum(XYZ)))
  points(x = uv[1], y = uv[2], col = rgb(XYZ2RGB(XYZ/sum(XYZ))), pch = 19, lw = 9) # rescale to have full intensity of color. Otherwise it may be just dark
  
})

output$color_text <- renderText({
  values$final_color
})

