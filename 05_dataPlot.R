
output$dataPlot <- renderPlot({
  
  if(is.null(combined_data()))
    return()
  
  ggplot(combined_data(), aes(x = wavelength, y = intensity, color = type)) +
    stat_wb_column(data = combined_data() %>% 
                     filter(type == "signal"), w.band = VIS_bands(), alpha = 0.4) +
    scale_fill_identity()+
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
  
  
  if(is.null(ranges_dataPlot$x))
    return(NULL)
  
  #browser()
  x_name <- rlang::sym(names(combined_data())[1])
  y_name <- rlang::sym(names(combined_data())[2])
  
  new_y_lim <- combined_data() %>%
    filter(between(!!x_name, ranges_dataPlot$x[1], ranges_dataPlot$x[2])) %>%
    summarise(max = max(!!y_name), min = min(!!y_name)) %>%
    unlist
  
  ranges_dataPlot$y <- new_y_lim
})

