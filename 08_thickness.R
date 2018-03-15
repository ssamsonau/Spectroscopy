################### helping function
#####################################
ggplotRegression <- function (fit) {
  #https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                               y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ), "\n",
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)))
}
##################################################
##################################################


found_peaks <- reactive({
  if(input$thickness_calculation == F)
    return()
  # 
  #browser()
  
  if(is.null(ranges_finalSpectrum_plot$x)){
    df <- finalSpectrum()
  }else{
    df <- finalSpectrum() %>% 
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
  
  peak_plot <- ggplot(df) +
    geom_point(aes(wavelength, Intensity)) 
  
  peak_w <- df[p$pos, "wavelength"] %>% unlist
  peak_int <- df[p$pos, "Intensity"] %>% unlist
  
  
  plot_peaks <- tibble(
    peak_w = peak_w, 
    peak_int = peak_int
  )
  
  if(input$use_manually_chosen_peak_w){
    points <- str_split(input$manual_thickness_points, 
                        pattern = ",", simplify = T)  %>% 
      as.numeric()
    
    plot_peaks <- tibble(
      peak_w = points, 
      peak_int = rep(1, length(points))
    ) 
    
    #browser()
    peak_plot <- peak_plot + 
      geom_vline(xintercept = plot_peaks$peak_w,  
                 color = "red", size = 1, linetype = 2)
  }
  
  
  if(!input$use_manually_chosen_peak_w){
    peak_plot <- peak_plot + 
      geom_point(data = plot_peaks, aes(peak_w, peak_int), 
                 color = "red", size = 2)
    
  }
    
  
  df_new <- tibble(
    lambda = plot_peaks$peak_w,
    inverse_wavelength = 1/ plot_peaks$peak_w
  ) %>%
    arrange(-lambda) %>%
    mutate(peak_m = row_number())
  
  #browser()
  
  mod <- lm(inverse_wavelength ~ peak_m, data = df_new)
  
  regr_plot <- ggplotRegression(mod) +
    scale_y_continuous( sec.axis = sec_axis(~1/., name = "wavelength")) +
    xlab("m")
  
  #summary(mod)
  
  slope <- mod$coefficients[2]
  slope_sigma <- summary(mod)$coefficients[2, 2]
  ind_refr <- input$index_of_refraction # 1.63
  d <- 1/(2 * ind_refr * slope)
  #d / 1000 # now in um
  
  #https://en.wikipedia.org/wiki/Propagation_of_uncertainty
  #f = aA
  # d = 1/(2 n slope)
  d_sigma =  slope_sigma / (2 * ind_refr * slope^2)
  #d_sigma
  
  #browser()
  thickness_plot <- grid.arrange(peak_plot, regr_plot, nrow = 1)
  
  list(d = d, d_sigma = d_sigma, 
       mod = mod, thickness_plot = thickness_plot)
})

output$thickness_plot <- renderPlot({
  print(found_peaks()$thickness_plot)
})

output$thickness_text <- renderText({
  paste0("Thickness is: (", round(found_peaks()$d), 
         " +/- ", round(found_peaks()$d_sigma), ") nm\n")
})



values$manually_chosen_peak_w <- ""

observeEvent(input$finalSpectrum_plot_click, {
  #browser()
  if(values$manually_chosen_peak_w == ""){
    values$manually_chosen_peak_w <- 
      round(input$finalSpectrum_plot_click$x, digits = 2)
  }else{
    values$manually_chosen_peak_w <- 
      paste0(values$manually_chosen_peak_w, ", ",
             round(input$finalSpectrum_plot_click$x, digits = 2))
  }
  
  updateTextInput(session, inputId = "manual_thickness_points", 
                  value = values$manually_chosen_peak_w)
  
})

observeEvent(input$manual_thickness_points, {
  #browser()
  values$manually_chosen_peak_w <- 
    input$manual_thickness_points
})



