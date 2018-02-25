################### helping function
#####################################
ggplotRegression <- function (fit) {
  #https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                               y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
##################################################
##################################################


found_peaks <- reactive({
  if(input$thickness_calculation == F)
    return()
  # 
  browser()
  
  if(is.null(ranges_finalSpectrum_plot$x)){
    df <- finalSpectrum()
  }else{
    df <- finalSpectrum() %>% 
      filter(between(wavelength, ranges_finalSpectrum_plot$x[1],
                     ranges_finalSpectrum_plot$x[2]))
  }
    
  
  library(gridExtra)
  library(Peaks)
  library.dynam('Peaks', 'Peaks', lib.loc=NULL)   # see http://r.789695.n4.nabble.com/Problem-with-quot-Peaks-quot-package-followup-td4674949.html
  p <- SpectrumSearch(df$Intensity  %>% as.numeric, 
                      background = T, window = 5, 
                      sigma = 3, threshold = 10)
  
  peak_w <- df[p$pos, "wavelength"] %>% unlist
  peak_int <- df[p$pos, "Intensity"] %>% unlist
  
  
  plot_peaks <- tibble(
    peak_w = peak_w, 
    peak_int = peak_int
  )
  
  peak_plot <- ggplot(df) +
    geom_point(aes(wavelength, Intensity)) +
    geom_point(data = plot_peaks, aes(peak_w, peak_int), 
               color = "red", size = 2)
  
  
  df_new <- tibble(
    lambda = peak_w,
    inverse_wavelength = 1/ peak_w
  ) %>%
    arrange(-lambda) %>%
    mutate(peak_m = row_number())
  
  mod <- lm(inverse_wavelength ~ peak_m, data = df_new)
  
  regr_plot <- ggplotRegression(mod) +
    scale_y_continuous( sec.axis = sec_axis(~1/., name = "wavelength"))
  
  #summary(mod)
  
  slope <- mod$coefficients[2]
  slope_sigma <- summary(mod)$coefficients[2, 2]
  ind_refr <- 1.63
  d <- 1/(2 * ind_refr * slope)
  #d / 1000 # now in um
  
  #https://en.wikipedia.org/wiki/Propagation_of_uncertainty
  #f = aA
  # d = 1/(2 n slope)
  d_sigma = sqrt(1/(2 * ind_refr)) * slope_sigma 
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
  paste0("Thickness is: (", found_peaks()$d, 
         " +/- ", found_peaks()$d_sigma, ") nm\n")
})

output$formula <- renderPrint({
  print(paste("Fit using this formula:", 
              "$1/\\lambda = [\\frac{1}{4nd}+ \\frac{m0}{2nd} ] + m \\frac{1}{2nd}$"))
})