output$color_plot <- renderPlot({
  #browser()
  w_range <- range(finalSpectrum()$wavelength)
  if(w_range[1] > 400 | w_range[2] < 700){
    values$final_color <- "NOT ENOUGTH DATA: spectrum should inlclude at least range 400-700 nm"
    showModal(modalDialog(
      title = "Error",
      paste("NOT ENOUGTH DATA: spectrum should inlclude at least range 400-700 nm")
    ))
    return()
  }
  
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
    filter(between(wavelength, 360, 760)) %>%
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