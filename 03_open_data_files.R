observeEvent(input$file_backg, {
  
  #browser()
  tryCatch({
    values$backg_or <- readr::read_csv(input$file_backg$datapath) 
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))  
    values$backg_or <- NULL
  }
  )
  
  if(ncol(values$backg_or) != 2){
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))
    values$backg_or <- NULL
  }
  
  if(!is.null( values$backg_or)){
    names(values$backg_or) <- c("wavelength", "intensity")
    values$backg_or$type <- "background"  
  } 
  
})

observeEvent(input$file_ref, {
  #browser()
  tryCatch({
    values$ref_or <- readr::read_csv(input$file_ref$datapath)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))  
    values$ref_or <- NULL
  }
  )
  
  if(ncol(values$ref_or) != 2){
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))
    values$ref_or <- NULL
  }
  
  if(!is.null( values$ref_or)){
    names(values$ref_or) <- c("wavelength", "intensity")
    values$ref_or$type <- "reference" 
  }
  
})

observeEvent(input$file_sig, {
  #browser()
  tryCatch({
    values$sig_or <- readr::read_csv(input$file_sig$datapath)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))  
    values$sig_or <- NULL
  }
  )
  
  if(ncol(values$sig_or) != 2){
    showModal(modalDialog(
      title = "Error",
      paste("Something is wrong with a file:", e)
    ))
    values$sig_or <- NULL
  }
  
  if(!is.null( values$sig_or)){
    names(values$sig_or) <- c("wavelength", "intensity")
    values$sig_or$type <- "signal"
  }
  
})


raw_data <- reactive({
  if(all(sapply(c("backg_or", "ref_or", "sig_or"),
                function(x) is.null(values[[x]]))))
    return()
  
  bind_rows(values$backg_or, values$ref_or, values$sig_or)
})
