## 

library(shiny)
library(schoolmath)

# Define server logic required 
shinyServer(function(input, output) {
  
  output$x <- renderText({
    ## Builds the decimal number from the inputs parts
    x <- ifelse(input$int_part == "", 0, as.integer(input$int_part))
    m <- as.integer(nchar(input$rec_part))
    
    # Generate the decimal number from the text input
    recurrent_char <- ifelse(m > 0, paste0("(", input$rec_part, ")"), "")
    paste0("x = ", as.character(x), ".", input$dec_part, recurrent_char)
  })
  
  output$y <- renderText({
    # Find the equivalent fraction
    x <- ifelse(input$int_part == "", 0, as.integer(input$int_part))
    n <- as.integer(nchar(input$dec_part))
    y <- input$dec_part
    m <- as.integer(nchar(input$rec_part))
    z <- input$rec_part
    # Generate the decimal number from the text input
    x1 <- as.integer(paste0(as.character(x), y, z))
    x2 <- as.integer(paste0(as.character(x), y))
    num_char <- as.character(ifelse(m == 0, x2, x1 - x2))
    den_char <- as.character(ifelse(m == 0, 10^n, 10^n * (10^m - 1)))
    
    resp <- paste0("x = ", num_char, " / ", den_char)
    
    num <- as.numeric(num_char)
    den <- as.numeric(den_char)
    MCD <- suppressWarnings(gcd(num, den))
    
    resp2 <- ""
    if (MCD > 1)  resp2 <-  paste0(" = ",
      as.character(num / MCD), " / ", as.character(den / MCD))
    
    resp3 <- ""
    if (den / MCD == 1) resp3 <- paste0(" = ", as.character(num / MCD))  
    
    paste0(resp, resp2, resp3)
    
  })
})
