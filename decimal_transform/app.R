library(shiny)
library(schoolmath)

ui <- fluidPage(
  
  # Application title
  titlePanel("Convert decimal numbers into fractions"),
  p("All rational numbers can be represented by a decimal number, finite or repeating and the other way around.
    Given a decimal number a.b(c), where c represents the repeating part or repetend, it can be expressed as a fraction.
    Decimal numbers that have non zero repetend are called repeating or recurring decimals.
    If they do not have repetend, they are called terminating decimals"),
  p("This application computes the fraction expression of a given decimal number"),
  
  
  # Sidebar with textboxes to get number info
  sidebarLayout(
    
    sidebarPanel(
      h4(textOutput("x")),
      textInput("int_part", label = "Integer part (a)"),
      textInput("dec_part", label = "Decimal part (b)"),
      textInput("rec_part", label = "Recurrent part (c)") 
    ),
    mainPanel(
      h4("Equivalent fraction"),
      h4(textOutput("y"))
    )
  )
) 


# Define server logic required 
shinyServer <- function(input, output) {
  
  ### write here reactive functions
  ## Compute decimal number x from a, b, c
  
  output$x <- renderText({
    ## Builds the decimal number from the inputs parts
    x <- ifelse(input$int_part == "", 0, as.integer(input$int_part))
    m <- as.integer(nchar(input$rec_part))
    
    # Generate the decimal number from the text input
    recurrent_char <- ifelse(m > 0, paste0("(", input$rec_part, ")"), "")
    paste0("a.b(c) = ", as.character(x), ".", input$dec_part, recurrent_char)
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
}

shinyApp(ui = ui, server = shinyServer)
