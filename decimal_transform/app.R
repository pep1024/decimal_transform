library(shiny)
library(schoolmath)
source("global.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("Convert decimal numbers into fractions"),

  
  HTML(paste0("All rational numbers can be represented by a decimal number", tags$sup("[1]"),
    ", finite or repeating and the other way around.
    Given a decimal number a.b(c), where c represents the repeating part or repetend, it can be expressed as a fraction.
    Decimal numbers that have non zero repetend are called", strong(" repeating"), " or ", strong("recurring decimals."),
    " If they do not have repetend, they are called ", strong("terminating decimals"), ".")), 
  p("This application finds the fraction expression of a given decimal number.
    Build first the decimal number by introducing each part a, b and c.\n Then, click the button \'Compute fraction\'"),
  
  
  # Sidebar with textboxes to get number info
  sidebarLayout(
    
    sidebarPanel(
      h4(textOutput("x")),
      textInput("int_part", label = "Integer part (a)"),
      textInput("dec_part", label = "Decimal part (b)"),
      textInput("rec_part", label = "Recurrent part (c)") 
    ),
    mainPanel(
      actionButton("go", "Compute fraction"),
      h4("Equivalent fraction"),
      h4(textOutput("y"))
    )
  ),
  tags$sup("[1]"),
  " ",
  a(" https://en.wikipedia.org/wiki/Repeating_decimal", 
    href = "[https://en.wikipedia.org/wiki/Repeating_decimal"),
  tags$div(
    'align' = "left", "Pep Porrà, 2017-10-12")
) 


# Define server logic required 
shinyServer <- function(input, output) {
  
  ### write here reactive functions
  ## Compute decimal number x from a, b, c
  create_num <- reactive({
    x <- as.character(
      ifelse(input$int_part == "", 0, as.integer(input$int_part)))
    m <- as.integer(nchar(input$rec_part))
    y <- input$dec_part
    n <- as.integer(nchar(input$dec_part))
    z <- input$rec_part
    
    if (m == 0 && n == 0) {
      resp <- x
    } else {
      resp <- paste0(x, ".", y, 
        ifelse(m > 0, paste0("(", input$rec_part, ")"), "")) 
    }
    
    # Generate the decimal number from the text input
    num <- c(resp, x, y, z)
    names(num) <- c("decimal", "a", "b", "c")
    num
  })
  
  create_fraction <- eventReactive(input$go,{
    dec <- create_num()
    # Generate the decimal number from the text input
    x1 <- as.integer(paste(dec[-1], collapse = ""))
    x2 <- as.integer(paste(dec[2:3], collapse = ""))
    n <- nchar(dec[3]) 
    m <- nchar(dec[4])
    #x1 <- as.integer(paste0(as.character(x), y, z))
    #x2 <- as.integer(paste0(as.character(x), y))
    num_char <- as.character(ifelse(m == 0, x2, x1 - x2))
    den_char <- as.character(ifelse(m == 0, 10^n, 10^n * (10^m - 1)))
    
    resp <- paste0(dec[1], " = ", num_char, " / ", den_char)
    
    num <- as.numeric(num_char)
    den <- as.numeric(den_char)
    MCD <- gcd_pep(num, den)
    
    resp2 <- ""
    if (MCD > 1)  resp2 <-  paste0(" = ",
      as.character(num / MCD), " / ", as.character(den / MCD))
    
    resp3 <- ""
    if (den / MCD == 1) resp3 <- paste0(" = ", as.character(num / MCD))  
    
    ifelse(dec[1] == "0", "", paste0(resp, resp2, resp3))
  })
  
  
  output$x <- renderText({
    ## Builds the decimal number from the inputs parts
    paste0("a.b(c) = ", create_num()[1])
  })
  
  output$y <- renderText({
    ## Find the equivalent fraction
    create_fraction()
  })
}

shinyApp(ui = ui, server = shinyServer)
