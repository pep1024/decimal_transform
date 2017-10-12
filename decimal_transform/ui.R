

library(shiny)

# Define UI 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Decimal (repeating) numbers into fractions"),
  p("All rational numbers can be represented by a decimal number, finite or repeating and the other way around.
    Given a decimal number a.b(c), where c represents the repeating part, the equivalent fraction is computed."),
  
  # Sidebar with textboxes to get number info
  sidebarLayout(
    
    sidebarPanel(
      textInput("int_part", label = "Integer part (a)"),
      textInput("dec_part", label = "Decimal part (b)"),
      textInput("rec_part", label = "Recurrent part (c)") 
    ),
    mainPanel(
      h4("Number"),
      textOutput("x"),
      h4("Equivalent fraction"),
      textOutput("y")
    )
  )
))
