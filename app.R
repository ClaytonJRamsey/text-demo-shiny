# A Shiny app for demonstrating the letter following frequency algorithm I wrote.

library(shiny)
library(readtext)

ui <- fluidPage(
  titlePanel("Letter Following Frequency Demo"),
  sidebarLayout(
    sidebarPanel(
      h3("Text Selector:"),
      radioButtons(inputId = "text_select",
                   label = "Select the text to analyze:",
                   choiceNames = c("Chance For Peace, Dwight D. Eisenhower, 1953",
                                   "The Gettysburg Address, Abraham Lincoln, 1863",
                                   "The Declaration of Independence, 1776",
                                   "Custom"
                                   ),
                   choiceValues = c("chance_for_peace", 
                                    "gettysburg_address", 
                                    "declaration_of_independence",
                                    "custom")
                   ),
      textInput(inputId = "custom",
                label = "Enter the custom text here:",
                value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed turpis diam, pretium quis felis a, congue varius justo. Etiam sollicitudin ex et pretium varius.")
    ),
    mainPanel(
      h2("Analysis Results:"),
      h4("Text Chosen:"),
      textOutput("text_choice")
    )
  )
)


server <- function(input, output){
  
  text_samples <- list(
    chance_for_peace = readtext("cfp.txt")$text,
    gettysburg_address = readtext("ga.txt")$text,
    declaration_of_independence = readtext("doi.txt")$text
  )
  
###### Main Observer ######
observe({

  ###### Text output widget. ######
  if(input$text_select == "custom"){
    output$text_choice <- renderText(input$custom)
  }
  if(input$text_select != "custom"){
    output$text_choice <- renderText(
      text_samples[[input$text_select]]
    )
  }
  
    
})
 
}

shinyApp(ui = ui, server = server)