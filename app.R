# A Shiny app for demonstrating the letter following frequency algorithm I wrote.

library(shiny)

ui <- fluidPage(
  titlePanel("Letter Following Frequency Demo"),
  sidebarLayout(
    sidebarPanel(
      h3("Text Selector:"),
      radioButtons(inputId = "text_select",
                   label = "Select the text to analyze:",
                   choiceNames = c("Chance For Peace, Dwight D. Eisenhower, 1953",
                                   "The Gettysburg Address, Abraham Lincoln, 1863",
                                   "The Declaration of Independence, 1776"
                                   ),
                   choiceValues = c("chance_for_peace", "gettysburg_address", "declaration_of_independence")
                   )
    ),
    mainPanel(
      h3("Analysis Results:"),
      p("Text Chosen:"),
      textOutput("text_choice")
    )
  )
)


server <- function(input, output){
  ####### Code to load the blocks of text based on what is selected in the UI. ######
  
  # Code to load the text from files will go here.
  
  # This list currently contains dummy text but will later have what is loaded from above
  text_samples <- list(
    chance_for_peace = "text of the cross of iron speech",
    gettysburg_address = "text of the gettysburg address",
    declaration_of_independence = "text from the declaration"
  )

    ###### Text output widget. ######
  output$text_choice <- renderText({
    text_samples[[input$text_select]]
  }) 
  
}

shinyApp(ui = ui, server = server)