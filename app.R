# A Shiny app for demonstrating the letter following frequency algorithm I wrote.

library(shiny)
library(readtext)
#source("worddata.R")

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
      plotOutput("wordlengths_plot"),
      h4("Text Chosen:"),
      textOutput("text_choice")
    )
  )
)


server <- function(input, output){
  
###### Word Data Function ######
  word_data <- function(text){
    # break the text into characters
    rawcharacters <- strsplit(text, "")[[1]]
    # adding a space to the beginning and end of the vector. This is because the code later on 
    # detects the starting letter of a word by seeing what follows a space and also detects when
    # a space follows the character, meaning that letter is the end of the word.
    charvector <- rep(" ", length(rawcharacters)+2)
    charvector[2:(length(rawcharacters)+1)] <- rawcharacters
    # removing all non-space, non-letter characters.
    lettersandwhitespace_location <- which(is.element(charvector, letters) | is.element(charvector, LETTERS) | charvector == " ")
    lettersandwhitespace <- charvector[lettersandwhitespace_location]
    # detecting capital letters and converting them to lowercase.
    capitals_location <- which(is.element(lettersandwhitespace, LETTERS))
    if(length(capitals_location > 0)){
      capitals <- lettersandwhitespace[capitals_location]
      lowercase <- tolower(capitals)
      lettersandwhitespace[capitals_location] <- lowercase
    }
    # This is what the function returns. The entries "beginning" and "ending" contain vectors of all the
    # beginning and ending letters of the words from the text. Each entry named after a letter contains a vector of the
    # characters following that letter, with spaces for incidences where the letter terminates a word.
    following <- list()
    beginlocationsraw <- which(lettersandwhitespace == " ")+1
    endlocationsraw <- which(lettersandwhitespace == " ")-1
    beginlength <- length(beginlocationsraw)
    endlength<- length(endlocationsraw)
    # This chops off the last space since a word doesn't start after it.
    beginlocations <- beginlocationsraw[1:(beginlength-1)]
    # and this chops off the first space.
    endlocations <- endlocationsraw[2:(beginlength)]
    following[["beginning"]] <- lettersandwhitespace[beginlocations]
    following[["ending"]] <- lettersandwhitespace[endlocations]
    for(i in letters){
      following[[i]] <- lettersandwhitespace[1+which(lettersandwhitespace == i)]
    }
    # checking where spaces are lets us calculate word lengths.
    spacelocations <- beginlocationsraw - 1
    spaceloop <- length(spacelocations)-1
    wordlengths <- vector("numeric", spaceloop)
    for(i in 1:spaceloop){
      wordlengths[i] <- spacelocations[i+1] - spacelocations[i] - 1
    }
    following[["wordlengths"]] <- wordlengths
    return(following)
  }
###### End of Word Data Function ######
  
###### Importing the prepared texts ######  
  text_samples <- list(
    chance_for_peace = readtext("cfp.txt")$text,
    gettysburg_address = readtext("ga.txt")$text,
    declaration_of_independence = readtext("doi.txt")$text
  )
  
###### Main Observer ######
observe({
  
  internal_vars <- reactiveValues()
  internal_vars$text_selection <- character()
  internal_vars$word_lengths <- character()
  
  ###### Text output widget. ######
  if(input$text_select == "custom"){
    internal_vars$text_selection <- renderText(input$custom)
  }
  if(input$text_select != "custom"){
    internal_vars$text_selection <- renderText(text_samples[[input$text_select]])
  }
  output$text_choice <- internal_vars$text_selection
   
  internal_vars$word_lengths <- word_data(internal_vars$text_selection())$wordlengths
  
  output$wordlengths_plot <- renderPlot(hist(internal_vars$word_lengths))
  })
###### End of Main Observer ######
 
}



shinyApp(ui = ui, server = server)