# A Shiny app for demonstrating the letter following frequency algorithm I wrote.

library(shiny)
library(readtext)
library(ggplot2)
#source("worddata.R")

###### UI ######
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
                value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed turpis diam, pretium quis felis a, congue varius justo. Etiam sollicitudin ex et pretium varius."),
      
      selectInput(inputId = "graph_display",
                  label = "Select the output to display:",
                  choices = c("Lengths of words" = "wordlengths",
                              "Beginning letters" = "beginning",
                              "Ending letters" = "ending",
                              "a",
                              "b",
                              "c",
                              "d",
                              "e",
                              "f",
                              "g",
                              "h",
                              "i",
                              "j",
                              "k",
                              "l",
                              "m",
                              "n",
                              "o",
                              "p",
                              "q",
                              "r",
                              "s",
                              "t",
                              "u",
                              "v",
                              "w",
                              "x",
                              "y",
                              "z"
                              ),
                  selected = "beginning"
      )
    ),
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("Information",
                           h2("App Information"),
                           p("This app demonstrates a text processing algorithm I wrote. Having known for a long time
                             about the distribution of letters in English and how this could be used, I became
                             interested in the frequency at which letters followed other letters."),
                           p("The 'Text and Plots' tab lets the user select from built in texts or 
                             enter custom text. The plots it displays include the
                             frequency of beginning letters, the frequency of ending letters, the lengths
                             of words, and the letters that follow any given letter in words."),
                           p("The 'Randomly Generated Text' tab generates randomized text by sampling
                             from the report the algorithm generates. It uses the letter following frequencies
                             to cause the text to have a similar profile to the input text.
                             The user can control how much of this text 
                             to generate.")
                           ),
                  tabPanel("Text and Plots",
                           h2("Analysis Results:"),
                           plotOutput("word_plot"),
                           h4("Text Chosen:"),
                           textOutput("text_choice")
                  ),
                  tabPanel("Randomly Generated Text:",
                      sliderInput("random_text_chars",
                              label = "Characters of text to produce:",
                              min = 10,
                              max = 1000,
                              value = 100,
                              step = 1),
                      actionButton(inputId = "gen_text",
                               label = "Generate Text"),
                      textOutput("random_text"))
                  )
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
    # Removing the phantom words caused by multiple spaces.
    beginning <- lettersandwhitespace[beginlocations]
    ending <- lettersandwhitespace[endlocations]
    following[["beginning"]] <- beginning[which(beginning != " ")]
    following[["ending"]] <- ending[which(ending != " ")]
    for(i in letters){
      following[[i]] <- lettersandwhitespace[1+which(lettersandwhitespace == i)]
    }
    # checking where spaces are lets us calculate word lengths.
    spacelocations <- beginlocationsraw - 1
    spaceloop <- length(spacelocations)-1
    wordlengths <- vector("numeric", spaceloop)
    for(i in 1:spaceloop){
      temp_length <- spacelocations[i+1] - spacelocations[i] - 1
      wordlengths[i] <- spacelocations[i+1] - spacelocations[i] - 1
    }
    following[["wordlengths"]] <- wordlengths[which(wordlengths != 0)] # to remove zero length words caused by extra spaces
    return(following)
  }
  
###### A function to create chart outputs ######
  report_graph <- function(report){
    list_of_plots <- list()
    
    for(lett in letters){
      entry <- report[[lett]]
      entry <- entry[which(entry != " ")]
      if(length(entry)>0){
        list_of_plots[[lett]] <- ggplot(data.frame(entry = entry), aes(x = entry)) + geom_bar() +
                                        labs(title = paste0("Letters Following", " ", '"', lett, '"')) +
                                        xlab(paste0("Following letter"))
      }
    }
    list_of_plots[["beginning"]] <- ggplot(data.frame(beginning = report[["beginning"]]),
                                           aes(x = beginning)) + geom_bar() + 
                                           labs(title = "Beginning Letter Frequency")
    list_of_plots[["ending"]] <- ggplot(data.frame(ending = report[["ending"]]), 
                                        aes(x = ending)) + geom_bar() +
                                        labs(title = "Ending Letter Frequency")
    list_of_plots[["wordlengths"]] <- ggplot(data.frame(wordlengths = report[["wordlengths"]]), 
                                             aes(x = wordlengths)) + geom_bar() +
                                             labs(title = "Word Length Frequency")
    return(list_of_plots)
  }
  
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
  
  ###### The plot ###### 
  internal_vars$word_lengths <- word_data(internal_vars$text_selection())[["wordlengths"]]
  
  output$word_plot <- renderPlot(report_graph(word_data(internal_vars$text_selection()))[[input$graph_display]])
  })
  
###### Action button observer ######
observeEvent(input$gen_text, {
  output$random_text <- renderText(paste0("Example: ", sample(letters, 20)))
})
 
}

shinyApp(ui = ui, server = server)