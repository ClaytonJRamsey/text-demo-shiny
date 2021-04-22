library(ggplot2)

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

tx_report_graph <- function(report){
  list_of_plots <- list()
  
  for(lett in letters){
    entry <- report[[lett]]
    entry <- entry[which(entry != " ")]
    if(length(entry)>0){
      list_of_plots[[lett]] <- ggplot(data.frame(entry = entry), aes(x = entry)) + geom_bar()
      }
  }
  list_of_plots[["beginning"]] <- ggplot(data.frame(beginning = report[["beginning"]]), aes(x = beginning)) + geom_bar()
  list_of_plots[["ending"]] <- ggplot(data.frame(ending = report[["ending"]]), aes(x = ending)) + geom_bar()
  list_of_plots[["wordlengths"]] <- hist(report$wordlengths, main = "Histogram of Word Lengths", xlab = "Word Length")
  
  return(list_of_plots)
}