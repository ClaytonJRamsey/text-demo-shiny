library(ggplot2)

sample_text_2 <- "the  spaces  are  all  doubled"
sample_text <- 
  "5. The enforcement of all these agreed limitations and prohibitions by adequate safeguards, including a practical system of inspection under the United Nations.
The details of such disarmament programs are manifestly critical and  complex. Neither the United States nor any other nation can properly claim to possess a perfect, immutable formula. But the formula matters less than the faith—the good faith without which no formula can work justly and effectively.
The fruit  of success in all these tasks would present the world with the greatest task, and the greatest opportunity, of all. It is this: the dedication of the energies, the resources, and the imaginations of all peaceful nations to a new kind of war. This would be a declared total war, not upon any human enemy but upon the brute forces of poverty and need.
The peace we seek, rounded upon decent trust and cooperative effort  among nations, can be fortified, not by weapons of war but by wheat and by cotton, by milk and by wool, by meat and by timber and by rice. These are words that translate into every language on earth. These are needs that challenge this world in arms.
This idea of a just and peaceful world is not new or strange to us. It inspired the people of the United States to initiate the European Recovery Program in 1947. That program was prepared to treat, with like and equal concern, the needs of Eastern and Western Europe.
We are prepared  to reaffirm, with the most concrete evidence, our readiness to help build a world in which all peoples can be productive and prosperous.
This Government is ready to ask its people to join with all nations in devoting a substantial percentage of the savings achieved by disarmament to a fund for world aid and reconstruction. The purposes of this great work would be to help other peoples to develop the undeveloped areas of the world, to stimulate profitable and fair world trade, to assist all peoples to know the blessings of productive freedom.
The monuments to this new kind of war would  be these: roads and schools, hospitals and homes, food and health.
We are ready, in short, to dedicate our strength to serving the needs, rather than the fears, of the world.
We  are ready, by these and all such  actions, to make of the United Nations an institution that can effectively guard the peace and security of all peoples.
I know of nothing I can add to make plainer the sincere purpose of the United States.
I know of no course, other than that marked by these and similar actions, that can be called the highway of peace.
I know of only one question upon which progress waits. It is this:
What is the Soviet Union ready to do?
Whatever the answer be, let it be plainly spoken.
Again we say: the  hunger for peace is too great, the hour in history too late, for any government to mock men's hopes with mere words and promises and gestures.
The test of truth is simple. There can be no persuasion but by deeds.
Is the new leadership of the Soviet Union prepared to use its decisive influence in the Communist world, including control of the flow of arms, to bring not merely an expedient truce in Korea but genuine peace in Asia?
Is it prepared to allow other nations, including those of Eastern Europe, the free choice of their own forms of government?
Is it prepared to act in concert with others upon serious disarmament proposals to be made firmly effective by stringent U.N. control and inspection?
If not, where then is the concrete evidence of the  Soviet Union's concern for peace?
The test is clear.
There is, before all peoples, a precious chance to turn the black tide of events. If we failed to strive to seize this chance, the judgment of future ages would be harsh and just.
If we strive but fail and the world remains armed against itself, it at least need be divided no longer in its clear knowledge of who has condemned humankind to this fate.
The purpose of the United States, in stating these proposals, is simple and clear.
These proposals spring, without ulterior purpose or political passion, from our calm c"

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
