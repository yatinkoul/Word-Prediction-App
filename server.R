
# Load required libraries
library(shiny)
library(tm)

# Function to custom clean enetered text phrase
custom_input_text_clean <- function (testline) 
{
    line <- iconv(testline, "latin1", "ASCII", sub = "")
    line <- gsub('[0-9]+', '', line)
    line <- tolower(line)
    line <- removeWords(line, stopwords())
    line <- removePunctuation(line)
    line <- gsub('\\S+[^\x20-\x7E]', '', line)
    emptyLines <- grepl('^\\s*$', line)
    line <- line[!emptyLines]
    line <- stripWhitespace(line)
    #line <- gsub("^ +| +$|( ) +", "\\1", line)
    return(line)
}

# Function to predict next word based on cleaned text phrase and model
predict_Backoff_custom_cleaning <- function (testline, modelsList, isDebugMode = F) 
{
  maxNGramIndex = length(modelsList)
  line <- custom_input_text_clean(testline)
  if (isDebugMode) 
    print(line)
  words <- unlist(strsplit(line, split = " "))
  len <- length(words)
  if (isDebugMode) 
    print(paste("Length of the string is: ", len))
  if (len < maxNGramIndex) {
    nGramIndex = len + 1
    localModelsList = modelsList[(maxNGramIndex - len):maxNGramIndex]
  }
  else {
    nGramIndex = maxNGramIndex
    localModelsList = modelsList
  }
  if (isDebugMode) 
    print(paste("Number of models will be used: ", length(localModelsList)))
  for (model in localModelsList) {
    pattern = paste0("^", paste(words[(len - nGramIndex + 
                                         2):len], collapse = " "))
    if (isDebugMode) 
      print(pattern)
    nextWords = model[grep(pattern, model$word)[1:5], 1]
    nextWords = nextWords[!is.na(nextWords)]
    if (length(nextWords) != 0) {
      nextWordIndex = sample(1:length(nextWords), 1)
      nextWord = nextWords[nextWordIndex]
    }
    else {
      nextWord = NA
    }
    if (isDebugMode) 
      print(nextWords)
    if (isDebugMode) 
      print(paste("Predicated word: ", nextWord))
    nGramIndex = nGramIndex - 1
    if (!is.na(nextWord)) {
      tempNextWord = unlist(strsplit(as.character(nextWord), 
                                     " "))
      if (isDebugMode) 
        print(paste("Splitted text: ", tempNextWord))
      nextWord = paste(tempNextWord[length(tempNextWord)])
      break
    }
  }
  if (is.na(nextWord)) {
    if (isDebugMode) 
      print(paste("No match found in ", paste(1:maxNGramIndex, 
                                              collapse = ","), "Gram models so returning the most frequent word"))
    nextWord = modelsList[[maxNGramIndex]][1, 1]
  }
  if (isDebugMode) 
    print(paste("The next predicated word using", nGramIndex + 
                  1, "gram model:-", nextWord))
  return(nextWord)
}


# Define server logic for application that realizes single word prediction
shinyServer(function(input, output, session) {
  
  # Loading required n-gram models
  isolate({
    withProgress(message = 'Loading required n-grams...',
      detail = 'This may take a several seconds...', value = 0, {
        unigramModelNews <- readRDS("./newsNGrams/unigramModelReduced.rds")
        incProgress(1/8)
        bigramModelNews <- readRDS("./newsNGrams/bigramModelReduced.rds")
        incProgress(1/8)
        trigramModelNews <- readRDS("./newsNGrams/trigramModelReduced.rds")
        incProgress(1/8)
        fourgramModelNews <- readRDS("./newsNGrams/fourgramModelReduced.rds")
        incProgress(1/8)
        unigramModelTwitter <- readRDS("./twitterNGrams/unigramModelReduced.rds")
        incProgress(1/8)
        bigramModelTwitter <- readRDS("./twitterNGrams/bigramModelReduced.rds")
        incProgress(1/8)
        trigramModelTwitter <- readRDS("./twitterNGrams/trigramModelReduced.rds")
        incProgress(1/8)
        fourgramModelTwitter <- readRDS("./twitterNGrams/fourgramModelReduced.rds")
        incProgress(1/8)
    })
  })
  
  # "Clean & Predict" and "Clear" action buttons
  v <- reactiveValues(data = NULL)
  
  # "Clean & Predict" action button
  observeEvent(input$action, {
    v$data <- runif(100)
    })
      
  # Clear entered text phrase and "Clear" action button
  observeEvent(input$reset, {
    v$data <- NULL
    updateTextAreaInput(session, "textInput", label = "Enter text phrase (without last word):", value = "")
    })  
    
  # Clean entered text phrase and clear cleaned text phrase
  output$cleanedText <- renderText({
    if (is.null(v$data)) return()
    custom_input_text_clean(input$textInput)
    })
    
  # Predict next word based on "News articles" text file or based on "Twitter data" text file 
  output$predictedWord <- renderText({
    testString <- custom_input_text_clean(input$textInput)
    choices <- list("News articles data" = 1, "Twitter data" = 2)
    if(input$model == choices[["News articles data"]]) {
      if (is.null(v$data)) return()
      nGramModelsListNews <- list(fourgramModelNews, trigramModelNews, bigramModelNews, unigramModelNews)
      set.seed(1234)
      predict_Backoff_custom_cleaning(testString, nGramModelsListNews, F)
    } else {
      if (is.null(v$data)) return()
      nGramModelsListTwitter <- list(fourgramModelTwitter, trigramModelTwitter, bigramModelTwitter, unigramModelTwitter)
      set.seed(1234)
      predict_Backoff_custom_cleaning(testString, nGramModelsListTwitter, F)
    }
    
  })

  
})
