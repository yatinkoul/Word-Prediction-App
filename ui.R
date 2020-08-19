library(shiny)

# Define UI for application that realizes single word prediction
shinyUI(fluidPage(
  
  # Application title
  titlePanel("WORD PREDICTION APP"),
    
    
  
  # Main panel with: input for enter text phrase, submit button, 
  # output for cleaned text phrase and output for single word prediction
  mainPanel(
    
    # Select box with a select input for each n-gram model
    selectInput("model", label = "Select n-gram model dataset:", 
                choices = list("News Data" = 2, "Twitter Data" = 1), 
                selected = 1),
    
    
    # Input box to enter text phrase without last word
    br(),  
    textAreaInput("textInput", label = "Enter word:", value = "", width = "100%", rows = 6),
    
       # Action button to submit entered text phrase
    actionButton("action", label = tags$b("Predict"), width = "25%"),
    br(),br(),
    tags$b("Predicted Next Word:"),
    
    # Output box to display predicted word
    fluidRow(column(4, verbatimTextOutput("predictedWord", placeholder = TRUE)))
    
  )
  
    ))
