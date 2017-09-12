#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressMessages(library(shiny))          # To create a shiny application.

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  x <- reactive({
    
    x <- tolower(input$phrase)
    y <- unlist(strsplit(as.character(x), ""))
    
    validate(need(y[length(y)] != 0, "Waiting for your phrase..."))
    
    first  <- uniGramDf
    second <- biGramDf
    third  <- triGramDf
    
    Prediction <<- predict(input$phrase, first, second, third)
    Prediction <<- Prediction[!duplicated(Prediction)]
    Prediction <<- as.data.table(Prediction)
    #Prediction
  
  })
  
  x1 <- reactive({
    Prediction <<- x()$Prediction
    Prediction <<- Prediction[1:input$many]
    
    if(input$ord  == "A"){Prediction <<- sort(Prediction)}
    if(input$case == "U"){Prediction <<- toupper(Prediction)}
    if(input$case == "P"){Prediction <<- capwords(Prediction)}
    
    Prediction <<- as.data.table(Prediction)
    #Prediction
  })
  
  output$myTable <- renderTable({x1()})
  
})
