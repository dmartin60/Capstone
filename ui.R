#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressMessages(library(shiny))          # To create a shiny application.

# Define UI for Next word project
shinyUI(fluidPage(
  
  # Application title
    titlePanel("What is the Next Word?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("many",
                  "Number of Predicted Words:",
                  min   = 1,
                  max   = 10,
                  value = 1,
                  step  = 1 ),
      
      radioButtons('case', 
                   "Case of predicted words", 
                    c('Lower case'  = "L",
                      'Proper case' = "P",
                      'Upper case'  = "U")),
      
      radioButtons('ord',  
                   "Order of predicted words", 
                   c('Most likely'  = "M", 
                     'Alphabetical' = "A"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
               h3("Enter phrase below:"), 
               textInput("phrase", "",NULL),
      
               h4("Next word prediction"), 
               div(tableOutput("myTable"), style = "font-size:110%;"),
      
               tags$head(tags$style(type="text/css", "#myTable table {width:50%;}")) 
    )
  )
))
