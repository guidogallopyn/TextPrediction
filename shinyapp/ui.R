# Shiny application for Coursera Data Specialization Capstone Project
# Guido Gallopyn
#

library(shiny) 
require(markdown)


shinyUI( fluidPage(
 
  titlePanel("Capstone Text Predictor"),

  fluidRow(
      column(2, wellPanel(
        radioButtons("radio", label = h4("Predictor Model"),
                     choices = list("Small" = "small", "Medium" = "medium", "Large" = "large"),
                     selected = "small")
      )),
      
      column (8,
          tabsetPanel(id="tabid",
             
              tabPanel("Evaluate",
                       br(),
                       tags$b("Hint:"), "Enter a phrase in the Input field, push the Predict button and observe the top 5 predicted following words.", 
                       br(),br(),
                       uiOutput("eval"), 
                       actionButton("predict", label = "Predict"),
                       actionButton("clear", label = "Clear"),
                       hr(),
                       h5("Top 5 word predictions"),
                       tableOutput("result"),
                       hr(),
                       tags$b("Hint:"),
                       "Choose a different predictor model in the side panel to reevaluate the prediction with a different model.",
                       hr(),
                       tags$b("Note:"),
                       "An auto-completion demo is available in the App tab."
              ),
              
              tabPanel("App", 
                       h4("Auto-complete demo"),
                       br(),
                       fluidRow(column(12, align="center",
                          uiOutput("edit"),
                          uiOutput("nbest"),
                          br(),
                          fluidRow( column(12, align="center",
                                        actionButton("key.q", label = "q"),  # keyboard
                                        actionButton("key.w", label = "w"),
                                        actionButton("key.e", label = "e"),
                                        actionButton("key.r", label = "r"),
                                        actionButton("key.t", label = "t"),
                                        actionButton("key.y", label = "y"),
                                        actionButton("key.u", label = "u"),
                                        actionButton("key.i", label = "i"),
                                        actionButton("key.o", label = "o"),
                                        actionButton("key.p", label = "p"))),
                          fluidRow( column(12, align="center",
                                        actionButton("key.a", label = "a"), 
                                        actionButton("key.s", label = "s"),
                                        actionButton("key.d", label = "d"),
                                        actionButton("key.f", label = "f"),
                                        actionButton("key.g", label = "g"),
                                        actionButton("key.h", label = "h"),
                                        actionButton("key.j", label = "j"),
                                        actionButton("key.k", label = "k"),
                                        actionButton("key.l", label = "l"))),
                          fluidRow( column(12, align="center",
                                        actionButton("key.z", label = "z"), 
                                        actionButton("key.x", label = "x"),
                                        actionButton("key.c", label = "c"),
                                        actionButton("key.v", label = "v"),
                                        actionButton("key.b", label = "b"),
                                        actionButton("key.n", label = "n"),
                                        actionButton("key.m", label = "m"),
                                        actionButton("key.BACK", label = "BACK"))),
                          fluidRow( column(12, align="center",
                                        actionButton("key.SPACE", label = "space"),
                                        actionButton("key.RETURN", label = "return"))))),

                       hr(),
                       tags$b("Note:"),
                       "type in the App window and observe the top 5 predicted words appear as you type.", 
                       "Press on the correct word to auto-complete or predict.",
                       br(),
                       tags$b("Hint:"),
                       "Choose a different predictor model in the side panel to reevaluate the prediction with a different language model.",
                       hr(),
                       helpText("See About tab for for more technical information")
              ),
              
              tabPanel("Under the hood",
                       h4("Language model summary"),  
                       tableOutput('table1'),
                       tableOutput('table2'),
                       # add perplexity and size table
                       # add accuracy graph
                       hr(),
                       tags$b("Hint:"),
                       "Choose a different predictor model in the side panel to see statistics on other models."
              ),
              
              tabPanel("About", 
                    includeMarkdown("about.md")
              )
          )    
       )
  )  

))
