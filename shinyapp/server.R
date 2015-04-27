# Shiny application for Coursera Data Specialization Capstone Project
# Guido Gallopyn
# April 26, 2015
#
# deployed at https://senseiguido.shinyapps.io/TextPrediction/

library(shiny)
library(xtable)
source("NGramLM.runtime.R")

# function used to predict next word
lastchar <- function(text) substr(text,nchar(text),nchar(text))

getprefix <- function(text) {
  if(length(text) == 0) return('')
  if(lastchar(text) == " " ) return('')
  nglast(text) 
}

edittext <- function(text, word, key) {
  if(length(text)==0) { if(length(word)) return(paste0(word,' ')) else return(toupper(key)) }
  if(length(key)) return(paste0(text,switch(key, SPACE = ' ', RET = '\n', key)))
  if(lastchar(text)==" ") return(paste0(text,' ',word,' ')) 
  paste0(ngprefix(text),' ',word,' ')
}

Predict <- function(mod, text, prefix="") {
  if(length(text)==0) return(numeric(0))
  names(predict(mod, text, Nbest=5, prefix=prefix))
}

models <- c(small  = modelfilename("large",N=2,K=40),
            medium = modelfilename("large",N=3,K=20),
            large  = modelfilename("large",N=3,K=7))

step <- 0

shinyServer( 
  function(input, output) {

    # side panel: model selection implemented with model cache (load once)
    cache <- reactiveValues(small= NULL, medium=NULL, large=NULL)
    
    getLM <- reactive({
      if(is.null(cache[[input$radio]])) { 
        load(file=file.path("models", models[input$radio]))
        cache[[input$radio]] <- mod
      }
      return(cache[[input$radio]])
    })
    
    # App panel
    b <- reactiveValues(word= NULL, nbest=character(0), key=NULL)
    
    observeEvent( input$nbest1, { b$word <- b$nbest[1] ; b$key<- NULL } )  # dualing buttons
    observeEvent( input$nbest2, { b$word <- b$nbest[2] ; b$key<- NULL } )
    observeEvent( input$nbest3, { b$word <- b$nbest[3] ; b$key<- NULL } )
    observeEvent( input$nbest4, { b$word <- b$nbest[4] ; b$key<- NULL } )
    observeEvent( input$nbest5, { b$word <- b$nbest[5] ; b$key<- NULL } )
    
    observeEvent( input$key.a, { b$key <- 'a' ; b$word <- NULL } )  # keyboard, haven't found how to do this shorter 
    observeEvent( input$key.b, { b$key <- 'b' ; b$word <- NULL } ) 
    observeEvent( input$key.c, { b$key <- 'c' ; b$word <- NULL } ) 
    observeEvent( input$key.d, { b$key <- 'd' ; b$word <- NULL } ) 
    observeEvent( input$key.e, { b$key <- 'e' ; b$word <- NULL } ) 
    observeEvent( input$key.f, { b$key <- 'f' ; b$word <- NULL } ) 
    observeEvent( input$key.g, { b$key <- 'g' ; b$word <- NULL } ) 
    observeEvent( input$key.h, { b$key <- 'h' ; b$word <- NULL } ) 
    observeEvent( input$key.i, { b$key <- 'i' ; b$word <- NULL } ) 
    observeEvent( input$key.j, { b$key <- 'j' ; b$word <- NULL } ) 
    observeEvent( input$key.k, { b$key <- 'k' ; b$word <- NULL } ) 
    observeEvent( input$key.l, { b$key <- 'l' ; b$word <- NULL } ) 
    observeEvent( input$key.m, { b$key <- 'm' ; b$word <- NULL } ) 
    observeEvent( input$key.n, { b$key <- 'n' ; b$word <- NULL } ) 
    observeEvent( input$key.o, { b$key <- 'o' ; b$word <- NULL } ) 
    observeEvent( input$key.p, { b$key <- 'p' ; b$word <- NULL } ) 
    observeEvent( input$key.q, { b$key <- 'q' ; b$word <- NULL } ) 
    observeEvent( input$key.r, { b$key <- 'r' ; b$word <- NULL } ) 
    observeEvent( input$key.s, { b$key <- 's' ; b$word <- NULL } ) 
    observeEvent( input$key.t, { b$key <- 't' ; b$word <- NULL } ) 
    observeEvent( input$key.u, { b$key <- 'u' ; b$word <- NULL } ) 
    observeEvent( input$key.v, { b$key <- 'v' ; b$word <- NULL } ) 
    observeEvent( input$key.w, { b$key <- 'w' ; b$word <- NULL } ) 
    observeEvent( input$key.x, { b$key <- 'x' ; b$word <- NULL } ) 
    observeEvent( input$key.y, { b$key <- 'y' ; b$word <- NULL } ) 
    observeEvent( input$key.z, { b$key <- 'z' ; b$word <- NULL } ) 
    observeEvent( input$key.BACK, { b$key <- 'BACK' ; b$word <- NULL } ) 
    observeEvent( input$key.SPACE, { b$key <- 'SPACE' ; b$word <- NULL } ) 
    observeEvent( input$key.RET, { b$key <- 'RET' ; b$word <- NULL } ) 
    
    output$edit <- renderUI({ blob <- edittext(isolate(input$text), b$word, b$key)
                              tags$textarea(id="text", rows=7, style="width:400px;", blob)
                            })
    
    output$nbest <- renderUI({ b$nbest <- Predict(getLM(), input$text, getprefix(input$text))
                               fluidRow( column(12, align="center",
                                                actionButton("nbest1", label = b$nbest[1]), 
                                                actionButton("nbest2", label = b$nbest[2]),
                                                actionButton("nbest3", label = b$nbest[3]), 
                                                actionButton("nbest4", label = b$nbest[4]),
                                                actionButton("nbest5", label = b$nbest[5])
                                       ))
                                 })
    
    # Evaluation panel
    v <- reactiveValues(text= NULL)
    
    observeEvent( input$predict, { v$text <- input$input } )
    observeEvent( input$clear,   { v$text <- NULL } )
    
    output$eval <- renderUI({ textInput("input", "Input", v$text) })
    
    output$result <- renderTable({ if(!is.null(v$text))  nbest <- Predict(getLM(), v$text)
                                   else nbest <- character(0)
                                   xtable(data.frame(N.Best=nbest))
                                })
    
    # under the hood panel
    output$table1 <- renderTable ({ xtable(as.data.frame(summary(getLM())[1:3]), 
                                           caption= "LM Summary",
                                           include.rownames = FALSE) 
                                  })
    
    output$table2 <- renderTable ({ xtable(summary(getLM())$stats, 
                                           caption= "LM Statistics",
                                           include.rownames = FALSE) 
                                    })
    

    
  }
)

