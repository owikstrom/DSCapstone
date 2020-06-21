library(shiny)
library(tidyverse)
source("../predict.R")

shinyServer(function(input, output) {
    
    db <- readRDS('finalmodel.rds')

    values <- reactiveValues(prediction = 0)
    values$prediction = 0 

    output$prediction <- renderText({
        paste(values$prediction)
    })
    
    observeEvent(input$submit, { 
        values$prediction <- predictWord(input$phrase,db)
    })
})
