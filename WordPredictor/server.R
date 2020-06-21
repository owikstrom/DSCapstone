library(shiny)
library(tidyverse)
source("../predict.R")

shinyServer(function(input, output) {
    
    db <- readRDS('finalmodel.rds')
    last_input <- ""
    
    updatePrediction <- reactive({
            predictWord(input$phrase,db)          
    })
    # values <- reactiveValues(prediction = 0)
    # values$prediction = "" 

    output$prediction <- renderText({
        paste(updatePrediction())
    })
    
    # observeEvent(input$submit, { 
    #     values$prediction <- updatePrediction()
    # })
})
