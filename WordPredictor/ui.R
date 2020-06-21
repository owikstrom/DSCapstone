library(shiny); library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    navbarPage("Word Predictor",
        tabPanel("Main",
            # Sidebar 
            # sidebarLayout(position= 'right',
            #     sidebarPanel(
            #     ),
            # Show a plot of the generated distribution
            #     mainPanel(
            # ),
            # ),
            fluidRow(
                column(4,
                       textInput("phrase", label = "Phrase segment")
                ),
                column(4,
                       strong("The word is : "),
                       textOutput("prediction", container = div, inline = FALSE)
                       
                )
            ),
            fluidRow(
                    column(4,
                           # actionButton("submit", label="Submit phrase")  
                    )           
            )    
        ),
        tabPanel("Help",
            includeMarkdown("../CapstonePresentation.Rmd")
        )
    )
))
