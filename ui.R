library(shiny)
library(data.table)
library(gfonts)
library(shinyTime)

ui <- fluidPage(
    tags$style(
        HTML(
            'body {
                 background-color: #388E3C;
             }
             h2 {
                 color: #F5FFFA;
                 font-size: 32px;
             }
             h3 {
                 color: #F5FFFA;
             }
             .rightAlign {
                float: right;
             }
             h4 {
              padding-top: 7px;
              font-size: 32px;
              animation: color-change 8s infinite;
             }
            
            @keyframes color-change {
              0% { color: #003300; }
              50% { color: #00FF00; }
              100% { color: #003300; }
             }
             '
        )
    ),
    
    gfonts::use_font("catamaran", "fonts/css/catamaran.css"),
    
    # App title ----
    titlePanel("taskmasteR"),
    
    absolutePanel(
        width = "600px",
        actionButton("getTask", label = h4("Get Task"), width = "100%",
                     style = "background-color: #F5FFFA")
    ),
    
    absolutePanel(
        width = "600px", top = "150px",
        wellPanel(
            style = "background-color: #F5FFFA",
            htmlOutput("taskUi"),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
        )
    ),
    
    absolutePanel(
        top = "5px", left = "650px",
        h3(textOutput("dateTime"))
    ),
    
    absolutePanel(
        left = "650px", width = "350px",
        wellPanel(
            style = "background-color: #F5FFFA",
            fluidRow(
                style = "padding-left: 10px",
                textInput("taskName", label = "Description", value = "", width = "85%"),
                timeInput("taskTimeStart", seconds = FALSE, label = "Time Restriction (Start)"),
                timeInput("taskTimeEnd", seconds = FALSE, label = "Time Restriction (End)", value = "23:59:59"),
                selectInput("taskPrerequisites", label = "Prerequisites", choices = c(), multiple = TRUE, width = "85%"),
                checkboxGroupInput("taskRecurrence", label = "Recurrence", choiceNames = c("M", "T", "W", "T", "F", "S", "S"), choiceValues = 1:7, inline = T),
                numericInputIcon("taskLikelihood", label = "Likelihood", value = 100, 
                                 min = 0, max = 100,
                                 width = "30%", icon = list(NULL, icon("percent")), size = "sm"),
                numericInput("taskRepetitions", label = "Repetitions", value = 1, width = "25%"),
                actionButton("createTask", label = "Create Task")
            )
        )
    )
)
