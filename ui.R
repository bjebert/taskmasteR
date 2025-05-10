library(shiny)
library(data.table)
library(gfonts)
library(shinyTime)
library(shinyWidgets)
library(shinyjs)

breaks <- function(n) tagList(lapply(1:n, function(x) br()))

ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    tags$style(
        HTML(readLines("style.css"))
    ),
    tags$script(
        HTML(readLines("scripts.js"))
    ),
    
    gfonts::use_font("catamaran", "fonts/css/catamaran.css"),
    
    # App title ----
    titlePanel("taskmasteR"),
    
    absolutePanel(
        width = "640px", top = "65px",
        wellPanel(
            absolutePanel(left = "0px", top = "2px", div(id = "getTask", actionButton("getTask", label = h4("Get Task"), width = "600px"))),
            htmlOutput("taskStatusUi"),
            breaks(2)
        )
    ),
    
    absolutePanel(
        # id = "taskUiPanel",
        width = "640px", top = "150px",
        wellPanel(style = "padding: 5px 15px; overflow-y: scroll; max-height: 925px;",
            htmlOutput("taskUi"),
        )
    ),
    
    absolutePanel(
        left = "665px", top = "5px", 
        h3(textOutput("dateTime"))
    ),
    
    absolutePanel(
        left = "937px", top = "20px",
        actionButton("datePrev", label = NULL, icon = icon("angle-left")),
    ),
    
    absolutePanel(
        left = "977px", top = "20px",
        actionButton("dateNext", label = NULL, icon = icon("angle-right")),
    ),
    
    absolutePanel(
        left = "665px", width = "350px",
        wellPanel(
            fluidRow(
                style = "padding-left: 10px",
                textInput("taskName", label = "Description", value = "", width = "85%"),
                timeInput("taskTimeStart", seconds = FALSE, label = "Time Restriction (Start)"),
                absolutePanel(top = "82px", left = "180px", checkboxInput("taskTimeFixed", value = FALSE, label = "Fixed Time")),
                timeInput("taskTimeEnd", seconds = FALSE, label = "Time Restriction (End)", value = "23:59:59"),
                selectInput("taskPrerequisites", label = "Prerequisites", choices = c(), multiple = TRUE, width = "85%"),
                selectInput("taskParent", label = "Parent", choices = c(), multiple = FALSE, width = "85%"),
                checkboxGroupInput("taskRecurrence", label = "Recurrence", choiceNames = c("M", "T", "W", "T", "F", "S", "S"), choiceValues = 1:7, inline = T),
                numericInputIcon("taskLikelihood", label = "Likelihood", value = 100, 
                                 min = 0, max = 100, width = "30%", icon = list(NULL, icon("percent")), size = "sm"),
                absolutePanel(top = "446px", left = "140px", width = "100px", numericInput("taskDuration", label = "Duration", value = NA, min = 0, max = 60*60)),
                actionButton("createTask", label = "Create Task"),
                actionButton("cancelEdit", label = "Cancel")
            )
        )
    ),
    
    absolutePanel(
        left = "665px", top = "626px", 
        fileInput("loadHistorical", label = "", width = "350px", buttonLabel = "Load Historical...")
    ),
    
    absolutePanel(
        left = "665px", top = "686px", 
        actionButton("clearTasks", label = strong("Clear Tasks"), width = "112px"),
    ),
    
    absolutePanel(
        left = "665px", top = "726px", 
        actionButton("gptEnhance", label = "Enhance", width = "114px", icon = icon("tornado")),
    ),
    
    absolutePanel(
        left = "784px", top = "726px", 
        selectInput("gptVersion", label = NULL, width = "231px", selected = "chatgpt-4o-latest",
                    choices = c("gpt-4o-mini", "chatgpt-4o-latest"))
    ),
    
    absolutePanel(
        left = "665px", top = "766px", 
        textAreaInput("gptEnhanceContext", label = NULL, placeholder = "Context", width = "231px",
                      rows = 4)
    ),
    
    absolutePanel(
        left = "901px", top = "766px", 
        selectInput("gptEnhanceSize", label = NULL, width = "114px", choices = c("tiny", "normal", "big", "huge", "absurd"),
                    selected = "normal"),
    ),
    
    absolutePanel(
        left = "901px", top = "806px", 
        numericInput("gptEnhanceAmount", label = NULL, value = 3, min = 1, max = 10, width = "54px")
    ),
    
    absolutePanel(
        left = "665px", top = "866px",
        actionButton("gptPlanDay", label = strong("Plan Day"), width = "114px", icon = icon("calendar-plus")),
    ),   
    
    absolutePanel(
        left = "784px", top = "866px",
        actionButton("gptClearPlan", label = "Clear Plan", width = "114px"),
    ),    
    
    absolutePanel(
        left = "908px", top = "851px",
        div(style = "color: white", checkboxInput("gptPlanIsContiguous", value = FALSE, label = "Contiguous")),
    ),
    
    absolutePanel(
        left = "908px", top = "871px",
        div(style = "color: white", checkboxInput("gptPlanIsOrdered", value = FALSE, label = "Keep Order")),
    ),
    
    absolutePanel(
        left = "665px", top = "906px", 
        absolutePanel(top = "0px", left = "0px", div(style = "color: white", 
                                                     checkboxInput(inputId = "isGptMotivational", value = FALSE, label = "Motivation")))
    ),
    
    absolutePanel(
        left = "665px", top = "946px", width = "350px",
        uiOutput("gptWellPanel")
    )
)
