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
        HTML("$(function(){ 
                $(document).keyup(function(e) {
                    if (e.code == 'Enter') {
                        $('#createTask').click()
                    }
                });
            })")
    ),
    
    gfonts::use_font("catamaran", "fonts/css/catamaran.css"),
    
    # App title ----
    titlePanel("taskmasteR"),
    
    absolutePanel(
        width = "600px", top = "60px",
        wellPanel(
            absolutePanel(left = "0px", top = "2px", div(id = "getTask", actionButton("getTask", label = h4("Get Task"), width = "600px"))),
            htmlOutput("taskStatusUi"),
            breaks(2)
        )
    ),
    
    absolutePanel(
        width = "600px", top = "150px",
        wellPanel(
            htmlOutput("taskUi"),
            breaks(48)
        )
    ),
    
    absolutePanel(
        top = "5px", left = "650px",
        h3(textOutput("dateTime"))
    ),
    
    absolutePanel(
        left = "650px", width = "350px",
        wellPanel(
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
                actionButton("createTask", label = "Create Task"),
                actionButton("cancelEdit", label = "Cancel")
            )
        )
    ),
    
    absolutePanel(
        top = "577px", left = "650px", 
        actionButton("loadHabits", label = strong("Load Habits"), width = "114px"),
    ),
    
    absolutePanel(
        top = "577px", left = "769px",
        actionButton("clearHabits", label = strong("Clear Habits"), width = "114px"),
    ),    
    
    absolutePanel(
        top = "577px", left = "888px",
        actionButton("clearTasks", label = strong("Clear Tasks"), width = "112px"),
    ),
    
    absolutePanel(
        top = "596px", left = "650px",
        fileInput("loadHistorical", label = "", width = "350px", buttonLabel = "Load Historical...")
    )
)
