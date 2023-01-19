library(shinyWidgets)
library(data.table)

rv <- reactiveValues()

if(file.exists("tasks.csv")) {
    rv[["tasks"]] <- fread("tasks.csv")
} else {
    rv[["tasks"]] <- data.table()
}

get_tasks_ui <- function() {
    if(nrow(rv[["tasks"]]) == 0) {
        return("No tasks have been created yet...")
    }
    
    create_ui_row <- function(top, id, label) {
        fluidRow(
            absolutePanel(left = "20px", top = sprintf("%dpx", top), checkboxInput(sprintf("chk%d", id), label = label)),
            absolutePanel(left = "500px", top = sprintf("%dpx", top), actionButton(sprintf("edit%d", id), "", icon = icon("edit"))),
            absolutePanel(left = "550px", top = sprintf("%dpx", top), actionButton(sprintf("del%d", id), "", icon = icon("trash"))),
        )
    }
    
    lapply(1:nrow(rv[["tasks"]]), function(i) create_ui_row(i*40 - 25, i, rv[["tasks"]][["Name"]][i]))
}


create_task <- function(task_name, task_weekly, task_time_start, task_time_end, task_prerequisites, task_repetitions) {
    start_hhmm <- sprintf("%02d:%02d", hour(task_time_start), minute(task_time_start))
    end_hhmm <- sprintf("%02d:%02d", hour(task_time_end), minute(task_time_end))
    
    task_dt <- data.table(Name = task_name, Weekly = paste(task_weekly, collapse = ","), TimeStart = start_hhmm, TimeEnd = end_hhmm,
                          Prerequisites = task_prerequisites, Repetitions = task_repetitions, Completions = NA)
    
    rv[["tasks"]] <- rbind(rv[["tasks"]], task_dt)
    fwrite(rv[["tasks"]], "tasks.csv")
}


server <- function(input, output, session) {
    output$dateTime <- renderText({
        invalidateLater(1, session)
        format(Sys.time(), format = "%A, %d %B %Y, %H:%M:%S")
    })
    
    tasks_ui <- eventReactive(rv[["tasks"]], get_tasks_ui())
    output$taskUi <- renderUI(tasks_ui())
    
    observeEvent(input$createTask, {
        create_task(input$taskName, input$taskWeekly, input$taskTimeStart, input$taskTimeEnd, input$taskPrerequisites, input$taskRepetitions)
    })
    
    # Temporary hack to allow deletion of 100+ tasks from the start; though if lots more are deleted then created, we may run out 
    del_obs <- lapply(1:(nrow(fread("tasks.csv")) + 100), function(N) {
        print(sprintf("Observer %d delete created", N))
        observeEvent(input[[sprintf("del%d", N)]], {
            print(sprintf("Deleting %d (from del_obs)", N))
            rv[["tasks"]] <- rv[["tasks"]][!N] 
            del_obs <<- del_obs[1:(length(del_obs) - 1)]
            print(length(del_obs))
        })
    })
}
