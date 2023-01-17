library(shinyWidgets)
library(data.table)


get_tasks_ui <- function() {
    if(file.exists("tasks.csv")) {
        tasks <- fread("tasks.csv")
    } else {
        tasks <- data.table()
    }
    
    if(nrow(tasks) == 0) {
        return("No tasks have been created yet...")
    }
    
    tasks <- fread("tasks.csv")
    
    lapply(1:nrow(tasks), function(i) {
        checkboxInput(inputId = sprintf("task%d", i), label = tasks[1][["Name"]])
    })
}


create_task <- function(task_name, task_weekly, task_time_start, task_time_end, task_prerequisites, task_repetitions) {
    if(file.exists("tasks.csv")) {
        tasks <- fread("tasks.csv")
    } else {
        tasks <- data.table()
    }
    
    start_hhmm <- sprintf("%02d:%02d", hour(task_time_start), minute(task_time_start))
    end_hhmm <- sprintf("%02d:%02d", hour(task_time_end), minute(task_time_end))
    
    task_dt <- data.table(Name = task_name, Weekly = paste(task_weekly, collapse = ","), TimeStart = start_hhmm, TimeEnd = end_hhmm,
                          Prerequisites = task_prerequisites, Repetitions = task_repetitions, Completions = NA)
    
    tasks <- rbind(tasks, task_dt)
    fwrite(tasks, "tasks.csv")
}


server <- function(input, output, session) {
    output$dateTime <- renderText({
        invalidateLater(1, session)
        format(Sys.time(), format = "%A, %d %B %Y, %H:%M:%S")
    })
    
    output$tasks <- renderUI({
        get_tasks_ui()
    })
    
    observeEvent(input$createTask, {
        create_task(input$taskName, input$taskWeekly, input$taskTimeStart, input$taskTimeEnd, input$taskPrerequisites, input$taskRepetitions)
    })
}
