library(shinyWidgets)
library(data.table)
library(shinyjs)
library(beepr)
library(profvis)


# Global variables --------------------------------------------------------

read_tasks <- function(f) {
    if(file.exists(f)) {
        tasks_tmp <- fread(f)
    } else {
        tasks_tmp <- fread("habits.csv")
        tasks_tmp[, IsVisible := FALSE]
        tasks_tmp[, IsComplete := FALSE]
        tasks_tmp[, IsActive := FALSE]
    }
    
    if("Completions" %in% colnames(tasks_tmp)) {  # Legacy interface
        tasks_tmp[, Completions := NULL]
    }
    
    return(tasks_tmp)
}


rv <- reactiveValues(task_mode = FALSE,
                     task_timer_active = FALSE,
                     task_start_time = NULL,
                     task_duration_seconds = 0,
                     last_duration = NULL,
                     edit_mode = FALSE,
                     edit_id = NA,
                     last_beep = Sys.time())

rv[["tasks"]] <- read_tasks(sprintf("history/tasks_%s.csv", gsub("-", "_", Sys.Date())))
statistics_dt <- fread("statistics.csv")

chk_observers <- list()
edit_observers <- list()
del_observers <- list()

gpt_agent_motivation <- ellmer::chat_openai(model = "gpt-4o-mini", system_prompt = readLines("promptMotivation"))
gpt_agent_enhance <- ellmer::chat_openai(model = "gpt-4o-mini", system_prompt = readLines("promptEnhance"))

# Helpers -----------------------------------------------------------------

breaks <- function(n) tagList(lapply(1:n, function(x) br()))

load_habits <- function() {
    tasks_tmp <- copy(rv[["tasks"]])
    hab_dt <- tasks_tmp[Recurrence != ""]
    tasks_tmp[Recurrence != "", IsVisible := Likelihood >= runif(nrow(hab_dt), min = 0, max = 100)]
    tasks_tmp[Recurrence != "", IsVisible := IsVisible & sapply(strsplit(hab_dt[["Recurrence"]], ";"), function(x) (wday(Sys.Date()) - 1) %in% as.numeric(x))]
    tasks_tmp[Recurrence != "", IsComplete := FALSE]
    tasks_tmp[IsComplete == TRUE, IsActive := FALSE]
    
    rv[["tasks"]] <<- copy(tasks_tmp)
}


get_tasks_ui <- function() {
    visible_tasks <- rv[["tasks"]][IsVisible == TRUE]
    if(nrow(visible_tasks) == 0) {
        return(absolutePanel("No tasks have been created yet..."))
    }
    
    create_ui_row <- function(top, id, label, time_start, time_end, prerequisites, is_complete, is_active) {
        label_full <- copy(label)
        active_colour <- "red"
        complete_colour <- "palegreen"
        inactive_colour <- "lightgray"
        
        if((time_start != "00:00" || time_end != "23:59") && time_start < time_end) {
            time_colour <- ifelse(is_complete, complete_colour, inactive_colour)
            label_full <- HTML(sprintf("%s <span style=color:%s><i>(%s - %s)</i></span>", label_full, time_colour, time_start, time_end))
        }
        
        if(all(!is.na(prerequisites)) && length(prerequisites)) {
            prereq_joined <- paste(prerequisites, collapse = ", ")
            
            MAX_PREREQ_CHAR_LEN <- 42
            
            if((nchar(prereq_joined) + nchar(label_full)) > MAX_PREREQ_CHAR_LEN) {
                prereq_joined <- sprintf("%s...", substr(prereq_joined, 1, MAX_PREREQ_CHAR_LEN - nchar(label_full)))
            }
            
            label_full <- HTML(sprintf("%s <i>(Req: %s)</i>", label_full, prereq_joined))
        }
        
        is_current_time_in_task_window <- as.POSIXlt(sprintf("%s %s", Sys.Date(), time_start)) < Sys.time() &&
            Sys.time() < as.POSIXlt(sprintf("%s %s", Sys.Date(), time_end))
        
        # Grey out if not currently doable (because of time or prereqs)
        if((all(!is.na(prerequisites)) && length(prerequisites)) || !is_current_time_in_task_window) {
            label_full <- HTML(sprintf("<span style=color:%s>%s</span>", inactive_colour, label_full))
        }
        
        if(is.na(is_active)) {
            p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id))
        } else if(is_active) {
            p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id), style = sprintf('color: %s; font-weight: bold', active_colour))
        } else {
            p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id), style = sprintf('color: %s', inactive_colour))
        }
        
        if(is_complete) {
            p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id), style = sprintf('text-decoration: line-through; color: %s', complete_colour))
        }
        
        fluidRow(
            absolutePanel(left = "20px", top = sprintf("%dpx", top), checkboxInput(sprintf("chk%d", id), label = p_label, value = is_complete)),
            absolutePanel(left = "500px", top = sprintf("%dpx", top), actionButton(sprintf("edit%d", id), "", icon = icon("edit"))),
            absolutePanel(left = "550px", top = sprintf("%dpx", top), actionButton(sprintf("del%d", id), "", icon = icon("trash"))),
        )
    }
    
    tasks_ui <- lapply(1:nrow(visible_tasks), function(i) {
        active_status <- if(!any(visible_tasks[["IsActive"]])) NA else visible_tasks[["IsActive"]][i]
        
        if(!is.na(visible_tasks[["Prerequisites"]][i])) {
            prerequisites <- strsplit(visible_tasks[["Prerequisites"]][i], ";")[[1]]
            prerequisites <- prerequisites[!(prerequisites %in% visible_tasks[IsVisible == F | IsComplete == T, Name])]
        } else {
            prerequisites <- NA
        }
        
        create_ui_row(i*40 - 25, i, 
                      visible_tasks[["Name"]][i], 
                      visible_tasks[["TimeStart"]][i],
                      visible_tasks[["TimeEnd"]][i],
                      prerequisites,
                      visible_tasks[["IsComplete"]][i],
                      active_status)
    })
    
    return(tasks_ui)
}


task_status_ui <- function() {
    active_task_row <- rv[["tasks"]][IsActive == TRUE]
    
    if(nrow(active_task_row) == 0) {
        exit_task_mode()
    }
    
    task_statistics <- get_task_statistics(active_task_row[["Name"]])
    
    status_ui <- fluidRow(
        absolutePanel(top = "10px", left = "55px", h5(textOutput("formattedTaskTime"))),
        absolutePanel(top = "24px", left = "10px", actionButton("toggleTaskTimer", label = NULL, icon = icon(ifelse(rv[["task_timer_active"]], "pause", "play")))),
        absolutePanel(top = "10px", left = "500px", actionButton("cancelTask", label = "Cancel", width = "90px", icon = icon("cancel"))),
        div(class = "task-statistics",
            absolutePanel(top = "10px", left = "220px", sprintf("Completions: %s", task_statistics[["completions"]])),
            absolutePanel(top = "25px", left = "220px", sprintf("Total Time: %s", format_duration(task_statistics[["total_duration"]]))),
            absolutePanel(top = "40px", left = "220px", sprintf("Average Time: %s", format_duration(task_statistics[["avg_duration"]]))),
            absolutePanel(top = "55px", left = "220px", sprintf("Fastest: %s", format_duration(task_statistics[["fastest_completion"]]))),
            absolutePanel(top = "10px", left = "330px", sprintf("Incomplete: %s", task_statistics[["failures"]])),
            absolutePanel(top = "25px", left = "330px", sprintf("Last Completion: %s", task_statistics[["last_completion"]])),
        )
    )
    
    return(status_ui)
}


format_duration <- function(duration) {
    if(is.null(duration) || is.character(duration)) {
        return("")
    }
    
    if(duration < 60) {
        return(sprintf("%ss", round(duration)))
    } else if(duration < 3600) {
        minutes <- duration %/% 60
        seconds <- round(duration %% 60)
        
        return(sprintf("%sm %ss", minutes, seconds))
    } else {
        hours <- duration %/% 3600
        minutes <- round((duration %% 3600) / 60)
        
        return(sprintf("%sh %sm", hours, minutes))
    }
}


get_task_time_seconds <- function() {
    if(rv[["task_mode"]]) {
        rv$task_timer()
        task_secs <- rv[["task_duration_seconds"]]
        
        if(!is.null(rv[["task_start_time"]])) {
            task_secs <- task_secs + difftime(Sys.time(), rv[["task_start_time"]], units = "secs")
        }
        return(as.numeric(task_secs))
    } else {
        return(NULL)
    }
}


get_formatted_task_time <- function() {
    task_secs <- get_task_time_seconds()
    
    if(!is.null(task_secs)) {
        formatted_time <- ifelse(task_secs >= 3600,
                                 format(.POSIXct(task_secs, tz = "UTC"), "%H:%M:%S"),
                                 format(.POSIXct(task_secs, tz = "UTC"), "%M:%S"))
        
        return(formatted_time)
    }
}


get_task_statistics <- function(task_name) {
    if(is.null(task_name) || !length(task_name)) {
        return(NULL)
    }
    
    task_stats <- statistics_dt[Name == task_name]
    
    if(nrow(task_stats) == 0) {
        return(list(completions = 0,
                    failures = 0,
                    total_duration = "",
                    avg_duration = "",
                    fastest_completion = "",
                    last_completion = ""))
    }
    
    task_success <- task_stats[!is.na(Time)]
    
    if(nrow(task_success) == 0) {
        return(list(completions = 0,
                    failures = nrow(task_stats[is.na(Time)]),
                    total_duration = "",
                    avg_duration = "",
                    fastest_completion = "",
                    last_completion = ""))
    }
    
    return(list(completions = nrow(task_success),
                failures = nrow(task_stats[is.na(Time)]),
                total_duration = sum(task_success[["Duration"]]),
                avg_duration = sum(task_success[["Duration"]]) / nrow(task_success),
                fastest_completion = min(task_success[["Duration"]]),
                last_completion = as.Date(max(task_success[["Time"]]))))
}


create_task <- function(task_name, task_time_start, task_time_end, task_prerequisites, task_recurrence, 
                        task_likelihood) {
    
    start_hhmm <- sprintf("%02d:%02d", hour(task_time_start), minute(task_time_start))
    end_hhmm <- sprintf("%02d:%02d", hour(task_time_end), minute(task_time_end))
    
    if(is.null(task_prerequisites)) {
        task_prerequisites <- ""
    } else {
        task_prerequisites <- paste(task_prerequisites, collapse = ";")
    }
    
    task_dt <- data.table(Name = task_name, TimeStart = start_hhmm, TimeEnd = end_hhmm,
                          Prerequisites = task_prerequisites, Recurrence = paste(task_recurrence, collapse = ";"), 
                          Likelihood = task_likelihood, IsComplete = FALSE, IsActive = FALSE,
                          IsVisible = TRUE)
    
    if(rv[["edit_mode"]]) {
        rv[["tasks"]][IsVisible == TRUE][rv[["edit_id"]]] <<- task_dt
    } else {
        rv[["tasks"]] <- rbind(rv[["tasks"]], task_dt)
    }
}


toggle_check_task <- function(i, chk_val) {
    if(chk_val && difftime(Sys.time(), rv[["last_beep"]], units = "secs") > 0.2) {
        runjs("var audio = new Audio('ding.mp3'); audio.volume = 0.75; audio.play();")
    }
    
    tmp <- copy(rv[["tasks"]])
    idx <- tmp[IsVisible == T, which = T][i]
    
    # Update statistics if completed task was the active task in task mode
    if(rv[["task_mode"]] && tmp[idx][["IsActive"]]) {
        completion_dt <- data.table(Name = tmp[["Name"]][idx],
                                    Time = Sys.time(),
                                    Duration = get_task_time_seconds())
        
        if(nrow(statistics_dt) == 0 || nrow(statistics_dt[!is.na(Name)]) == 0) {
            statistics_dt <<- copy(completion_dt)
        } else {
            statistics_dt <<- rbind(statistics_dt, completion_dt)
        }
    }
    
    tmp[idx, IsComplete := chk_val]
    tmp[idx, IsActive := FALSE]
    rv[["tasks"]] <<- copy(tmp)
}


edit_task <- function(i) {
    if(!rv[["edit_mode"]] || rv[["edit_id"]] != i) {
        rv[["edit_mode"]] <<- TRUE
        rv[["edit_id"]] <<- i
        
        visible_tasks <- rv[["tasks"]][IsVisible == T]
        updateTextInput(session = getDefaultReactiveDomain(), inputId = "taskName", value = visible_tasks[["Name"]][i])
        
        start_split <- strsplit(visible_tasks[["TimeStart"]][i], ":")[[1]]
        start_posix <- as.POSIXlt(sprintf("%s %s:%s:00", Sys.Date(), start_split[1], start_split[2]))
        
        end_split <- strsplit(visible_tasks[["TimeEnd"]][i], ":")[[1]]
        end_posix <- as.POSIXlt(sprintf("%s %s:%s:00", Sys.Date(), end_split[1], end_split[2]))
        
        updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeStart", value = start_posix)
        updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeEnd", value = end_posix)
        
        task_prerequisites <- if(is.na(visible_tasks[["Prerequisites"]][i])) NULL else visible_tasks[["Prerequisites"]][i]
        prereq_split <- if(!is.null(task_prerequisites)) strsplit(task_prerequisites, ";")[[1]] else NULL
        
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites",
                          selected = prereq_split)
        
        task_recurrence <- if(is.na(visible_tasks[["Recurrence"]][i])) NULL else visible_tasks[["Recurrence"]][i]
        recurrence_split <- if(!is.null(task_recurrence)) strsplit(task_recurrence, ";")[[1]] else NULL
        
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), inputId = "taskRecurrence", 
                                 selected = as.numeric(recurrence_split))
        
        updateNumericInputIcon(session = getDefaultReactiveDomain(), inputId = "taskLikelihood", value = visible_tasks[["Likelihood"]][i])
        updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskRepetitions", value = visible_tasks[["Repetitions"]][i])
        
        updateActionButton(session = getDefaultReactiveDomain(), inputId = "createTask", label = "Edit Task")
        
        shinyjs::show(id = "cancelEdit")
    } else {
        cancel_edit()
    }
}


delete_task <- function(i) {
    tmp <- copy(rv[["tasks"]])
    idx <- tmp[IsVisible == T, which = T][i]
    
    rv[["tasks"]] <<- tmp[!idx]
    cancel_edit()
}


reset_task_inputs <- function() {
    updateTextInput(session = getDefaultReactiveDomain(), inputId = "taskName", value = "")
    updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeStart", value = as.POSIXlt(sprintf("%s 00:00:00", Sys.Date())))
    updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeEnd", value = as.POSIXlt(sprintf("%s 23:59:00", Sys.Date())))
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites", selected = "")
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), inputId = "taskRecurrence", selected = numeric(0))
    updateNumericInputIcon(session = getDefaultReactiveDomain(), inputId = "taskLikelihood", value = 100)
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskRepetitions", value = 1)
    updateActionButton(session = getDefaultReactiveDomain(), inputId = "createTask", label = "Create Task")
}


cancel_edit <- function() {
    rv[["edit_mode"]] <- FALSE
    rv[["edit_id"]] <- NA
    
    shinyjs::hide(id = "cancelEdit")
    
    reset_task_inputs()
}


enter_task_mode <- function() {
    rv$task_mode <- TRUE
    rv$task_timer_active <- TRUE
    rv$task_start_time <- Sys.time()
    rv$task_duration_seconds <- 0
    rv$task_timer <- reactiveTimer(1000)
    
    shinyjs::hide(id = "getTask")
    shinyjs::show(id = "taskStatusUi")
}


exit_task_mode <- function() {
    rv$task_mode <- FALSE
    rv$task_timer_active <- FALSE
    rv$task_start_time <- NULL
    rv$last_duration <- rv$task_duration_seconds
    rv$task_duration_seconds <- 0
    rv$task_timer <- NULL
    
    shinyjs::show(id = "getTask")
    shinyjs::hide(id = "taskStatusUi")
}


get_new_task_id <- function() {
    
    # Filter out tasks already complete and invisible tasks
    tasks_tmp <- copy(rv[["tasks"]])
    tasks_tmp[, Id := 1:.N]
    tasks_tmp <- tasks_tmp[IsComplete == FALSE & IsVisible == TRUE]
    
    # Filter out tasks with prerequisites not complete
    missing_prereq_idx <- c()
    
    for(i in 1:nrow(tasks_tmp)) {
        prereq <- tasks_tmp[["Prerequisites"]][i]
        if(all(!is.na(prereq)) && prereq != "") {
            pr_split <- strsplit(prereq, ";")[[1]]
            if(tasks_tmp[Name %in% pr_split, any(!IsComplete)]) {
                missing_prereq_idx <- c(missing_prereq_idx, i)
            }
        }
    }
    
    if(length(missing_prereq_idx)) {
        tasks_tmp <- tasks_tmp[!missing_prereq_idx]
    }
    
    # Filter out incorrect time of day
    tasks_tmp <- tasks_tmp[as.POSIXlt(sprintf("%s %s", Sys.Date(), TimeStart)) < Sys.time() & 
                               Sys.time() < as.POSIXlt(sprintf("%s %s", Sys.Date(), TimeEnd))]
    
    if(nrow(tasks_tmp) == 0) {
        return(NA)  
    } else if(nrow(tasks_tmp) == 1) {
        return(tasks_tmp[["Id"]])
    }
    
    return(sample(tasks_tmp[["Id"]], 1))
}


server <- function(input, output, session) {
    shinyjs::hide("cancelEdit")
    shinyjs::hide("taskStatusUi")
    
    label_timer <- reactiveTimer(1000)
    output$dateTime <- renderText({
        label_timer()
        format(Sys.time(), format = "%A, %d %B %Y, %H:%M:%S")
    })
    
    output$taskUi <- renderUI({
        ui <- get_tasks_ui()
        N <- length(ui)
        
        if("children" %in% names(ui)) {  # Single absolutePanel with no tasks label
            N <- 0
        }
        
        # Create UI observers ---
        if(N > length(chk_observers)) {
            i <- length(chk_observers) + 1
            chk_observers <<- c(chk_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("chk%d", i)]], toggle_check_task(i, input[[sprintf("chk%d", i)]]))))
            edit_observers <<- c(edit_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("edit%d", i)]], edit_task(i))))
            del_observers <<- c(del_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("del%d", i)]], delete_task(i))))
        }
        
        return(ui) 
    })
    
    output$taskStatusUi <- renderUI(task_status_ui())
    
    output$formattedTaskTime <- renderText({
        get_formatted_task_time()
    })
    
    observeEvent(rv[["tasks"]], {
        if(nrow(rv[["tasks"]]) == 0) {
            updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites", choices = "")
        } else {
            updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites", choices = rv[["tasks"]][IsVisible == T][["Name"]])
        }
        
        fwrite(rv[["tasks"]], sprintf("history/tasks_%s.csv", gsub("-", "_", Sys.Date())))
        fwrite(rv[["tasks"]][Recurrence != ""], "habits.csv")
    })
    
    observeEvent(input$createTask, {
        create_task(input$taskName, input$taskTimeStart, input$taskTimeEnd, input$taskPrerequisites, input$taskRecurrence, 
                    input$taskLikelihood)
        
        cancel_edit()
    })
    
    observeEvent(input$cancelEdit, {
        cancel_edit()
    })
    
    observeEvent(input$getTask, {
        enter_task_mode()
        
        tmp <- copy(rv[["tasks"]])
        if(nrow(tmp[IsVisible == T]) == 0) {
            return()
        }
        
        new_task_id <- get_new_task_id()
        
        if(is.na(new_task_id)) {
            return()
        }
        
        tmp[, IsActive := 1:.N == new_task_id]
        rv[["tasks"]] <<- copy(tmp)
    })
    
    observeEvent(input$cancelTask, {
        tmp <- copy(rv[["tasks"]])
        tmp[, IsActive := F]
        rv[["tasks"]] <<- copy(tmp)
        
        exit_task_mode()
    })
    
    observeEvent(input$toggleTaskTimer, {
        rv[["task_timer_active"]] <- !rv[["task_timer_active"]]
        
        if(rv[["task_timer_active"]]) {
            # Timer started
            rv$task_start_time <- Sys.time()
            updateActionButton(session, "toggleTaskTimer", icon = icon("pause"))
        } else {
            # Timer paused
            rv[["task_duration_seconds"]] <- rv[["task_duration_seconds"]] + floor(difftime(Sys.time(), rv$task_start_time, units = "secs"))
            
            rv$task_start_time <- NULL
            updateActionButton(session, "toggleTaskTimer", icon = icon("play"))
        }
    })
    
    # Right sidebar -----------------------------------------------------------
    
    observeEvent(input$loadHabits, {
        rv[["tasks"]] <<- load_habits()
    })
    
    observeEvent(input$clearHabits, {
        tmp <- copy(rv[["tasks"]])
        tmp[Recurrence != "", IsVisible := FALSE]
        rv[["tasks"]] <<- copy(tmp)
    })
    
    observeEvent(input$clearTasks, {
        rv[["tasks"]] <<- rv[["tasks"]][Recurrence != ""]
        rv[["tasks"]][, IsVisible := FALSE]
    })    
    
    observeEvent(input$loadHistorical, {
        tmp <- tryCatch(expr = {
            f2 <- fread(input[["loadHistorical"]][["datapath"]])
            if(ncol(f2) == 10 && "Recurrence" %in% colnames(f2)) {
                f2
            } else {
                NULL
            }
        }, error = function(x) NULL)
        
        if(!is.null(tmp)) {
            rv[["tasks"]] <<- copy(tmp)
        }
    })
    
    
    # GPT Enhance -------------------------------------------------------------
    
    observeEvent(input$gptEnhance, {
        visible_tasks <- rv$tasks[IsVisible == TRUE]
        
        chat_message <- sprintf("Current time is %s.  The user's tasks today are: %s.",
                                format(Sys.time(), "%A %H:%M:%S"),
                                paste(visible_tasks[["Name"]], collapse = "; "))
        
        response <- gpt_agent_enhance$chat(chat_message, echo = "none")
        
        tryCatch({
            new_tasks <- strsplit(response, "; ")[[1]]
            new_task_dt <- rbindlist(lapply(new_tasks, function(task) {
                start_hhmm <- "00:00"
                end_hhmm <- "23:59"
                task_prerequisites <- NA
                task_recurrence <- ""
                task_likelihood <- 100
                
                if(grepl("\\(.*\\)", task)) {
                    ts <- strsplit(task, " \\(")[[1]]
                    task <- ts[1]
                    times <- strsplit(gsub("\\)", "", ts[2]), "-")[[1]]
                    start_hhmm <- times[1]
                    end_hhmm <- times[2]
                    
                    if(start_hhmm == end_hhmm) {
                        start_hhmm <- "00:00"
                        end_hhmm <- "23:59"
                    }
                }
                
                return(data.table(Name = task, TimeStart = start_hhmm, TimeEnd = end_hhmm,
                                  Prerequisites = task_prerequisites, Recurrence = paste(task_recurrence, collapse = ";"), 
                                  Likelihood = task_likelihood, IsComplete = FALSE, IsActive = FALSE,
                                  IsVisible = TRUE))
            }))
            
            rv[["tasks"]] <- rbind(rv[["tasks"]], new_task_dt)
        })
    })
    
    
    # GPT motivational panel ------------------------------------------------------
    
    output$gptWellPanel <- renderUI({
        class <- if(rv[["task_mode"]]) "active-gpt-bg" else "inactive-gpt-bg"
        
        wellPanel(
            class = class,
            htmlOutput("gptOutput"),
            breaks(2)
        )
    })
    
    # Dummy output for demonstration
    output$gptOutput <- renderText({
        if(!rv[["task_mode"]] || !input$isGptMotivational) {
            return("")
        }
        
        # Generate a GPT motivational output for the current task
        current_task <- rv$tasks[IsActive == TRUE]
        other_tasks <- paste(rv$tasks[IsActive == FALSE & IsVisible == TRUE & IsComplete == FALSE][["Name"]], collapse = "; ")
        
        visible_tasks <- rv$tasks[IsVisible == T]
        
        chat_message <- sprintf("Current time is %s.  The current task is: %s.  The user's other tasks today are: %s.  The user has completed %d/%d tasks today.",
                                format(Sys.time(), "%A %H:%M:%S"),
                                current_task[["Name"]],
                                other_tasks,
                                visible_tasks[, sum(IsComplete)],
                                nrow(visible_tasks))
        
        response <- gpt_agent_motivation$chat(chat_message, echo = "none")
        
        return(response)
    })
    
    # Write statistics on stop ------------------------------------------------
    
    onStop(function() {
        if(isolate(rv[["task_mode"]])) {
            current_task <- isolate(rv$tasks)[IsActive == TRUE]
            if(nrow(current_task) > 0) {
                # Add a "failed" completion for the current task
                task_secs <- as.numeric(isolate(rv[["task_duration_seconds"]]) + difftime(Sys.time(), isolate(rv[["task_start_time"]]), units = "secs"))
                
                completion_dt <- data.table(Name = current_task[["Name"]],
                                            Time = as.POSIXct(NA),
                                            Duration = task_secs)
                
                if(nrow(statistics_dt) == 0) {
                    statistics_dt <<- copy(completion_dt)
                } else {
                    statistics_dt <<- rbind(statistics_dt, completion_dt)
                }
            }
        }
        
        fwrite(statistics_dt, "statistics.csv")
    })
}
