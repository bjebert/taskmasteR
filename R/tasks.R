
# tasks.R: task managemenet -----------------------------------------------
#' functions related to task management; creating, editing, deleting, atomising


id2task <- function(tasks_dt, ids) {
    sapply(ids, function(x) tasks_dt[Id == x][1][["Name"]])
}


task2id <- function(tasks_dt, tasks) {
    sapply(tasks, function(x) tasks_dt[Name == x][1][["Id"]])
}


read_tasks <- function(f) {
    if(file.exists(f)) {
        tasks_tmp <- fread(f)
    } else {
        tasks_tmp <- data.table(Id = character(0), Name = character(0), TimeStart = character(0), 
                                TimeEnd = character(0), Prerequisites = character(0), Recurrence = character(0), 
                                Likelihood = integer(0), IsComplete = logical(0), IsActive = logical(0), 
                                IsVisible = logical(0), IsFixed = logical(0), TaskDuration = integer(0), 
                                PlanStart = character(0), PlanEnd = character(0))
    }
    
    # Legacy interfacing ---
    if(!("Id" %in% colnames(tasks_tmp))) {
        tasks_tmp[, Id := sprintf("T%s", .I)]
        
        # Move Id to first column
        w <- which(colnames(tasks_tmp) == "Id")
        tasks_tmp <- tasks_tmp[, c(w, setdiff(1:ncol(tasks_tmp), w)), with = F]
    }
    
    if("Completions" %in% colnames(tasks_tmp)) {  
        tasks_tmp[, Completions := NULL]
    }
    
    if("Parent" %in% colnames(tasks_tmp)) {
        tasks_tmp[, Parent := NULL]
    }
    
    if(!("IsFixed" %in% colnames(tasks_tmp))) {  
        tasks_tmp[, IsFixed := FALSE]
    }
    
    if(!("TaskDuration" %in% colnames(tasks_tmp))) {
        tasks_tmp[, TaskDuration := 0]
    }
    
    if(!("PlanStart" %in% colnames(tasks_tmp))) {
        tasks_tmp[, PlanStart := as.character(NA)]
    }    
    
    if(!("PlanEnd" %in% colnames(tasks_tmp))) {
        tasks_tmp[, PlanEnd := as.character(NA)]
    }    
    
    # Convert format and filter ---
    tasks_tmp[, Prerequisites := as.character(Prerequisites)]
    tasks_tmp[, Recurrence := as.character(Recurrence)]
    tasks_tmp <- tasks_tmp[!is.na(IsComplete) & Name != ""]
    
    return(order_tasks(tasks_tmp))
}


order_tasks <- function(tasks) {
    if(any(!is.na(tasks[["PlanStart"]]) | !(tasks[["PlanStart"]] %in% c("0:00", "00:00")))) {
        return(tasks[order(-IsComplete, as.POSIXct(PlanStart, format = "%H:%M"))])
    } else {
        return(rbind(tasks[IsFixed == T][order(TimeStart)], tasks[IsFixed == F]))
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


create_task_id <- function() {
    i <- as.numeric(gsub("T", "", rv[["tasks"]][["Id"]]))
    
    if(length(i) == 0) {
        return("T1")
    } else {
        return(sprintf("T%d", setdiff(1:(max(i)+1), i)[1]))
    }
}


get_new_task_id <- function() {
    #' Chooses a task randomly for the user and returns its id
    
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


create_task <- function(task_name, task_time_start, task_time_end, task_prerequisites,
                        task_recurrence, task_likelihood, task_time_fixed, task_duration) {
    
    start_hhmm <- sprintf("%02d:%02d", hour(task_time_start), minute(task_time_start))
    end_hhmm <- sprintf("%02d:%02d", hour(task_time_end), minute(task_time_end))
    
    if(is.null(task_prerequisites)) {
        task_prerequisites <- ""
    } else {
        task_prerequisites <- paste(id2names(rv[["tasks"]], task_prerequisites), collapse = ";")
        # task_prerequisites <- paste(task_prerequisites, collapse = ";")
    }
    
    task_duration <- if(is.na(task_duration)) 0 else task_duration
    
    if(task_time_fixed) {
        task_duration <- as.numeric(difftime(as.POSIXct(task_time_end, format = "%H:%M"),
                                             as.POSIXct(task_time_start, format = "%H:%M"),
                                             units = "mins"))
    }
    
    task_dt <- data.table(Id = create_task_id(), Name = task_name, TimeStart = start_hhmm, TimeEnd = end_hhmm,
                          Prerequisites = task_prerequisites, Recurrence = paste(task_recurrence, collapse = ";"), 
                          Likelihood = task_likelihood, IsComplete = FALSE, IsActive = FALSE,
                          IsVisible = TRUE, IsFixed = task_time_fixed, TaskDuration = task_duration,
                          PlanStart = NA, PlanEnd = NA)
    
    if(rv[["edit_mode"]]) {
        rv[["tasks"]][IsVisible == TRUE][rv[["edit_id"]]] <<- task_dt
    } else {
        rv[["tasks"]] <- rbind(rv[["tasks"]], task_dt)
    }
    
    # Order fixed tasks and put them on top
    rv[["tasks"]] <- order_tasks(rv[["tasks"]])
}


toggle_check_task <- function(i, chk_val, session) {
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
        
        # Show notification
        task_statistics <- get_task_statistics(completion_dt[["Name"]])
        
        message <- paste(c("Task complete!",
                           sprintf("Completions: <b>%s</b>", task_statistics[["completions"]]),
                           sprintf("Duration: <b>%s</b>", format_duration(completion_dt[["Duration"]]))), 
                         collapse = "<br>")
        
        showNotification(
            p(img(src = "favicon.ico", width = "16px", height = "16px"), HTML(message)),
            duration = 15,
            session = session,
            type = "message",
        )
        
        tmp[idx, IsActive := FALSE]
        exit_task_mode()
    }
    
    tmp[idx, IsComplete := chk_val]
    rv[["tasks"]] <<- copy(tmp)
}


select_task <- function(i) {
    tmp <- copy(rv[["tasks"]])
    tmp[, IsActive := 1:.N == rv[["tasks"]][IsVisible == T, which = T][i]]
    
    rv[["tasks"]] <- copy(tmp)
    enter_task_mode()
}


atomise_task <- function(i) {
    tmp <- copy(rv[["tasks"]])
    idx <- tmp[IsVisible == T, which = T][i]
    
    task_id <- tmp[IsVisible == T][i][["Id"]]
    task_name <- tmp[IsVisible == T][i][["Name"]]
    
    chat_message <- sprintf("The task the user would like to atomise: %s.  The user's other tasks today are: %s", task_name,
                            paste(rv$tasks[IsVisible == TRUE][["Name"]], collapse = "; "))
    
    response <- gpt_agent_atomise$chat(chat_message, echo = "none")
    gpt_tasks <- parse_gpt_tasks(response)
    
    tryCatch({
        tmp <- rbind(tmp, gpt_tasks)
        rv[["tasks"]] <- copy(tmp)
    }, error = function(x) {
        browser()
        print("Error binding GPT tasks")
        return(NULL)
    })
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
        
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "taskTimeFixed", value = visible_tasks[["IsFixed"]][i])
        
        task_prerequisites <- if(is.na(visible_tasks[["Prerequisites"]][i])) NULL else visible_tasks[["Prerequisites"]][i]
        prereq_split <- if(!is.null(task_prerequisites)) strsplit(task_prerequisites, ";")[[1]] else NULL
        
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites",
                          selected = as.character(id2task(rv[["tasks"]], prereq_split)))
        
        task_recurrence <- if(is.na(visible_tasks[["Recurrence"]][i])) NULL else visible_tasks[["Recurrence"]][i]
        recurrence_split <- if(!is.null(task_recurrence)) strsplit(task_recurrence, ";")[[1]] else NULL
        
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), inputId = "taskRecurrence", 
                                 selected = as.numeric(recurrence_split))
        
        updateNumericInputIcon(session = getDefaultReactiveDomain(), inputId = "taskLikelihood", value = visible_tasks[["Likelihood"]][i])
        updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskRepetitions", value = visible_tasks[["Repetitions"]][i])
        
        duration <- visible_tasks[["TaskDuration"]][i]
        updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskDuration", value = if(duration == 0) NA else duration)
        
        updateActionButton(session = getDefaultReactiveDomain(), inputId = "createTask", label = "Edit Task")
        
        shinyjs::show(id = "cancelEdit")
    } else {
        cancel_edit()
    }
}


delete_task <- function(i) {
    tmp <- copy(rv[["tasks"]])
    idx <- tmp[IsVisible == T, which = T][i]
    
    if(length(idx) == 0 || is.na(idx) || idx > nrow(tmp)) {
        return()
    }
    
    rv[["tasks"]] <<- tmp[!idx]
    cancel_edit()
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
    
    update_task_motivation()
    
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


reset_task_inputs <- function() {
    updateTextInput(session = getDefaultReactiveDomain(), inputId = "taskName", value = "")
    updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeStart", value = as.POSIXlt(sprintf("%s 00:00:00", Sys.Date())))
    updateTimeInput(session = getDefaultReactiveDomain(), inputId = "taskTimeEnd", value = as.POSIXlt(sprintf("%s 23:59:00", Sys.Date())))
    updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "taskTimeFixed", value = FALSE)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "taskPrerequisites", selected = "")
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), inputId = "taskRecurrence", selected = numeric(0))
    updateNumericInputIcon(session = getDefaultReactiveDomain(), inputId = "taskLikelihood", value = 100)
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskRepetitions", value = 1)
    updateNumericInput(session = getDefaultReactiveDomain(), inputId = "taskDuration", value = NA)
    updateActionButton(session = getDefaultReactiveDomain(), inputId = "createTask", label = "Create Task")
}

