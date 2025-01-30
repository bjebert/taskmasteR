library(shinyWidgets)
library(data.table)
library(shinyjs)
library(beepr)
library(profvis)
library(sortable)


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
    
    # Legacy interfacing ---
    if("Completions" %in% colnames(tasks_tmp)) {  
        tasks_tmp[, Completions := NULL]
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


get_prompts <- function() {
    prompt_motivation <- readLines("promptMotivation")
    prompt_enhance <- readLines("promptEnhance")
    prompt_atomise <- readLines("promptAtomise")
    prompt_estimate <- readLines("promptEstimate")
    prompt_plan <- readLines("promptPlan")
    
    if(file.exists("promptPersonal")) {
        prompt_motivation <- c(prompt_motivation, readLines("promptPersonal"))
        prompt_enhance <- c(prompt_enhance, readLines("promptPersonal"))
    }
    
    return(list("motivation" = prompt_motivation,
                "enhance" = prompt_enhance,
                "atomise" = prompt_atomise,
                "estimate" = prompt_estimate,
                "plan" = prompt_plan))
}


rv <- reactiveValues(current_date = Sys.Date(),
                     task_mode = FALSE,
                     task_timer_active = FALSE,
                     task_start_time = NULL,
                     task_duration_seconds = 0,
                     last_duration = NULL,
                     edit_mode = FALSE,
                     edit_id = NA,
                     motivational_text = NULL,
                     last_beep = Sys.time())

statistics_dt <- fread("statistics.csv")
statistics_dt[, Time := as.POSIXct(Time)]

chk_observers <- list()
sel_observers <- list()
atom_observers <- list()
edit_observers <- list()
del_observers <- list()


prompts <- get_prompts()
launch_time <- Sys.time()

# Helpers -----------------------------------------------------------------

breaks <- function(n) tagList(lapply(1:n, function(x) br()))

load_habits <- function() {
    tasks_tmp <- copy(rv[["tasks"]])
    hab_dt <- tasks_tmp[Recurrence != ""]
    tasks_tmp[Recurrence != "", IsVisible := Likelihood >= runif(nrow(hab_dt), min = 0, max = 100)]
    tasks_tmp[Recurrence != "", IsVisible := IsVisible & sapply(strsplit(as.character(hab_dt[["Recurrence"]]), ";"), function(x) (wday(Sys.Date()) - 1) %in% as.numeric(x))]
    tasks_tmp[Recurrence != "", IsComplete := FALSE]
    tasks_tmp[IsComplete == TRUE, IsActive := FALSE]
    
    rv[["tasks"]] <<- copy(tasks_tmp)
}


determine_task_label_colour <- function(time_start, time_end, prerequisites, is_fixed, is_complete, is_active) {
    active_colour <- "red"
    complete_colour <- "palegreen"
    inactive_colour <- "lightgray"
    fixed_colour <- "lightblue"
    default_colour <- "#444444"
    
    if(!is.na(is_active) && is_active) {
        return(active_colour)
    }
    
    if(is_fixed) {
        return(fixed_colour)
    }
    
    if(is_complete) {
        return(complete_colour)
    }
    
    is_current_time_in_task_window <- as.POSIXlt(sprintf("%s %s", Sys.Date(), time_start)) < Sys.time() &&
        Sys.time() < as.POSIXlt(sprintf("%s %s", Sys.Date(), time_end))
    
    # Grey out if not currently doable (because of time or prereqs)
    if((all(!is.na(prerequisites)) && length(prerequisites)) || !is_current_time_in_task_window) {
        return(inactive_colour)
    }
    
    return(default_colour)
}


get_tasks_ui <- function() {
    visible_tasks <- rv[["tasks"]][IsVisible == TRUE]
    if(nrow(visible_tasks) == 0) {
        return(absolutePanel("No tasks have been created yet..."))
    }
    
    create_ui_row <- function(top, id, label, time_start, time_end, prerequisites, is_fixed, is_complete, is_active,
                              plan_start, plan_end) {
        
        label_full <- copy(label)
        label_colour <- determine_task_label_colour(time_start, time_end, prerequisites, is_fixed, is_complete, is_active)
        
        nchar_size_map <- c(`0` = 18, `40` = 17, `50` = 16, `60` = 14, `70` = 12)
        label_length <- nchar(label) + nchar(paste(prerequisites, collapse = ", "))
        
        has_time <- (!(time_start %in% c("0:00", "00:00")) || time_end != "23:59") && time_start < time_end    
        has_plan <- !is.na(plan_start) && !is.na(plan_end) && plan_start != "" && plan_end != "" &&
            (!(plan_start %in% c("0:00", "00:00")) || plan_end != "23:59" && plan_start < plan_end) 
        
        if(has_time) {
            complete_colour <- "palegreen"
            inactive_colour <- "lightgray"
            fixed_colour <- "lightblue"
            
            time_colour <- ifelse(is_fixed, fixed_colour, ifelse(is_complete, complete_colour, inactive_colour))
            
            if(!(is_fixed && has_plan)) {
                label_full <- HTML(sprintf("%s <span style=color:%s><i>(%s - %s)</i></span>", label_full, time_colour, time_start, time_end))
                label_length <- label_length + 15
            }
        }
        
        if(has_plan) {
            complete_colour <- "palegreen"
            plan_time_colour <- "purple"
            
            plan_colour <- ifelse(is_complete, complete_colour, plan_time_colour)
            
            label_full <- HTML(sprintf("<span style=color:%s>(%s - %s):</span> %s", plan_colour, plan_start, plan_end, label_full))
            label_length <- label_length + 15
        }
        
        w_sz <- max(which(label_length > as.numeric(names(nchar_size_map))))
        font_size <- nchar_size_map[w_sz]
        
        if(all(!is.na(prerequisites)) && length(prerequisites)) {
            prereq_joined <- paste(prerequisites, collapse = ", ")
            
            MAX_PREREQ_CHAR_LEN <- 75
            
            if((nchar(prereq_joined) + nchar(label_full)) > MAX_PREREQ_CHAR_LEN) {
                prereq_joined <- sprintf("%s...", substr(prereq_joined, 1, MAX_PREREQ_CHAR_LEN - nchar(label_full)))
            }
            
            label_full <- HTML(sprintf("%s <i>(Req: %s)</i>", label_full, prereq_joined))
        }
        
        label_style <- sprintf('font-size: %spx; color: %s', font_size, label_colour)
        
        if(!is.na(is_active) && is_active) {
            label_style <- sprintf('%s; font-weight: bold', label_style)
        } else if(is_complete) {
            label_style <- sprintf('%s; text-decoration: line-through', label_style)
        }
        
        p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id), style = label_style)
        
        return(
            fluidRow(class = "task-row",
                     absolutePanel(left = "2000px", sprintf("[%s]", id)),  # ID
                     absolutePanel(left = "20px", checkboxInput(sprintf("chk%d", id), label = p_label, value = is_complete, width = "450px")),
                     absolutePanel(left = "465px", actionButton(sprintf("sel%d", id), "", icon = icon("location-arrow"))),
                     absolutePanel(left = "506px", actionButton(sprintf("atom%d", id), "", icon = icon("cubes"))),
                     absolutePanel(left = "551px", actionButton(sprintf("edit%d", id), "", icon = icon("edit"))),
                     absolutePanel(left = "594px", actionButton(sprintf("del%d", id), "", icon = icon("trash"))),
            )
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
                      visible_tasks[["IsFixed"]][i],
                      visible_tasks[["IsComplete"]][i],
                      active_status,
                      visible_tasks[["PlanStart"]][i],
                      visible_tasks[["PlanEnd"]][i])
    })
    
    rank_list(text = NULL,
              input_id = "taskSortableList", 
              labels = tasks_ui)
}


# get_tasks_ui_old <- function() {
#     visible_tasks <- rv[["tasks"]][IsVisible == TRUE]
#     if(nrow(visible_tasks) == 0) {
#         return(absolutePanel("No tasks have been created yet..."))
#     }
#     
#     create_ui_row <- function(top, id, label, time_start, time_end, prerequisites, is_fixed, is_complete, is_active,
#                               plan_start, plan_end) {
#         
#         label_full <- copy(label)
#         label_colour <- determine_task_label_colour(time_start, time_end, prerequisites, is_fixed, is_complete, is_active)
#         
#         nchar_size_map <- c(`0` = 18, `40` = 17, `50` = 16, `60` = 14, `70` = 12)
#         label_length <- nchar(label) + nchar(paste(prerequisites, collapse = ", "))
# 
#         has_time <- (!(time_start %in% c("0:00", "00:00")) || time_end != "23:59") && time_start < time_end    
#         has_plan <- !is.na(plan_start) && !is.na(plan_end) && plan_start != "" && plan_end != "" &&
#             (!(plan_start %in% c("0:00", "00:00")) || plan_end != "23:59" && plan_start < plan_end) 
#            
#         if(has_time) {
#             complete_colour <- "palegreen"
#             inactive_colour <- "lightgray"
#             fixed_colour <- "lightblue"
#             
#             time_colour <- ifelse(is_fixed, fixed_colour, ifelse(is_complete, complete_colour, inactive_colour))
#             
#             if(!(is_fixed && has_plan)) {
#                 label_full <- HTML(sprintf("%s <span style=color:%s><i>(%s - %s)</i></span>", label_full, time_colour, time_start, time_end))
#                 label_length <- label_length + 15
#             }
#         }
#         
#         if(has_plan) {
#             complete_colour <- "palegreen"
#             plan_time_colour <- "purple"
#             
#             plan_colour <- ifelse(is_complete, complete_colour, plan_time_colour)
#             
#             label_full <- HTML(sprintf("<span style=color:%s>(%s - %s):</span> %s", plan_colour, plan_start, plan_end, label_full))
#             label_length <- label_length + 15
#         }
#         
#         w_sz <- max(which(label_length > as.numeric(names(nchar_size_map))))
#         font_size <- nchar_size_map[w_sz]
#         
#         if(all(!is.na(prerequisites)) && length(prerequisites)) {
#             prereq_joined <- paste(prerequisites, collapse = ", ")
#             
#             MAX_PREREQ_CHAR_LEN <- 75
#             
#             if((nchar(prereq_joined) + nchar(label_full)) > MAX_PREREQ_CHAR_LEN) {
#                 prereq_joined <- sprintf("%s...", substr(prereq_joined, 1, MAX_PREREQ_CHAR_LEN - nchar(label_full)))
#             }
#             
#             label_full <- HTML(sprintf("%s <i>(Req: %s)</i>", label_full, prereq_joined))
#         }
#         
#         label_style <- sprintf('font-size: %spx; color: %s', font_size, label_colour)
#         
#         if(!is.na(is_active) && is_active) {
#             label_style <- sprintf('%s; font-weight: bold', label_style)
#         } else if(is_complete) {
#             label_style <- sprintf('%s; text-decoration: line-through', label_style)
#         }
#         
#         p_label <- tags$p(label_full, id = sprintf("chkLabel%d", id), style = label_style)
#         
#         fluidRow(
#             absolutePanel(left = "20px", top = sprintf("%dpx", top), checkboxInput(sprintf("chk%d", id), label = p_label, value = is_complete, width = "450px")),
#             absolutePanel(left = "465px", top = sprintf("%dpx", top), actionButton(sprintf("sel%d", id), "", icon = icon("location-arrow"))),
#             absolutePanel(left = "506px", top = sprintf("%dpx", top), actionButton(sprintf("atom%d", id), "", icon = icon("cubes"))),
#             absolutePanel(left = "551px", top = sprintf("%dpx", top), actionButton(sprintf("edit%d", id), "", icon = icon("edit"))),
#             absolutePanel(left = "594px", top = sprintf("%dpx", top), actionButton(sprintf("del%d", id), "", icon = icon("trash"))),
#         )
#     }
#     
#     tasks_ui <- lapply(1:nrow(visible_tasks), function(i) {
#         active_status <- if(!any(visible_tasks[["IsActive"]])) NA else visible_tasks[["IsActive"]][i]
#         
#         if(!is.na(visible_tasks[["Prerequisites"]][i])) {
#             prerequisites <- strsplit(visible_tasks[["Prerequisites"]][i], ";")[[1]]
#             prerequisites <- prerequisites[!(prerequisites %in% visible_tasks[IsVisible == F | IsComplete == T, Name])]
#         } else {
#             prerequisites <- NA
#         }
#         
#         create_ui_row(i*40 - 25, i, 
#                       visible_tasks[["Name"]][i], 
#                       visible_tasks[["TimeStart"]][i],
#                       visible_tasks[["TimeEnd"]][i],
#                       prerequisites,
#                       visible_tasks[["IsFixed"]][i],
#                       visible_tasks[["IsComplete"]][i],
#                       active_status,
#                       visible_tasks[["PlanStart"]][i],
#                       visible_tasks[["PlanEnd"]][i])
#     })
#     
#     return(tasks_ui)
# }


task_status_ui <- function() {
    active_task_row <- rv[["tasks"]][IsActive == TRUE]
    
    if(nrow(active_task_row) == 0) {
        exit_task_mode()
    }
    
    task_statistics <- get_task_statistics(active_task_row[["Name"]])
    
    status_ui <- fluidRow(
        absolutePanel(top = "10px", left = "55px", h5(textOutput("formattedTaskTime"))),
        absolutePanel(top = "24px", left = "10px", actionButton("toggleTaskTimer", label = NULL, icon = icon(ifelse(rv[["task_timer_active"]], "pause", "play")))),
        absolutePanel(top = "10px", left = "530px", actionButton("cancelTask", label = "Cancel", width = "100px", icon = icon("cancel"))),
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
                        task_likelihood, task_time_fixed, task_duration) {
    
    start_hhmm <- sprintf("%02d:%02d", hour(task_time_start), minute(task_time_start))
    end_hhmm <- sprintf("%02d:%02d", hour(task_time_end), minute(task_time_end))
    
    if(is.null(task_prerequisites)) {
        task_prerequisites <- ""
    } else {
        task_prerequisites <- paste(task_prerequisites, collapse = ";")
    }
    
    task_duration <- if(is.na(task_duration)) 0 else task_duration
    
    if(task_time_fixed) {
        task_duration <- as.numeric(difftime(as.POSIXct(task_time_end, format = "%H:%M"),
                                             as.POSIXct(task_time_start, format = "%H:%M"),
                                             units = "mins"))
    }
    
    task_dt <- data.table(Name = task_name, TimeStart = start_hhmm, TimeEnd = end_hhmm,
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
    }
    
    tmp[idx, IsComplete := chk_val]
    tmp[idx, IsActive := FALSE]
    rv[["tasks"]] <<- copy(tmp)
    
    if(!chk_val) {
        exit_task_mode()
    }
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
                          selected = prereq_split)
        
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


select_task <- function(i) {
    tmp <- copy(rv[["tasks"]])
    tmp[, IsActive := 1:.N == rv[["tasks"]][IsVisible == T, which = T][i]]
    
    rv[["tasks"]] <- copy(tmp)
    enter_task_mode()
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
    
    output$dateTime <- renderText({
        format(rv[["current_date"]], format = "%A, %d %B %Y")
    })
    
    observeEvent(input$datePrev, {
        rv[["current_date"]] <- rv[["current_date"]] - 1
    })
    
    observeEvent(input$dateNext, {
        rv[["current_date"]] <- rv[["current_date"]] + 1
    })
    
    observeEvent(rv[["current_date"]], {
        rv[["tasks"]] <- read_tasks(sprintf("history/tasks_%s.csv", gsub("-", "_", rv[["current_date"]])))
    })
    
    output$taskUi <- renderUI({
        ui <- get_tasks_ui()
        
        if("children" %in% names(ui)) {
            N <- 0
        } else {
            N <- length(ui[[1]][["children"]][[2]][["children"]][[1]])
        }
        
        if(N > length(chk_observers)) {
            i <- length(chk_observers) + 1
            chk_observers <<- c(chk_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("chk%d", i)]], toggle_check_task(i, input[[sprintf("chk%d", i)]], session))))
            sel_observers <<- c(sel_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("sel%d", i)]], select_task(i))))
            atom_observers <<- c(atom_observers, lapply(i:N, function(i) observeEvent(input[[sprintf("atom%d", i)]], atomise_task(i))))
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
        
        fwrite(rv[["tasks"]], sprintf("history/tasks_%s.csv", gsub("-", "_", rv[["current_date"]])))
        fwrite(rv[["tasks"]][Recurrence != ""], "habits.csv")
    })
    
    observeEvent(input$taskSortableList, {
        if(length(input$taskSortableList) == 0) {
            return()
        }
        
        id <- as.numeric(stringr::str_extract(input$taskSortableList, pattern = "\\d+"))
        rv[["tasks"]] <- rv[["tasks"]][id]
    })
    
    observeEvent(input$createTask, {
        create_task(input$taskName, input$taskTimeStart, input$taskTimeEnd, input$taskPrerequisites, input$taskRecurrence, 
                    input$taskLikelihood, input$taskTimeFixed, input$taskDuration)
        
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
    
    observeEvent(input$user_clicked, {  # Ding!
        if(nchar(input$user_clicked) >= 3) {
            if(substr(input$user_clicked, 1, 3) == "chk") {
                if(!isolate(input[[input$user_clicked]])) {
                    runjs("var audio = new Audio('ding.mp3'); audio.volume = 0.7; audio.play();")
                }
            }    
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
    
    
    # GPT Settings ------------------------------------------------------------
    
    observeEvent(input$gptVersion, {
        gpt_agent_motivation <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["motivation"]])
        gpt_agent_enhance <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["enhance"]])
        gpt_agent_atomise <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["atomise"]])
        gpt_agent_estimate <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["estimate"]])
        gpt_agent_plan <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["plan"]])
    })
    
    
    parse_gpt_tasks <- function(response, sample_amount = NULL) {
        gpt_tasks <- tryCatch({
            new_tasks <- strsplit(response, "; ")[[1]]
            new_task_dt <- rbindlist(lapply(new_tasks, function(task) {
                start_hhmm <- "00:00"
                end_hhmm <- "23:59"
                
                extracted_time <- stringr::str_extract(task, "(?<=\\()[0-9]{2}:[0-9]{2}-[0-9]{2}:[0-9]{2}(?=\\)$)")
                task <- gsub('"', "'", task)
                
                if(!is.na(extracted_time)) {
                    times <- strsplit(extracted_time, "-")[[1]]
                    start_hhmm <- times[1]
                    end_hhmm <- times[2]
                    
                    if(start_hhmm == end_hhmm) {
                        start_hhmm <- "00:00"
                        end_hhmm <- "23:59"
                    }
                    
                    task <- gsub(sprintf("\\(%s\\)", extracted_time), "", task)
                }
                
                task <- sub("\\.+$", "", task)  # Strip trailing periods
                task <- sub("\\s+$", "", task)  # Strip trailing whitespace
                
                return(data.table(Name = task, TimeStart = start_hhmm, TimeEnd = end_hhmm,
                                  Prerequisites = NA, Recurrence = "", 
                                  Likelihood = 100, IsComplete = FALSE, IsActive = FALSE,
                                  IsVisible = TRUE, IsFixed = FALSE, TaskDuration = 0,
                                  PlanStart = NA, PlanEnd = NA))
            }))
            
            new_task_dt <- new_task_dt[!is.na(Name) & Name != ""]
            
            if(!is.null(sample_amount)) {
                new_task_dt <- new_task_dt[sample(1:nrow(new_task_dt), sample_amount)]
            }
            
            return(new_task_dt)
        }, error = function(x) {
            print(sprintf("Error in response: %s", response))
            return(NULL)
        })
        
        return(gpt_tasks)
    }
    
    # GPT Atomise -------------------------------------------------------------
    
    atomise_task <- function(i) {
        tmp <- copy(rv[["tasks"]])
        idx <- tmp[IsVisible == T, which = T][i]
        
        task_name <- tmp[IsVisible == T][i][["Name"]]
        
        chat_message <- sprintf("The task the user would like to atomise: %s.  The user's other tasks today are: %s", task_name,
                                paste(rv$tasks[IsVisible == TRUE][["Name"]], collapse = "; "))
        
        response <- gpt_agent_atomise$chat(chat_message, echo = "none")
        gpt_tasks <- parse_gpt_tasks(response)
        
        tryCatch({
            tmp <- rbind(tmp, gpt_tasks)
            tmp <- tmp[-idx]
            rv[["tasks"]] <- copy(tmp)
        }, error = function(x) {
            print("Error binding GPT tasks")
            return(NULL)
        })
    }
    
    # GPT Enhance -------------------------------------------------------------
    
    observeEvent(input$gptEnhance, {
        size_message <- c("tiny" = "Provide small bite-sized tasks, that will help the user getting started in a positive direction.",
                          "normal" = "Provide normal-sized tasks.",
                          "big" = "Provide reasonably challenging tasks.",
                          "huge" = "Provide difficult tasks.  The user wants something hard to push them outside their comfort zone.",
                          "absurd" = "Provide ridiculously difficult tasks.  The user should look at the tasks and realise they will need to commit an insane amount of effort to complete one of them.  The tasks should still be physically completable by the user TODAY; so don't choose stuff the user will literally find impossible to do.  Use some creativity.")
        
        enhance_context <- if(!is.null(input$gptEnhanceContext)) sprintf("IMPORTANT - the user has provided this context: %s.", 
                                                                         input$gptEnhanceContext) else NULL
        
        chat_message <- sprintf("Current time is %s.  The user's tasks today are: %s.  %s.  %s",
                                format(Sys.time(), "%A %H:%M:%S"),
                                paste(rv$tasks[IsVisible == TRUE][["Name"]], collapse = "; "),
                                size_message[[input$gptEnhanceSize]],
                                enhance_context)
        
        response <- gpt_agent_enhance$chat(chat_message, echo = "none")
        gpt_tasks <- parse_gpt_tasks(response, input$gptEnhanceAmount)
        
        tryCatch({
            rv[["tasks"]] <- rbind(rv[["tasks"]], gpt_tasks)
        }, error = function(x) {
            print("Error binding GPT tasks")
            return(NULL)
        })
    })
    
    
    # GPT planner -------------------------------------------------------------
    
    get_tasks_with_fixed_times <- function(tasks) {
        tasks_durations <- tasks[, sprintf("%s%s", Name, ifelse(IsFixed, sprintf(" (%s - %s)", TimeStart, TimeEnd), ""))]
        paste(sample(tasks_durations), collapse = "; ")
    }
    
    get_tasks_with_times_and_durations <- function(tasks) {
        tasks_durations <- tasks[, sprintf("%s%s [%s minutes]", Name, ifelse(!(TimeStart %in% c("0:00", "00:00")) | TimeEnd != "23:59", sprintf(" (%s - %s)", TimeStart, TimeEnd), ""),
                                           TaskDuration)]
        paste(sample(tasks_durations), collapse = "; ")
    }
    
    parse_durations <- function(response) {
        estimates <- strsplit(response, ";")[[1]]
        durations <- as.numeric(stringr::str_extract(stringr::str_extract(estimates, "\\[.*\\]"), "\\d+"))
        return(durations)
    }
    
    parse_plan <- function(response, tasks_dt) {
        plan_split <- strsplit(response, ";")[[1]]
        
        plan_times <- gsub("\\{|\\}", "", stringr::str_extract(plan_split, "\\{.*\\}"))
        plan_list <- strsplit(plan_times, "-")
        
        plan_dt <- data.table(Name = stringr::str_trim(sapply(strsplit(plan_split, "\\{"), function(x) x[1])),
                              PlanStart = sapply(plan_list, function(x) x[1]),
                              PlanEnd = sapply(plan_list, function(x) x[2]))
        
        plan_dt[, I := 1:.N, by = Name]
        tasks_dt[, I := 1:.N, by = Name]
        
        return(merge(tasks_dt[, -c("PlanStart", "PlanEnd")], plan_dt, by = c("Name", "I"), all.x = T, sort = F)[, -"I"])
    }
    
    observeEvent(input$gptPlanDay, {
        # Estimate duration ---
        
        w_est <- rv$tasks[IsVisible & !IsComplete & !IsFixed & TaskDuration == 0, which = T]
        
        est_message <- sprintf("The user's tasks today are: %s",
                               get_tasks_with_fixed_times(rv$tasks[w_est]))
        
        response <- gpt_agent_estimate$chat(est_message, echo = "none")
        durations <- tryCatch(parse_durations(response),
                              error = function(x) {
                                  print(x)
                                  return(rep(NA, length(w_plan)))
                              })
        
        rv$tasks[w_est, TaskDuration := durations]
        
        # Create plan ---
        
        w_plan <- rv$tasks[IsVisible & !IsComplete & TaskDuration != 0, which = T]
        
        plan_message <- sprintf("The current time is: %s.  The user's tasks today are: %s",
                                format(Sys.time(), "%A %H:%M:%S"),
                                get_tasks_with_times_and_durations(rv$tasks[w_plan]))
        response <- gpt_agent_plan$chat(plan_message, echo = "none")
        
        tasks_dt <- parse_plan(response, rv$tasks[w_plan])
        tmp <- copy(rv$tasks)
        
        fail_counter <- 0
        while(nrow(tasks_dt[is.na(PlanEnd)]) > 0 && fail_counter < 3) {
            print(sprintf('Plan failed: %d', fail_counter + 1))
            
            w_plan2 <- tasks_dt[is.na(PlanEnd), which = T]
            
            existing_plan <- tasks_dt[!is.na(PlanStart)][order(PlanStart)][, sprintf("%s {%s-%s}", Name, PlanStart, PlanEnd)]
            
            plan_message <- sprintf("Error: you MUST include all tasks in the plan!  The current time is: %s. You forgot to include these task{s}: %s. The current plan is: %s.",
                                    format(Sys.time(), "%A %H:%M:%S"),
                                    get_tasks_with_times_and_durations(tasks_dt[w_plan2]),
                                    paste(existing_plan, collapse = "; "))
            
            response <- gpt_agent_plan$chat(plan_message, echo = "none")
            
            tasks_dt <- parse_plan(response, rv$tasks[w_plan])
            tmp <- copy(rv$tasks)
            
            fail_counter <- fail_counter + 1
        }
        
        tmp[, c("PlanStart", "PlanEnd") := NULL]
        tmp[, PlanStart := ""]
        tmp[, PlanEnd := ""]
        tmp[w_plan] <- tasks_dt
        tmp <- tmp[order(-IsComplete, as.POSIXct(PlanStart, format = "%H:%M"))]
        
        rv$tasks <- copy(tmp)
    })
    
    observeEvent(input$gptClearPlan, {
        tmp <- copy(rv[["tasks"]])
        tmp[, PlanStart := as.character(NA)]
        tmp[, PlanEnd := as.character(NA)]
        rv[["tasks"]] <- copy(tmp)
    })
    
    # GPT motivational panel ------------------------------------------------------
    
    output$gptWellPanel <- renderUI({
        if(!is.null(rv[["motivational_text"]]) && nchar(rv[["motivational_text"]]) > 0 && input$isGptMotivational) {
            motivational_class <- "active-gpt-bg" 
        } else {
            motivational_class <- "inactive-gpt-bg"
        }
        
        wellPanel(
            class = motivational_class,
            htmlOutput("gptOutput"),
            breaks(2)
        )
    })
    
    observeEvent(c(rv[["task_mode"]], rv[["tasks"]], input$isGptMotivational), {
        if(!rv[["task_mode"]] || !input$isGptMotivational) {
            rv[["motivational_text"]] <- ""
            return(NULL)
        }
        
        # Generate a GPT motivational output for the current task
        current_task <- rv$tasks[IsActive == TRUE]
        other_tasks <- paste(rv$tasks[IsActive == FALSE & IsVisible == TRUE & IsComplete == FALSE][["Name"]], collapse = "; ")
        
        visible_tasks <- rv$tasks[IsVisible == TRUE]
        
        if(nrow(current_task) == 0) {
            rv[["motivational_text"]] <- ""
            return(NULL)
        }
        
        chat_message <- sprintf("Current time is %s.  The current task is: %s.  The user's other tasks today are: %s.  The user has completed %d/%d tasks today.",
                                format(Sys.time(), "%A %H:%M:%S"),
                                current_task[["Name"]],
                                other_tasks,
                                visible_tasks[, sum(IsComplete)],
                                nrow(visible_tasks))
        
        response <- gpt_agent_motivation$chat(chat_message, echo = "none")
        rv[["motivational_text"]] <- response
    })
    
    output$gptOutput <- renderText(rv$motivational_text)
    
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
