
# ui_helpers: functions to create UI components ---------------------------


# Status UI ---------------------------------------------------------------

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

# Tasks UI ----------------------------------------------------------------


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


create_ui_row <- function(order_id, top_px, left_px, label, time_start, time_end, prerequisites, is_fixed, is_complete, is_active,
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
    
    p_label <- tags$p(label_full, id = sprintf("chkLabel%d", order_id), style = label_style)
    
    return(
        fluidRow(class = "task-row",
                 absolutePanel(left = "2000px", sprintf("[%s]", order_id)),  # numeric ordered ID (separate to task ID)
                 absolutePanel(left = "20px", checkboxInput(sprintf("chk%d", order_id), label = p_label, value = is_complete, width = "440px")),
                 absolutePanel(left = "452px", actionButton(sprintf("sel%d", order_id), "", icon = icon("location-arrow"))),
                 absolutePanel(left = "493px", actionButton(sprintf("atom%d", order_id), "", icon = icon("cubes"))),
                 absolutePanel(left = "538px", actionButton(sprintf("edit%d", order_id), "", icon = icon("edit"))),
                 absolutePanel(left = "581px", actionButton(sprintf("del%d", order_id), "", icon = icon("trash"))),
        )
    )
}


get_tasks_ui <- function() {
    root_tasks <- copy(rv[["tasks"]][IsVisible == TRUE])
    if(nrow(root_tasks) == 0) {
        return(tagList(list(absolutePanel(left = "10px", top = "10px", width = "80%", "No tasks have been created yet..."),
                            breaks(2))))
    }
    
    tasks_ui <- lapply(1:nrow(root_tasks), function(i) {
        active_status <- if(!any(root_tasks[["IsActive"]])) NA else root_tasks[["IsActive"]][i]
        
        if(!is.na(root_tasks[["Prerequisites"]][i])) {
            prerequisites <- strsplit(root_tasks[["Prerequisites"]][i], ";")[[1]]
            prerequisites <- id2task(rv[["tasks"]], prerequisites)
            prerequisites <- prerequisites[!(prerequisites %in% root_tasks[IsVisible == F | IsComplete == T, Name])]
        } else {
            prerequisites <- NA
        }
        
        create_ui_row(i, 
                      i*40 - 25,
                      0,
                      root_tasks[["Name"]][i], 
                      root_tasks[["TimeStart"]][i],
                      root_tasks[["TimeEnd"]][i],
                      prerequisites,
                      root_tasks[["IsFixed"]][i],
                      root_tasks[["IsComplete"]][i],
                      active_status,
                      root_tasks[["PlanStart"]][i],
                      root_tasks[["PlanEnd"]][i])
    })
    
    rank_list(text = NULL,
              input_id = "taskSortableList", 
              labels = tasks_ui)
}
