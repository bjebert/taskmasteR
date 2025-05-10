
# gpt: functions for interacting with GPT ---------------------------------

get_prompts <- function() {
    prompt_motivation <- readLines("prompts/promptMotivation")
    prompt_enhance <- readLines("prompts/promptEnhance")
    prompt_atomise <- readLines("prompts/promptAtomise")
    prompt_estimate <- readLines("prompts/promptEstimate")
    prompt_plan <- readLines("prompts/promptPlan")
    prompt_plan_ordered <- readLines("prompts/promptPlanOrdered")
    
    if (file.exists("prompts/promptPersonal")) {
        prompt_motivation <- c(prompt_motivation, readLines("prompts/promptPersonal"))
        prompt_enhance <- c(prompt_enhance, readLines("prompts/promptPersonal"))
    }
    
    return(list(
        motivation = prompt_motivation,
        enhance = prompt_enhance,
        atomise = prompt_atomise,
        estimate = prompt_estimate,
        plan = prompt_plan,
        plan_ordered = prompt_plan_ordered
    ))
}


prompts <- get_prompts()



# Motivation --------------------------------------------------------------

update_task_motivation <- function() {
    if(!rv[["task_mode"]] || !getDefaultReactiveDomain()$input$isGptMotivational) {
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
}


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
            
            return(data.table(Id = create_task_id(), Name = task, TimeStart = start_hhmm, TimeEnd = end_hhmm,
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


# Enhance -----------------------------------------------------------------

gpt_enhance_tasks <- function(tasks_dt, enhance_context, enhance_size, enhance_amount) {
    
    size_message <- c("tiny" = "Provide small bite-sized tasks, that will help the user getting started in a positive direction.",
                      "normal" = "Provide normal-sized tasks.",
                      "big" = "Provide reasonably challenging tasks.",
                      "huge" = "Provide difficult tasks.  The user wants something hard to push them outside their comfort zone.",
                      "absurd" = "Provide ridiculously difficult tasks.  The user should look at the tasks and realise they will need to commit an insane amount of effort to complete one of them.  The tasks should still be physically completable by the user TODAY; so don't choose stuff the user will literally find impossible to do.  Use some creativity.")
    
    enhance_context <- if(!is.null(enhance_context)) sprintf("IMPORTANT - the user has provided this context: %s.", 
                                                             enhance_context) else NULL
    
    chat_message <- sprintf("Current time is %s.  The user's tasks today are: %s.  %s.  %s",
                            format(Sys.time(), "%A %H:%M:%S"),
                            paste(rv$tasks[IsVisible == TRUE][["Name"]], collapse = "; "),
                            size_message[[enhance_size]],
                            enhance_context)
    
    response <- gpt_agent_enhance$chat(chat_message, echo = "none")
    gpt_tasks <- parse_gpt_tasks(response, enhance_amount)
    
    res <- tryCatch({
        return(rbind(tasks_dt, gpt_tasks))
    }, error = function(x) {
        print("Error binding GPT tasks")
        return(tasks_dt)
    })
    
    return(res)
}


# GPT Planning ------------------------------------------------------------


get_tasks_with_fixed_times <- function(tasks) {
    tasks_durations <- tasks[, sprintf("%s%s", Name, ifelse(IsFixed, sprintf(" (%s - %s)", TimeStart, TimeEnd), ""))]
    paste(tasks_durations, collapse = "; ")
}


get_tasks_with_times_and_durations <- function(tasks) {
    tasks_durations <- tasks[, sprintf("%s%s [%s minutes]", Name, ifelse(!(TimeStart %in% c("0:00", "00:00")) | TimeEnd != "23:59", sprintf(" (%s - %s)", TimeStart, TimeEnd), ""),
                                       TaskDuration)]
    paste(tasks_durations, collapse = "; ")
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
    
    plan_dt <- data.table(NameUpper = toupper(stringr::str_trim(sapply(strsplit(plan_split, "\\{"), function(x) x[1]))),
                          PlanStart = sapply(plan_list, function(x) x[1]),
                          PlanEnd = sapply(plan_list, function(x) x[2]))
    
    plan_dt[, I := 1:.N, by = NameUpper]
    tasks_dt[, I := 1:.N, by = Name]
    
    tasks_dt[, NameUpper := toupper(Name)]
    
    merge_dt <- merge(tasks_dt[, -c("PlanStart", "PlanEnd")], plan_dt, by = c("NameUpper", "I"), all.x = T, sort = F)[, -"I"]
    return(merge_dt[, -"NameUpper"])
}