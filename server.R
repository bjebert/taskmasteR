
source("R/global.R")
source("R/helpers.R")
source("R/ui_helpers.R")
source("R/tasks.R")
source("R/gpt.R")


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
        
        return(fluidRow(ui))
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
    })
    
    observeEvent(input$taskSortableList, {
        if(length(input$taskSortableList) == 0) {
            return()
        }
        
        id <- as.numeric(stringr::str_extract(input$taskSortableList, pattern = "\\d+"))
        rv[["tasks"]] <- rv[["tasks"]][id]  # temporarily commented out while children are being sorted out
    })
    
    observeEvent(input$createTask, {
        create_task(input$taskName, input$taskTimeStart, input$taskTimeEnd, input$taskPrerequisites,
                    input$taskRecurrence, input$taskLikelihood, input$taskTimeFixed, input$taskDuration)
        
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
    
    observeEvent(input$clearTasks, {
        rv[["tasks"]] <<- rv[["tasks"]][Recurrence != ""]
        rv[["tasks"]][, IsVisible := FALSE]
    })    
    
    observeEvent(input$loadHistorical, {
        rv[["tasks"]] <- read_tasks(input$loadHistorical[["datapath"]])
    })
    
    
    # GPT Settings ------------------------------------------------------------
    
    observeEvent(input$gptVersion, {
        gpt_agent_motivation <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["motivation"]])
        gpt_agent_enhance <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["enhance"]])
        gpt_agent_atomise <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["atomise"]])
        gpt_agent_estimate <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["estimate"]])
        gpt_agent_plan <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["plan"]])
        gpt_agent_plan_ordered <<- ellmer::chat_openai(model = input$gptVersion, system_prompt = prompts[["plan_ordered"]])
    })
    
    # GPT Enhance -------------------------------------------------------------
    
    observeEvent(input$gptEnhance, {
        rv[["tasks"]] <- gpt_enhance_tasks(rv[["tasks"]], input$gptEnhanceContext, input$gptEnhanceSize,
                                           input$gptEnhanceAmount)
    })
    
    # GPT planner -------------------------------------------------------------
    
    observeEvent(input$gptPlanDay, {
        # Estimate duration ---
        
        w_est <- rv$tasks[IsVisible & !IsComplete & !IsFixed & (is.na(TaskDuration) | TaskDuration == 0), which = T]
        
        if(length(w_est) > 0) {
            est_message <- sprintf("The user's tasks today are: %s",
                                   get_tasks_with_fixed_times(rv$tasks[w_est]))
            
            response <- gpt_agent_estimate$chat(est_message, echo = "none")
            durations <- tryCatch(parse_durations(response),
                                  error = function(x) {
                                      print(x)
                                      return(rep(NA, length(w_plan)))
                                  })
            
            rv$tasks[w_est, TaskDuration := durations]
        }
        
        # Create plan ---
        
        w_plan <- rv$tasks[IsVisible & !IsComplete & TaskDuration != 0, which = T]
        
        plan_message <- sprintf("The current time is: %s.  The user's tasks today are: %s",
                                format(Sys.time(), "%A %H:%M:%S"),
                                get_tasks_with_times_and_durations(rv$tasks[w_plan]))
        
        agent <- if(input$gptPlanIsOrdered) gpt_agent_plan_ordered else gpt_agent_plan
        response <- agent$chat(plan_message, echo = "none")
        
        tasks_dt <- parse_plan(response, rv$tasks[w_plan])
        
        fail_counter <- 0
        while(nrow(tasks_dt[is.na(PlanEnd)]) > 0 && fail_counter < 3) {
            fail_message <- sprintf('Plan failed: %d (%s)', fail_counter + 1, paste(tasks_dt[is.na(PlanEnd), Name], collapse = ";"))
            print(fail_message)
            
            showNotification(
                fail_message,
                duration = 5,
                session = getDefaultReactiveDomain(),
                type = "error",
            )
            
            w_plan2 <- tasks_dt[is.na(PlanEnd), which = T]
            
            existing_plan <- tasks_dt[!is.na(PlanStart)][order(PlanStart)][, sprintf("%s {%s-%s}", Name, PlanStart, PlanEnd)]
            
            plan_message <- sprintf("Error: you MUST include all tasks in the plan!  The current time is: %s. You forgot to include these task{s}: %s. The current plan is: %s.",
                                    format(Sys.time(), "%A %H:%M:%S"),
                                    get_tasks_with_times_and_durations(tasks_dt[w_plan2]),
                                    paste(existing_plan, collapse = "; "))
            
            response <- agent$chat(plan_message, echo = "none")
            
            tasks_dt <- parse_plan(response, rv$tasks[w_plan])
            
            fail_counter <- fail_counter + 1
        }
        
        if(input$gptPlanIsContiguous) {
            # Make times contiguous
            start_mins <- cumsum(c(time2mins(tasks_dt[["PlanStart"]])[1], tasks_dt[["TaskDuration"]])) %% (24 * 60)
            start_mins <- start_mins[1:(length(start_mins) - 1)]
            
            end_mins <- (start_mins + tasks_dt[["TaskDuration"]]) %% (24 * 60)
            
            tasks_dt[, PlanStart := mins2time(start_mins)]
            tasks_dt[, PlanEnd := mins2time(end_mins)]
        }
        
        tmp <- copy(rv$tasks)
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
    
    observeEvent(input$isGptMotivational, {
        update_task_motivation()
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
