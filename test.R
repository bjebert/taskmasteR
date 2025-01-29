library(shiny)
library(data.table)
library(gfonts)
library(shinyTime)
library(shinyWidgets)
library(shinyjs)

breaks <- function(n) tagList(lapply(1:n, function(x) br()))

ui <- fluidPage(
    shinyjs::useShinyjs(),
    absolutePanel(
        width = "600px", top = "50px",
        wellPanel(
            htmlOutput("taskUi"),
            breaks(48)
        )
    ),
    
    absolutePanel(
        top = "56px", left = "650px",
        actionButton("enhance", label = "Enhance", width = "114px"),
    )
)


get_tasks_ui <- function(txt) {
    if(length(txt) == 0) {
        return()
    }
    
    create_ui_row <- function(top, id, x) {
        fluidRow(
            absolutePanel(left = "20px", top = sprintf("%dpx", top), x),
            absolutePanel(left = "550px", top = sprintf("%dpx", top), actionButton(sprintf("del%d", id), "", icon = icon("trash"))),
        )
    }
    
    tasks_ui <- lapply(1:length(txt), function(i) {
        create_ui_row(i*40 - 25, i, txt[i])
    })
    
    return(tasks_ui)
}


delete <- function(rv, i) {
    print(sprintf('deleting task %d', i))
    rv[["text"]] <- rv[["text"]][-i]
}


server <- function(input, output, session) {
    rv <- reactiveValues(text = NULL)
    observers <- list()
    
    output$taskUi <- renderUI({
        ui <- get_tasks_ui(rv[["text"]])
        # N <- length(ui)
        # 
        # if("children" %in% names(ui)) {  # Single absolutePanel with no tasks label
        #     N <- 0
        # }
        # 
        # print(ui)
        # 
        # if(N > length(observers)) {
        #     i <- length(observers) + 1
        #     print(sprintf('creating observer %d', i))
        #     observers <<- c(observers, lapply(i:N, function(i) observeEvent(input[[sprintf("del%d", i)]], {
        #         delete(rv, i)
        #     }, once = TRUE)))
        #     
        # } else if(N < length(observers)) {
        #     for(i in length(observers):(N+1)) {
        #         print(sprintf('destroying observer %d', i))
        #         observers[[i]]$destroy()
        #         observers[[i]] <<- NULL
        #     }
        # }
        
        return(ui) 
    })
    
    observe({
        # Check all delete buttons
        browser()obser
        isolate({
            lapply(seq_along(rv$text), function(i) {
                del_id <- sprintf("del%d", i)
                if (!is.null(input[[del_id]]) && input[[del_id]] > 0) {
                    delete(rv, i)
                    
                    # Reset the input value to prevent re-triggering
                    session$sendCustomMessage(type = "resetInput", message = list(id = del_id))
                }
            })
        })
    })
    
    observeEvent(input$enhance, {
        print(rv[["text"]])
        if(is.null(rv[["text"]]) || length(rv[["text"]]) == 0) {
            rv[["text"]] <- 1
        } else {
            rv[["text"]] <- c(rv[["text"]], max(rv[["text"]]) + 1)
        }
    })
}


shinyApp(ui, server)