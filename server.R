library(shinyWidgets)
library(data.table)

server <- function(input, output, session) {
    output$dateTime <- renderText({
        invalidateLater(1, session)
        format(Sys.time(), format = "%A, %d %B %Y, %H:%M:%S")
    })
    
    output$tasks <- renderUI({
        return("No tasks have been created yet...")
    })
}
