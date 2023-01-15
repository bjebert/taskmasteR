library(shiny)
library(data.table)
library(gfonts)

ui <- fluidPage(
    tags$style(
        HTML(
            'body {
                 background-color: #388E3C;
             }
             h2 {
                 color: #F5FFFA;
                 font-size: 32px;
             }
             .rightAlign {
                float: right;
             }
             '
        )
    ),
    
    gfonts::use_font("catamaran", "fonts/css/catamaran.css"),
    
    # App title ----
    titlePanel("taskmasteR"),
    
    absolutePanel(
        width = "600px",
        wellPanel(
            style = "background-color: #F5FFFA",
            htmlOutput("tasks")
        )
    ),
    
    absolutePanel(
        top = "0px", left = "800px",
        h2(textOutput("dateTime"))
    )
)
