
# global.R: global variables and data handling ----------------------------
#' - functions for reading and ordering tasks
#' - loading prompts
#' - creating reactive values


library(shinyWidgets)
library(data.table)
library(shinyjs)
library(beepr)
library(profvis)
library(sortable)


# Initialize reactive values
rv <- reactiveValues(
    current_date = Sys.Date(),
    task_mode = FALSE,
    task_timer_active = FALSE,
    task_start_time = NULL,
    task_duration_seconds = 0,
    last_duration = NULL,
    edit_mode = FALSE,
    edit_id = NA,
    motivational_text = NULL,
    last_beep = Sys.time()
)


statistics_dt <- fread("statistics.csv")
statistics_dt[, Time := as.POSIXct(Time)]

chk_observers <- list()
sel_observers <- list()
atom_observers <- list()
edit_observers <- list()
del_observers <- list()

launch_time <- Sys.time()