# TimeClockR
#
# Author: Trevor Williams
#
# Description: This is a shiny app to keep track of time worked in a day.
#              It connects to a Postgres Database which stores the times
#              in and out.
#


library(shiny)
library(shinyjs)
library(DBI)
library(tidyverse)
library(hms)
library(readr)

# Query to pull and fetch data from database
run_query <- function(con, query) {
  res <- dbSendQuery(con, query)
  vals <- dbFetch(res)
  dbClearResult(res)
  return(vals)
}

# Read in files with username and password
# NOTE: These files will need to be added, the format is a single line file
# with the username for user.config and the password for password.config

user = read_lines('user.config')
pw = read_lines('password.config')


# Set up connection to DB
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost',
                 port = '5432',
                 dbname = 'timeclock',
                 user = user,
                 password = pw,
                 timezone = 'America/New_York')


# Define UI

ui <- fluidPage(h3('TimeClockR'),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
                 actionButton('clockin', 'Clock-in'),
                 actionButton('clockout', 'Clock-out'),
                 hr(),
                 tags$b('Total Time Worked:'),
                 textOutput('print_time_worked')
    ),
    mainPanel(
              DT::dataTableOutput('time_df'),
    )
  )

)

# Define server logic
server <- function(input, output, session) {

  db_vals <- reactiveValues() # Initiate list of reactive values to store times in and out and ids

  # Pull times in, out, and time_in ids for the current date
  db_vals$in_times <- run_query(con, 'SELECT "time" FROM time_in WHERE "time"::date = CURRENT_DATE;')
  db_vals$out_times <- run_query(con, 'SELECT "time" FROM time_out WHERE "time"::date = CURRENT_DATE;')
  db_vals$in_ids <- run_query(con, 'SELECT id FROM time_in WHERE "time"::date = CURRENT_DATE;')

  # Observer to enable/disable clock-in and clock-out buttons to ensure that
  # clock-out can only be pressed after clock-in and clock-in can only be pressed
  # if there are the same number of clock-ins and clock-outs
  observe({

    if(nrow(db_vals$in_times) > nrow(db_vals$out_times)) {
      disable("clockin")
      enable('clockout')
    } else {
      enable('clockin')
      disable('clockout')
    }

  })

  # Reactive to pull the data from the DB and store it as a dataframe
  all_data <- reactive({

    if(nrow(db_vals$in_times) != nrow(db_vals$out_times)) {
      out <- data.frame(in_time = db_vals$in_times$time, out_time = c(db_vals$out_times$time, NA))
    } else {
      out <- data.frame(in_time = db_vals$in_times$time, out_time = db_vals$out_times$time)
    }

    out$time_worked <- as_hms(out$out_time - out$in_time)
    out

  })


  # Reactive to list the total time worked
  total_time_worked <- reactive({
    df <- all_data() # pull current data from DB

    if(nrow(db_vals$in_times) != nrow(db_vals$out_times)) { # Check if clocked-in, if yes:
      invalidateLater(1000) # Set up invalidation so that the time worked continuously updates
      time_calc <- sum(c(sum(df$time_worked, na.rm = TRUE),
                         Sys.time() - db_vals$in_times$time[nrow(db_vals$in_times)]))
    } else { # If clocked-out
      time_calc <- sum(df$time_worked) # Just output the total time worked
    }
    return(time_calc)

  })

  # Logic outlining what to do when the clock-in button is pressed
  observeEvent(input$clockin, {

    # Insert the new clock-in time to the DB
    query_time_in <- paste0('INSERT into time_in (time, queue) VALUES (now(), ', nrow(db_vals$in_times)+1, ');')
    res <- dbSendQuery(con, query_time_in)
    dbClearResult(res)
    # Update the reactive values relating to time-ins and time_in ids
    db_vals$in_times <- run_query(con, 'SELECT "time" FROM time_in WHERE "time"::date = CURRENT_DATE;')
    db_vals$in_ids <- run_query(con, 'SELECT id FROM time_in WHERE "time"::date = CURRENT_DATE;')

  })

  # Logic outlining what to do when the clock-out button is pressed
  observeEvent(input$clockout, {

    # Insert the new clock-out time into the DB
    query_time_out <- paste0("INSERT into time_out (time, queue, time_in_id) VALUES (now(), ",
                             nrow(db_vals$in_times), ", '",
                             db_vals$in_ids[nrow(db_vals$in_times), 'id'], "');")
    res <- dbSendQuery(con, query_time_out)
    dbClearResult(res)
    # Update the reactive value storing the times out
    db_vals$out_times <- run_query(con, 'SELECT "time" FROM time_out WHERE "time"::date = CURRENT_DATE;')
  })

  # Set up outputs
  output$time_df <-  DT::renderDataTable({ DT::datatable(data = all_data()) %>% DT::formatDate(1:2, "toLocaleString") })
  output$print_time_worked <- renderPrint({
    as_hms(total_time_worked())
    })


}

# Run the application
shinyApp(ui = ui, server = server)
