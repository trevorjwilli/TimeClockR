# TimeClockR

This repository contains an R shiny app that can be used to log time worked. It contains clock-in and clock-out buttons that when pressed
query a Postgres database to insert times in and out. 

## Necessary R libraries

This shiny app requires the following R dependencies:
- shiny
- shinyjs
- DBI
- tidyverse
- hms
- readr
- RPostgres
- DT

## Additional Requirements

The app also requires two text files each with a single line:
- user.config - containing the username for the Postgres user
- password.config - containing the password for the Postgres user
