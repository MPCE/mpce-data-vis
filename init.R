###########################################################################
#
# MAPPING PRINT, CHARTING ENLIGHTENMENT
#
# DATA VISUALISATION EXPERIMENTS
#
# Script: init
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(DBI)
library(RMySQL)
library(lubridate)


manuscripts <- dbConnect(MySQL(), user="root", dbname="manuscripts", host="127.0.0.1")
# Ensure correct charset
manuscripts %>%
  dbSendQuery("SET NAMES utf8") %>%
  dbClearResult()


fetch_table <- function(con, tbl_name) {
  # Helper function that selects an entire table.
  #
  # Params:
  #   con (connection): the databse where the table is
  #   tbl_name (str): the name of the table in the database
  #
  # Returns:
  #   out (tbl): a tibble of the data
  
  query <- paste0("SELECT * FROM ", tbl_name)
  
  out <- con %>%
    dbSendQuery(query) %>%
    fetch(n = Inf) %>%
    as_tibble()
  
  return(out)
}
