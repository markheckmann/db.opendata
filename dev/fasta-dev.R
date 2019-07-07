#////////////////////////////////////////////////////////////////
#
#                      FaSta API
#
#////////////////////////////////////////////////////////////////


library(tidyverse)
library(lubridate)
library(httr)
library(db.opendata)
library(data.table)


# each x minutes
interval_in_min <- 2
ii <- seq(1:(60/wait_min * 24))

for (i in ii) {

  x <- retrieve_fasta_data()
  log <- x %>% create_fasta_log
  df <- x %>% clean_fasta_data
  append_log(log, file = "fasta_log.csv")
  append_data(df, file = "fasta_data.csv")
  append_data(df, file = "fasta_data_cleaned.csv")

  # clean data and keep change of state only
  dt <- fread("fasta_data_cleaned.csv")
  dt <- keep_state_changes(dt)
  fwrite(dt, "fasta_data_cleaned.csv")

  nw <- now()
  cat("\nFaSta API Request: ", as.character(nw),
      "Status:", status_code(x$response))

  # determine waiting time until next request
  wait <- difftime(ceiling_date(nw, unit = "minute") + minutes(interval_in_min - 1),
                   nw, units = "secs")
  Sys.sleep(wait)

}


# clean data to keep only timestamps where status changes
x <- fread("dev/fasta_data.csv")
x2 <- keep_state_changes(x)  # can be called again and again

# number of state change in dataset
x2[, .N, by = c("equipmentnumber")][order(-N)]
x2[, .(state_changes = .N), by = c("equipmentnumber")][, .N, by = "state_changes"]
















