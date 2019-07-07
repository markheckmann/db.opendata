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
wait_min <- 2
ii <- seq(1:(60/wait_min * 24))

for (i in ii) {

  x <- retrieve_fasta_data()
  log <- x %>% create_fasta_log()
  df <- x %>% clean_fasta_data
  append_log(log, file = "fasta_log.csv")
  append_data(df, file = "fasta_data.csv")

  nw <- now()
  cat("\nFaSta API Request: ", as.character(nw),
      "Status:", status_code(x$response))

  # wait until end of minute
  wait <- difftime(ceiling_date(nw, unit = "minute") + minutes(wait_min - 1),
                   nw, units = "secs")

  Sys.sleep(wait)

}




