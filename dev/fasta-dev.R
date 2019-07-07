#////////////////////////////////////////////////////////////////
#
#                      FaSta API
#
#////////////////////////////////////////////////////////////////


library(tidyverse)
library(lubridate)
library(httr)
library(db.opendata)

for (i in 1:(60 * 1)) {

  x <- retrieve_fasta_data()
  log <- x %>% create_fasta_log()
  df <- x %>% clean_fasta_data
  append_log(log, file = "dev/fasta_log.csv")
  append_data(df, file = "dev/fasta_data.csv")

  nw <- now()
  cat("\nFaSta API Request: ", as.character(nw), "Status:", status_code(x$response))

  # wait until end of minute
  wait <- ceiling_date(nw, unit = "minute") - nw

Sys.sleep(wait)

}




