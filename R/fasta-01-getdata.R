#////////////////////////////////////////////////////////////////
#
#                      FaSta API
#
#////////////////////////////////////////////////////////////////



#' Query FaSta API
#'
#' @param path Path to FaSta API.
#' @export
#' @family FaSta
#'
retrieve_fasta_data <- function(path = "fasta/v2/facilities", key = NA)
{
  # API setup
  url <- modify_url("https://api.deutschebahn.com", path = path)

  if (is.na(key)) {
    key <- keyring::key_get("DB_FASTA_API_KEY")  # get token stored in keyring
  }

  headers = add_headers(Authorization = paste("Bearer", key, sep = " "))
  resp <- GET(url, config = headers)

  # check if JSON was returned
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse of status is 200 = success
  status <- status_code(resp)
  if (status == 200) {
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  } else {
    parsed <- NA
  }

  # return response plus parsed S3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "fasta_api"
  )

}


#' Print method for FaSta response
#'
#' @export
#' @keywords internal
#'
print.fasta_api <- function(x, ...)
{
  cat("<FaSta API  ", x$path, ">\n", sep = "")
  cat("->", length(x$content), "data points in", x$response$times["total"], "seconds\n")
  print(x$response)
  invisible(x)
}


#' Clean response data from FaSta API
#'
#' @param x Return object from \code{retrieve_fasta_data}.
#' @export
#' @family FaSta
#'
clean_fasta_data <- function(x)
{
  if (status_code(x$response) != 200)
    return(NULL)

  l <- x$content

  flat <- lapply(l, function(l) {
    l %>% unlist %>% t %>% as.data.table
  })
  df <- dplyr::bind_rows(flat)

  df$datetime <- lubridate::with_tz(x$response$date, "CET") # save as German time
  df
}


#' Create log data from FaSta API response
#'
#' @param x Return object from \code{retrieve_fasta_data}.
#' @export
#' @family FaSta
#'
create_fasta_log <- function(x)
{
  data.frame(
    path = x$path
    ,datetime = lubridate::with_tz(x$response$date, "CET") # save as German time
    ,status = x$response$status_code
    ,duration = x$response$times[["total"]]
    ,datapoints = length(x$content)
    , row.names = NULL)
}


#' Save FaSta data
#'
#' @param x Dataframe with data
#' @export
#'
append_data <- function(x, file = "fasta_master.csv")
{
  if (is.null(x) || nrow(x) < 10)
    return(NULL)

  fwrite(x, file, append = TRUE)
}


#' Save FaSta log info
#'
#' @param x Dataframe with log info
#' @export
#'
append_log <- function(x, file = "fasta_log.csv")
{
  fwrite(x, file, append = TRUE)
}



# encode each changed value with new running index
encode_changes <- function(x)
{
  r <- rle(x)
  rep(seq_along(r$values), r$lengths)
}


#' Keep data poits where state changes occur
#'
#' @param  dt dataframe from \code{clean_fasta_data}.
#' @export
#
keep_state_changes <- function(dt, change_index = FALSE)
{
  setDT(dt)
  dt <- dt[order(equipmentnumber, datetime)]                      # sort by equi and time
  dt[, change_index := encode_changes(state), "equipmentnumber"]  # add change index
  dt <- dt[, .SD[1], by = c("equipmentnumber", "change_index")]   # select first value after change

  # delete change index if not prompted
  if (!change_index) {
    dt$change_index <- NULL
  }
  dt
}




