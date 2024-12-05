.gfd_request <- function(series, ...) {
  url <- "https://api.globalfinancialdata.com/API/API.ashx?username=&password=&filename=&series_id=55970&include=send%2Csbegin&splitadjusted=true&closeonly=false&inflationadjusted=false&annualizedflow=false&totalreturn=false&corporate_actions=false&IncludeSeriesId=false&average=false&periodpercentchange=false&periodicity=Annual&type=csv"
  args <- list(...)
  replacers <- purrr::imap(args, \(arg_value, arg_name) {
    # mainly true/false
    if (is.logical(arg_value)) arg_value <- str_to_lower(arg_value)
    
    arg_value <- arg_value |> # for API call
      stringr::str_replace_all("@", "%40") |> 
      stringr::str_replace_all(" ", "%20")

    list(
      pattern = paste0(arg_name, "=.*?&"),
      replacement = paste0(arg_name, "=", arg_value, "&")
    )
  })


  username <- Sys.getenv("GFD_USER") |> 
    stringr::str_replace_all("@", "%40")
  if (username == "") {
    cli::cli_alert_danger("GFD_USER not found in your environment!")
    usethis::edit_r_environ(scope = "user")
  } 

  password <- Sys.getenv("GFD_PWD") |> 
    stringr::str_replace_all("@", "%40") |> 
    stringr::str_replace_all(" ", "%20")
  if (password == "") {
    cli::cli_alert_danger("GFD_PWD not found in your environment!")
    usethis::edit_r_environ(scope = "user")
  }

  url <- url |> 
    stringr::str_replace_all("username=.*?&", stringr::str_c("username=", username, "&")) |>
    stringr::str_replace_all("password=.*?&", stringr::str_c("password=", password, "&")) |> 
    stringr::str_replace_all("filename=", stringr::str_c("filename=", series))

  for (replacer in replacers) {
    url <- url |> 
      stringr::str_replace_all(replacer$pattern, replacer$replacement)
  }

  url
}

.gfd_fetch <- memoise::memoise(function(url, skip = 2) {
  url |> 
    readr::read_csv(skip = skip) |> 
    dplyr::mutate(
      dplyr::across(dplyr::any_of("Date"), lubridate::mdy),
    ) |> 
    janitor::clean_names()
})

#' Downloads data from GFD API.
#' 
#' The function uses the GFD API. You need to set the environment variable `GFD_USER` and `GFD_PWD` with your username and password.
#'
#' @param series A string with the series ID.
#' @param ... Additional arguments to be passed to the API. (i.e.: periodicity = "Monthly")
#'
#' @return A data.frame with the downloaded data.
#'
#' @examples
#' gfd_data("_SPXTRD")
#' 
#' @export
#'

gfd_data <- function(series, ..., skip_rows = 2) {
  request_url <- .gfd_request(series, ...)
  suppressMessages(.gfd_fetch(request_url, skip = skip_rows))
}